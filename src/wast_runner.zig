const std = @import("std");
const dbg = std.debug.print;

const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;

pub fn readall(allocator: std.mem.Allocator, filename: []u8) ![]u8 {
    const fil = try std.fs.cwd().openFile(filename, .{});
    const stat = try std.posix.fstat(fil.handle);
    const size = std.math.cast(usize, stat.size) orelse return error.FileTooBig;
    const buf = try allocator.alloc(u8, size);
    if (try fil.readAll(buf) < size) {
        return error.IOError;
    }
    return buf;
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const argv = std.os.argv;
    if (argv.len < 2) {
        dbg("wast_runner {{test_file.wast}}\n", .{});
        return 1;
    }
    const filearg = std.mem.span(argv[1]);
    const buf = try readall(allocator, filearg);
    defer allocator.free(buf);

    var t: Tokenizer = .{ .str = buf };
    errdefer t.fail_pos();

    _ = try t.expect(.LeftParen);
    try t.expectAtom("module");

    try t.skip(1);

    const mod_source = buf[0..t.pos];
    const mod_code = try wat2wasm(mod_source, allocator);

    var mod = try wasm_shelf.Module.parse(mod_code, allocator);
    defer mod.deinit();

    var cases: u32 = 0;
    var failures: u32 = 0;

    var params: std.ArrayList(StackValue) = .init(allocator);

    const AssertKind = enum { assert_return, assert_trap };
    const ConstKind = enum { @"i32.const", @"i64.const" };

    while (t.nonws()) |_| {
        dbg("\rtest at {}:", .{t.lnum + 1});

        _ = try t.expect(.LeftParen);
        const kind = try t.expectAtomChoice(AssertKind);
        _ = try t.expect(.LeftParen);
        try t.expectAtom("invoke");
        const name_tok = try t.expect(.String);
        const name = try t.simple_string(name_tok);

        while (try t.expect_maybe(.LeftParen)) |_| {
            const typ = try t.expectAtomChoice(ConstKind);
            const param = try t.expect(.Atom);
            const value: StackValue = switch (typ) {
                .@"i32.const" => .{ .i32 = try t.int(i32, param) },
                .@"i64.const" => .{ .i64 = try t.int(i64, param) },
            };
            _ = try t.expect(.RightParen);
            try params.append(value);
        }
        _ = try t.expect(.RightParen);

        const sym = try mod.lookup_export(name) orelse
            return error.NotFound;

        if (sym.kind != .func) return error.Wattaf;

        cases += 1;

        switch (kind) {
            .assert_return => {
                const res = try mod.execute(sym.idx, params.items);

                if (try t.expect_maybe(.LeftParen)) |_| {
                    const typ = try t.expectAtomChoice(ConstKind);
                    const ret = try t.expect(.Atom);
                    _ = try t.expect(.RightParen);

                    switch (typ) {
                        .@"i32.const" => {
                            const num_ret = try t.int(i32, ret);
                            if (res.i32 != num_ret) {
                                dbg("{s}(...): actual: {}, expected: {}\n", .{ name, res.i32, num_ret });
                                failures += 1;
                            }
                        },
                        .@"i64.const" => {
                            const num_ret = try t.int(i64, ret);
                            if (res.i64 != num_ret) {
                                dbg("{s}(...): actual: {}, expected: {}\n", .{ name, res, num_ret });
                                failures += 1;
                            }
                        },
                    }
                }
                _ = try t.expect(.RightParen);
            },
            .assert_trap => {
                _ = try t.expect(.String);
                _ = try t.expect(.RightParen);
                if (mod.execute(sym.idx, params.items)) |res| {
                    dbg("{s}(...): expected trap but got: {}\n", .{ name, res });
                    failures += 1;
                } else |err| {
                    if (err == error.WASMTrap) {
                        // ok!
                    } else {
                        return err;
                    }
                }
            },
        }

        params.items.len = 0;
    }

    dbg("\r{} tests, {} ok, {} fail\n", .{ cases, cases - failures, failures });
    return if (failures > 0) 1 else 0;
}

const Tokenizer = struct {
    str: []const u8,
    pos: usize = 0,
    lnum: u32 = 0,
    lpos: usize = 0,
    // if non-null, pos is already at the end of `peeked_tok`
    peeked_tok: ?Token = null,

    const ParseError = error{ParseError};

    pub fn err_pos(self: *Tokenizer) struct { u32, u32 } {
        return .{ self.lnum + 1, @intCast(self.pos - self.lpos) };
    }

    pub fn fail_pos(self: *Tokenizer) void {
        const line, const col = self.err_pos();
        dbg("fail at {}:{}\n", .{ line + 1, col });
    }

    fn block_comment(self: *Tokenizer) void {
        var lvl: u32 = 1;
        self.pos += 2;
        var c1: u8 = 0;
        while (self.pos < self.str.len) : (self.pos += 1) {
            const c2 = self.str[self.pos];
            if (c1 == '(' and c2 == ';') {
                lvl += 1;
                c1 = 0;
            } else if (c1 == ';' and c2 == ')') {
                lvl -= 1;
                if (lvl == 0) {
                    self.pos += 1;
                    return;
                }
                c1 = 0;
            } else {
                c1 = c2;
            }
        }
    }

    pub fn nonws(self: *Tokenizer) ?u8 {
        while (self.pos < self.str.len) : (self.pos += 1) {
            switch (self.str[self.pos]) {
                ' ', '\t', '\r' => continue,
                '\n' => {
                    self.lnum += 1;
                    self.lpos = self.pos;
                    continue;
                },
                ';' => {
                    if (self.pos + 1 < self.str.len and self.str[self.pos + 1] == ';') {
                        while (self.pos < self.str.len and self.str[self.pos] != '\n') {
                            self.pos += 1;
                        }
                        if (self.pos < self.str.len) self.pos -= 1; // use \n above
                        continue;
                    }
                    return ';';
                },
                '(' => {
                    if (self.pos + 1 < self.str.len and self.str[self.pos + 1] == ';') {
                        self.block_comment();
                        continue;
                    }
                    return '(';
                },
                else => |c| return c,
            }
        }
        return null;
    }

    pub fn idlike(self: *Tokenizer) bool {
        if (self.pos >= self.str.len) return false;
        const char = self.str[self.pos];
        if (char <= 32) return false;
        if (char == '"' or char == ';' or char == '(' or char == ')') return false;
        return true;
    }

    const TokenKind = enum {
        LeftParen,
        RightParen,
        Atom,
        String,
    };
    const Token = struct {
        kind: TokenKind,
        pos: usize,
        len: usize,
    };

    fn next_inner(self: *Tokenizer) !?Token {
        const t = self.nonws() orelse return null;
        const start = self.pos;

        const kind: TokenKind = valid: {
            switch (t) {
                '(' => {
                    self.pos += 1;
                    break :valid .LeftParen;
                },
                ')' => {
                    self.pos += 1;
                    break :valid .RightParen;
                },
                '"' => {
                    try self.string();
                    break :valid .String;
                },
                // nonws should already skipped valid comments
                ';' => return error.ParseError,
                else => {
                    if (!self.idlike()) return error.ParseError;
                    self.pos += 1;
                    while (self.idlike()) self.pos += 1;
                    break :valid .Atom;
                },
            }
        };
        return .{ .kind = kind, .pos = start, .len = self.pos - start };
    }

    fn next(self: *Tokenizer) !?Token {
        if (self.peeked_tok) |tok| {
            self.peeked_tok = null;
            return tok;
        }
        return self.next_inner();
    }

    fn peek(self: *Tokenizer) !?Token {
        if (self.peeked_tok) |tok| {
            return tok;
        }
        self.peeked_tok = try self.next_inner();
        return self.peeked_tok;
    }

    pub fn skip(self: *Tokenizer, levels: u32) !void {
        var level: u32 = levels;

        while (try self.next()) |tok| {
            // dbg("{},{}: {s} {}\n", .{ self.lnum + 1, tok.pos - self.lpos, @tagName(tok.kind), tok.len });
            switch (tok.kind) {
                .LeftParen => level += 1,
                .RightParen => level -= 1,
                else => continue,
            }
            if (level == 0) break;
        }
    }

    fn string(self: *Tokenizer) !void {
        self.pos += 1; // first "
        while (self.pos < self.str.len) : (self.pos += 1) {
            switch (self.str[self.pos]) {
                '\\' => self.pos += 1,
                '"' => {
                    self.pos += 1;
                    return;
                },
                else => continue,
            }
        }
        return error.ParseError;
    }

    pub fn expect(self: *Tokenizer, t: TokenKind) !Token {
        const tok = try self.next() orelse return error.ParseError;
        if (tok.kind != t) return error.ParseError;
        return tok;
    }

    pub fn expect_maybe(self: *Tokenizer, t: TokenKind) !?Token {
        const tok = try self.peek() orelse return error.ParseError;
        if (tok.kind != t) return null;
        self.peeked_tok = null;
        return tok;
    }

    pub fn expectAtom(self: *Tokenizer, atom: []const u8) !void {
        const tok = try self.expect(.Atom);
        if (!std.mem.eql(u8, self.rawtext(tok), atom)) {
            return error.ParseError;
        }
    }

    pub fn expectAtomChoice(self: *Tokenizer, comptime Choices: type) !Choices {
        const tok = try self.expect(.Atom);
        return std.meta.stringToEnum(Choices, self.rawtext(tok)) orelse error.ParseError;
    }

    fn rawtext(self: *Tokenizer, t: Token) []const u8 {
        return self.str[t.pos..][0..t.len];
    }

    // dummy hack, give us any string value which can be read without allocation
    fn simple_string(self: *Tokenizer, t: Token) ![]const u8 {
        const text = self.rawtext(t);
        if (text.len < 2 or text[0] != '"' or text[text.len - 1] != '"') return error.FormatError;
        if (std.mem.indexOfScalar(u8, text, '\\')) |_| return error.NotImplemented;
        return text[1 .. text.len - 1];
    }

    fn int(self: *Tokenizer, ityp: type, t: Token) !ityp {
        const utyp = if (ityp == i32) u32 else if (ityp == i64) u64 else unreachable;
        const text = self.rawtext(t);
        if (text.len >= 2 and text[0] == '0' and (text[1] == 'x' or text[1] == 'X')) {
            return @bitCast(try std.fmt.parseInt(utyp, text[2..], 16));
        } else if (text.len >= 3 and text[0] == '-' and text[1] == '0' and (text[2] == 'x' or text[2] == 'X')) {
            return -@as(ityp, @bitCast(try std.fmt.parseInt(utyp, text[3..], 16)));
        }

        return try std.fmt.parseInt(ityp, text, 10);
    }
};

fn wat2wasm(source: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const argv = &[_][]const u8{ "wat2wasm", "-", "--output=-" };

    const Child = std.process.Child;
    var child: Child = .init(argv, allocator);

    child.stdout_behavior = Child.StdIo.Pipe;
    child.stdin_behavior = Child.StdIo.Pipe;
    child.stderr_behavior = Child.StdIo.Inherit;
    try child.spawn();
    try child.stdin.?.writeAll(source);
    child.stdin.?.close();
    child.stdin = null; // dumma
    const out = child.stdout.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    switch (try child.wait()) {
        .Exited => |res| {
            if (res == 0) return out;
        },
        else => {},
    }
    return error.ProcessError;
}
