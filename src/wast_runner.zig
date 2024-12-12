const std = @import("std");
const dbg = std.debug.print;

const wasm_shelf = @import("wasm_shelf");

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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const argv = std.os.argv;
    if (argv.len < 2) return dbg("wast_runner {{test_file.wast}}\n", .{});
    const filearg = std.mem.span(argv[1]);
    const buf = try readall(allocator, filearg);
    defer allocator.free(buf);

    var t: Tokenizer = .{ .str = buf };
    errdefer t.fail_pos();

    _ = try t.expect(.LeftParen);
    try t.expectAtom("module");

    var level: u32 = 1;

    while (try t.next()) |tok| {
        // dbg("{},{}: {s} {}\n", .{ t.lnum + 1, tok.pos - t.lpos, @tagName(tok.kind), tok.len });
        switch (tok.kind) {
            .LeftParen => level += 1,
            .RightParen => level -= 1,
            else => continue,
        }
        if (level == 0) break;
    }

    while (t.nonws()) |_| {
        _ = try t.expect(.LeftParen);
        try t.expectAtom("assert_return");
        _ = try t.expect(.LeftParen);
        try t.expectAtom("invoke");
        const name = try t.expect(.String);
        dbg("raw: {s}\n", .{t.rawtext(name)});
        _ = try t.expect(.LeftParen);
        try t.expectAtom("i32.const");
        const param = try t.expect(.Atom);
        dbg("raw param: {s}\n", .{t.rawtext(param)});
        _ = try t.expect(.RightParen);
        _ = try t.expect(.RightParen);
        _ = try t.expect(.LeftParen);
        try t.expectAtom("i32.const");
        const ret = try t.expect(.Atom);
        dbg("raw ret: {s}\n", .{t.rawtext(ret)});
        _ = try t.expect(.RightParen);
        _ = try t.expect(.RightParen);
    }

    while (try t.next()) |tok| {
        dbg("{},{}: {s} {}\n", .{ t.lnum + 1, tok.pos - t.lpos, @tagName(tok.kind), tok.len });
    }
}

const Tokenizer = struct {
    str: []const u8,
    pos: usize = 0,
    lnum: u32 = 0,
    lpos: usize = 0,

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

    pub fn next(self: *Tokenizer) !?Token {
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

    pub fn expectAtom(self: *Tokenizer, atom: []const u8) !void {
        const tok = try self.expect(.Atom);
        if (!std.mem.eql(u8, self.rawtext(tok), atom)) {
            return error.ParseError;
        }
    }

    fn rawtext(self: *Tokenizer, t: Token) []const u8 {
        return self.str[t.pos..][0..t.len];
    }
};
