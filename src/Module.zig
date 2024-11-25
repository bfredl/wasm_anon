const std = @import("std");
const testing = std.testing;
const dbg = std.debug.print;
const Reader = std.io.FixedBufferStream([]const u8).Reader;

const defs = @import("./defs.zig");
const Module = @This();

fn readLeb(r: Reader, comptime T: type) !T {
    return switch (@typeInfo(T).int.signedness) {
        .signed => std.leb.readILEB128(T, r),
        .unsigned => std.leb.readULEB128(T, r),
    };
}

fn readu(r: Reader) !u32 {
    return readLeb(r, u32);
}

fn readName(r: Reader) ![]const u8 {
    const len = try readu(r);
    const str = r.context.buffer[r.context.pos..][0..len];
    r.context.pos += len;
    return str;
}

fn readLimits(r: Reader) !struct { min: u32, max: ?u32 } {
    const kind = try r.readByte();
    const min = try readu(r);
    return .{ .min = min, .max = switch (kind) {
        0x00 => null,
        0x01 => try readu(r),
        else => return error.InvalidFormat,
    } };
}

allocator: std.mem.Allocator,
raw: []const u8,
funcs: []FuncDef = undefined,

const FuncDef = struct {
    typeidx: u32,
    codeoff: u32,
};

pub fn parse(module: []const u8, allocator: std.mem.Allocator) !Module {
    if (module.len < 8) return error.InvalidFormat;
    if (!std.mem.eql(u8, module[0..8], &.{ 0, 'a', 's', 'm', 1, 0, 0, 0 })) return error.InvalidFormat;

    var self: Module = .{ .raw = module, .allocator = allocator };

    var fbs = std.io.FixedBufferStream([]const u8){ .pos = 8, .buffer = module };
    const r = fbs.reader();

    while (true) {
        const id = r.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        const kind: defs.SectionKind = @enumFromInt(id);

        const len = try readu(r);
        dbg("SECTION: {s} ({}) with len {}\n", .{ @tagName(kind), id, len });
        const end_pos = fbs.pos + len;
        switch (kind) {
            .type => try type_section(r),
            .function => try self.function_section(r),
            .memory => try memory_section(r),
            .global => try global_section(r),
            .import => try import_section(r),
            .export_ => try export_section(r),
            .code => try self.code_section(r),
            .table => try table_section(r),
            else => {}, // try r.skipBytes(len, .{})
        }

        // TODO: this should be strict, but we are just fucking around and finding out for now
        if (fbs.pos > end_pos) {
            return error.InvalidFormat;
        }
        fbs.pos = end_pos;
    }

    return self;
}

pub fn deinit(self: *Module) void {
    self.allocator.free(self.funcs);
}

pub fn type_section(r: Reader) !void {
    const len = try readu(r);
    dbg("TYPES: {}\n", .{len});
    for (0..len) |_| {
        const tag = try r.readByte();
        if (tag != 0x60) return error.InvalidFormat;
        const n_params = try readu(r);
        dbg("(", .{});
        for (0..n_params) |_| {
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            dbg("{s}, ", .{@tagName(typ)});
        }
        dbg("): (", .{});
        const n_ret = try readu(r);
        for (0..n_ret) |_| {
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            dbg("{s}, ", .{@tagName(typ)});
        }
        dbg(")\n", .{});
    }
}

pub fn import_section(r: Reader) !void {
    const len = try readu(r);
    dbg("IMPORTS: {}\n", .{len});
    for (0..len) |_| {
        const mod = try readName(r);
        const name = try readName(r);
        dbg("{s}:{s} = ", .{ mod, name });
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        switch (kind) {
            .func => {
                const idx = try readu(r);
                dbg("func {}\n", .{idx});
            },
            .table => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const limits = try readLimits(r);
                dbg("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
            },
            .mem => {
                const limits = try readLimits(r);
                dbg("mem w {}:{?}\n", .{ limits.min, limits.max });
            },
            .global => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const mut = (try r.readByte()) > 0;
                dbg("global {} {}\n", .{ typ, mut });
            },
        }
    }
}

pub fn export_section(r: Reader) !void {
    const len = try readu(r);
    dbg("EXPORTS: {}\n", .{len});
    for (0..len) |_| {
        const name = try readName(r);
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try readu(r);
        dbg("{s} = {s} {}\n", .{ name, @tagName(kind), idx });
    }
}

pub fn function_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    self.funcs = try self.allocator.alloc(FuncDef, len);
    dbg("FUNCS: {}\n", .{len});
    for (0..len) |i| {
        const idx = try readu(r);
        self.funcs[i].typeidx = idx;
    }
    dbg("...\n", .{});
}

pub fn memory_section(r: Reader) !void {
    const len = try readu(r);
    dbg("MEMORYS: {}\n", .{len});
    for (0..len) |_| {
        const lim = try readLimits(r);
        dbg("mem {}:{?}\n", .{ lim.min, lim.max });
    }
}

pub fn global_section(r: Reader) !void {
    const len = try readu(r);
    dbg("GLOBALS: {}\n", .{len});
    dbg("tbd...\n", .{});
}

pub fn table_section(r: Reader) !void {
    const len = try readu(r);
    dbg("Tables: {}\n", .{len});
    for (0..len) |_| {
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        const limits = try readLimits(r);
        dbg("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
    }
}

pub fn code_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    dbg("Codes: {}\n", .{len});
    for (0..len) |i| {
        const size = try readu(r);
        dbg("CODE with size {}\n", .{size});
        const endpos = r.context.pos + size;

        self.funcs[i].codeoff = @intCast(r.context.pos);

        const n_locals = try readu(r);
        for (0..n_locals) |_| {
            const n_decl = try readu(r);
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            dbg("{} x {s}, ", .{ n_decl, @tagName(typ) });
        }
        dbg("\n", .{});
        try expr(r);

        r.context.pos = endpos;
        dbg("\n", .{});
    }
}

pub fn peekByte(r: Reader) u8 {
    return r.context.buffer[r.context.pos];
}

pub fn blocktype(r: Reader) !void {
    // TODO: just readLeb(r, i33) directly and "interpret" negative values might be simpler?
    const nextByte = peekByte(r);
    if ((nextByte & 0xc0) == 0x40) {
        const t: defs.ValType = @enumFromInt(try r.readByte());
        dbg(" typ={s}", .{@tagName(t)});
    } else {
        const tidx: u32 = @intCast(try readLeb(r, i33));
        dbg(" typid={}", .{tidx});
    }
}

pub fn expr(r: Reader) !void {
    var level: u32 = 1;

    while (level >= 1) {
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        if (inst == .end or inst == .else_) level -= 1;

        for (0..level) |_| dbg("  ", .{});
        dbg("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                level += 1;
                try blocktype(r);
            },
            .end, .ret => {},
            .else_ => {
                level += 1;
            },
            .br, .br_if => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                dbg(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
            },
            .drop, .select => {},
            .prefixed => {
                const code: defs.Prefixed = @enumFromInt(try readu(r));
                dbg(":{s}", .{@tagName(code)});
                switch (code) {
                    .memory_fill => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    .memory_copy => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    else => {
                        dbg(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
                        return;
                    },
                }
            },
            else => {
                const idx = @intFromEnum(inst);
                if (idx >= 0x45 and idx <= 0xc4) {
                    // ok, parameterless
                } else if (idx >= 0x28 and idx <= 0x3e) {
                    const alignas = try readu(r);
                    const offset = try readu(r);
                    dbg(" a={} o={}", .{ alignas, offset });
                } else {
                    dbg(" TBD, aborting!\n", .{});
                    return;
                }
            },
        }
        dbg("\n", .{});
    }
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
