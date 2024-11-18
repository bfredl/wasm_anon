const std = @import("std");
const testing = std.testing;
const dbg = std.debug.print;
const Reader = std.io.FixedBufferStream([]const u8).Reader;

const instructions = @import("./wasm_instructions.zig");
const OpCode = instructions.OpCode;

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

const SectionKind = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    export_ = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_count = 12,
    _,
};

const ValType = enum(u8) {
    int32 = 0x7F,
    int64 = 0x7E,
    float32 = 0x7D,
    float64 = 0x7C,
    vec128 = 0x7B,
    _,
};

const ImportExportKind = enum(u8) {
    func,
    table,
    mem,
    global,
};

pub fn parse(module: []const u8) !void {
    if (module.len < 8) return error.InvalidFormat;
    if (!std.mem.eql(u8, module[0..8], &.{ 0, 'a', 's', 'm', 1, 0, 0, 0 })) return error.InvalidFormat;

    var fbs = std.io.FixedBufferStream([]const u8){ .pos = 8, .buffer = module };
    const r = fbs.reader();

    while (true) {
        const id = r.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        const kind: SectionKind = @enumFromInt(id);

        const len = try readu(r);
        dbg("SECTION: {s} ({}) with len {}\n", .{ @tagName(kind), id, len });
        const end_pos = fbs.pos + len;
        switch (kind) {
            .type => try type_section(r),
            .function => try function_section(r),
            .memory => try memory_section(r),
            .global => try global_section(r),
            .import => try import_section(r),
            .export_ => try export_section(r),
            .code => try code_section(r),
            else => {}, // try r.skipBytes(len, .{})
        }

        // TODO: this should be strict, but we are just fucking around and finding out for now
        if (fbs.pos > end_pos) {
            return error.InvalidFormat;
        }
        fbs.pos = end_pos;
    }
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
            const typ: ValType = @enumFromInt(try r.readByte());
            dbg("{s}, ", .{@tagName(typ)});
        }
        dbg("): (", .{});
        const n_ret = try readu(r);
        for (0..n_ret) |_| {
            const typ: ValType = @enumFromInt(try r.readByte());
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
        const kind: ImportExportKind = @enumFromInt(try r.readByte());
        switch (kind) {
            .func => {
                const idx = try readu(r);
                dbg("func {}\n", .{idx});
            },
            .table => {
                const typ: ValType = @enumFromInt(try r.readByte());
                const limits = try readLimits(r);
                dbg("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
            },
            .mem => {
                const limits = try readLimits(r);
                dbg("mem w {}:{?}\n", .{ limits.min, limits.max });
            },
            .global => {
                const typ: ValType = @enumFromInt(try r.readByte());
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
        const kind: ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try readu(r);
        dbg("{s} = {s} {}\n", .{ name, @tagName(kind), idx });
    }
}

pub fn function_section(r: Reader) !void {
    const len = try readu(r);
    dbg("FUNCS: {}\n", .{len});
    for (0..len) |i| {
        const idx = try readu(r);
        if (i < 10) {
            dbg("func {}\n", .{idx});
        }
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

pub fn code_section(r: Reader) !void {
    const len = try readu(r);
    dbg("Codes: {}\n", .{len});
    for (0..len) |_| {
        const size = try readu(r);
        dbg("CODE with size {}\n", .{size});
        const endpos = r.context.pos + size;

        const n_locals = try readu(r);
        for (0..n_locals) |_| {
            const n_decl = try readu(r);
            const typ: ValType = @enumFromInt(try r.readByte());
            dbg("{} x {s}, ", .{ n_decl, @tagName(typ) });
        }
        dbg("\n", .{});

        const inst: OpCode = @enumFromInt(try r.readByte());
        dbg("block: {s}\n", .{@tagName(inst)});

        r.context.pos = endpos;
        dbg("\n", .{});
    }
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
