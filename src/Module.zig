const std = @import("std");
const testing = std.testing;
const dbg = std.debug.print;

const defs = @import("./defs.zig");
const Module = @This();

const read = @import("./read.zig");
const readu = read.readu;
const readName = read.readName;
const Reader = read.Reader;

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
// TODO: {.ptr = undefined, .size = 0} would be a useful idiom..
funcs: []Function = undefined,

export_off: u32 = 0,

const Function = @import("./Function.zig");

pub fn fbs_at(self: Module, off: u32) std.io.FixedBufferStream([]const u8) {
    return .{ .buffer = self.raw, .pos = off };
}

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

        const pos = fbs.pos;
        const len = try readu(r);
        dbg("SECTION: {s} ({}) at {} with len {}\n", .{ @tagName(kind), id, pos, len });
        const end_pos = fbs.pos + len;
        switch (kind) {
            .type => try type_section(r),
            .function => try self.function_section(r),
            .memory => try memory_section(r),
            .global => try global_section(r),
            .import => try import_section(r),
            .export_ => {
                self.export_off = @intCast(fbs.pos);
                try export_section_dbg(r);
            },
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

fn export_section_dbg(r: Reader) !void {
    const len = try readu(r);
    dbg("EXPORTS: {}\n", .{len});
    for (0..len) |_| {
        const name = try readName(r);
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try readu(r);
        dbg("{s} = {s} {}\n", .{ name, @tagName(kind), idx });
    }
}

pub const Export = struct { kind: defs.ImportExportKind, idx: u32 };

pub fn lookup_export(self: *Module, name: []const u8) !?Export {
    if (self.export_off == 0) return null;
    var fbs = self.fbs_at(self.export_off);
    const r = fbs.reader();

    const len = try readu(r);
    for (0..len) |_| {
        const item_name = try readName(r);
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try readu(r);
        if (std.mem.eql(u8, name, item_name)) {
            return .{ .kind = kind, .idx = idx };
        }
    }
    return null;
}

fn function_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    self.funcs = try self.allocator.alloc(Function, len);
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

        try self.funcs[i].parse(r, self.allocator);

        r.context.pos = endpos;
        dbg("\n", .{});
    }
}

pub fn execute(self: *Module, idx: u32, arg0: i32) !i32 {
    if (idx >= self.funcs.len) return error.OutOfRange;
    return self.funcs[idx].execute(self, arg0);
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
