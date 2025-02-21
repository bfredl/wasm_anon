const std = @import("std");
const testing = std.testing;

//pub const dbg = std.debug.print;
pub const dbg = nodbg;
pub fn nodbg(str: anytype, vals: anytype) void {
    _ = str;
    _ = vals;
}

const severe = std.debug.print;

const defs = @import("./defs.zig");
const Module = @This();

const ImportTable = @import("./ImportTable.zig");

const read = @import("./read.zig");
const readu = read.readu;
const readName = read.readName;
const Reader = read.Reader;
const Limits = struct { min: u32, max: ?u32 };

fn readLimits(r: Reader) !Limits {
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
n_funcs_import: u32 = 0,
// TODO: {.ptr = undefined, .size = 0} would be a useful idiom..
funcs_internal: []Function = &.{},
types: []u32 = undefined,

export_off: u32 = 0,
data_off: u32 = 0,

// TODO: maybe not even parse "imports" until justin time?
n_globals_import: u32 = 0,
n_globals_internal: u32 = 0,
globals_off: u32 = 0,

n_imports: u32 = 0,
imports_off: u32 = 0,

mem_limits: Limits = .{ .min = 0, .max = null },

// this a bit of a hack, I think a module can has multiple funcref tables
funcref_table: []u32 = &.{},

const Function = @import("./Function.zig");
const Interpreter = @import("./Interpreter.zig");

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
            .type => try self.type_section(r),
            .function => try self.function_section(r),
            .memory => try self.memory_section(r),
            .global => try self.global_section(r),
            .import => try self.import_section(r),
            .export_ => {
                self.export_off = @intCast(fbs.pos);
                try export_section_dbg(r);
            },
            .code => try self.code_section(r),
            .table => try self.table_section(r),
            .element => try self.element_section(r),
            .data => self.data_off = @intCast(fbs.pos),
            else => {
                // dbg("NO {s}!\n", .{@tagName(kind)});
            },
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
    // ?[]Thingie is annoying when you could use .{.data = static, .len = 0}
    // this shalt be a common pattern somehow. Perhaps a wrapping allocator unless Allocator wrapper is smart already.
    if (self.funcs_internal.len > 0) {
        self.allocator.free(self.funcs_internal);
    }
}

pub fn type_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    dbg("TYPES: {}\n", .{len});
    self.types = try self.allocator.alloc(u32, len);
    for (0..len) |i| {
        self.types[i] = @intCast(r.context.pos);
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

pub fn import_section(self: *Module, r: Reader) !void {
    self.imports_off = @intCast(r.context.pos);
    const len = try readu(r);
    dbg("IMPORTS: {}\n", .{len});
    self.n_imports = len;
    for (0..len) |_| {
        const mod = try readName(r);
        const name = try readName(r);
        dbg("{s}:{s} = ", .{ mod, name });
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        switch (kind) {
            .func => {
                const idx = try readu(r);
                dbg("func {}\n", .{idx});
                self.n_funcs_import += 1;
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
                self.n_globals_import += 1;
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
    if (len > 0) self.funcs_internal = try self.allocator.alloc(Function, len);
    dbg("FUNCS: {}\n", .{len});
    for (0..len) |i| {
        const idx = try readu(r);
        self.funcs_internal[i] = .{ .typeidx = idx };
    }
    dbg("...\n", .{});
}

pub fn memory_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    dbg("MEMORYS: {}\n", .{len});
    for (0..len) |i| {
        const lim = try readLimits(r);
        dbg("mem {}:{?}\n", .{ lim.min, lim.max });
        if (i == 0) {
            self.mem_limits = lim;
        } else {
            return error.NotImplemented;
        }
    }
}

pub fn init_data(self: *Module, mem: []u8, preglobals: []const defs.StackValue) !void {
    if (self.data_off == 0) return;

    var fbs = self.fbs_at(self.data_off);
    const r = fbs.reader();

    const len = try readu(r);
    dbg("DATAS: {}\n", .{len});
    for (0..len) |_| {
        const typ = try readu(r);
        dbg("TYP: {}\n", .{typ});

        if (typ > 2 or typ == 1) return error.NotImplemented;
        const memidx = if (typ == 0) 0 else try readu(r);
        if (memidx > 0) return error.NotImplemented;

        const offset: usize = @intCast((try Interpreter.eval_constant_expr(r, .i32, preglobals)).i32);
        const lenna = try readu(r);
        dbg("offsetta: {}, len: {}\n", .{ offset, lenna });

        if (offset + lenna > mem.len) return error.WASMTrap;
        try r.readNoEof(mem[offset..][0..lenna]);
    }
}

pub fn global_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    dbg("GLOBALS: {}\n", .{len});
    self.n_globals_internal = len;
    self.globals_off = @intCast(r.context.pos);
}

pub fn init_globals(self: *Module, globals: []defs.StackValue, imports: ?ImportTable) !void {
    var fbs = self.fbs_at(self.imports_off);
    const r = fbs.reader();
    const len = try readu(r);

    for (0..len) |i| {
        const mod = try readName(r);
        const name = try readName(r);
        dbg("{s}:{s} = ", .{ mod, name });
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const imp = imports orelse return error.InvalidArgument;
        switch (kind) {
            .func => {
                const idx = try readu(r);
                _ = idx;
            },
            .table => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const limits = try readLimits(r);
                _ = typ;
                _ = limits;
            },
            .mem => {
                const limits = try readLimits(r);
                _ = limits;
            },
            .global => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const mut = (try r.readByte()) > 0;
                _ = mut;

                const item = imp.globals.get(name) orelse return error.MissingImport;
                if (typ != item.typ) return error.ImportTypeMismatch;
                globals[i] = .{ .indir = item.ref };
            },
        }
    }

    r.context.pos = self.globals_off;
    for (0..self.n_globals_internal) |i| {
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        _ = try r.readByte(); // WHO FUCKING CARES IF IT IS MUTABLE OR NOT

        globals[self.n_globals_import + i] = try Interpreter.eval_constant_expr(r, typ, globals[0..self.n_globals_import]);
    }
}

pub fn table_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    dbg("Tables: {}\n", .{len});
    for (0..len) |_| {
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        const limits = try readLimits(r);
        dbg("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
        if (typ == .funcref and limits.min > 0) {
            if (self.funcref_table.len > 0) return error.NotImplemented;
            self.funcref_table = try self.allocator.alloc(u32, limits.min);
            @memset(self.funcref_table, 0xffffffff); // TODO: or zero?? what is it??
        }
    }
}

pub fn element_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    dbg("Elements: {}\n", .{len});
    for (0..len) |_| {
        const kinda = try readu(r);
        if (kinda == 0) {
            const offset: usize = @intCast((try Interpreter.eval_constant_expr(r, .i32, &.{})).i32); // TODO: fail
            const elen = try readu(r);
            if (offset + len > self.funcref_table.len) {
                return error.InvalidFormat;
            }
            for (0..elen) |i| {
                const item = try readu(r);
                self.funcref_table[offset + i] = item;
            }
        } else {
            severe("KINDA: {}\n", .{kinda});
            @panic("unhandled!");
        }
    }
}

pub fn code_section(self: *Module, r: Reader) !void {
    const len = try readu(r);
    dbg("Codes: {}\n", .{len});
    for (0..len) |i| {
        const size = try readu(r);
        dbg("CODE with size {}\n", .{size});
        const endpos = r.context.pos + size;
        self.funcs_internal[i].codeoff = @intCast(r.context.pos);

        // if(force_eager)
        // try self.funcs[i].parse(self, r);

        r.context.pos = endpos;
        dbg("\n", .{});
    }
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
