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

table_off: u32 = 0,
element_off: u32 = 0,

mem_limits: Limits = .{ .min = 0, .max = null },

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
            .export_ => self.export_off = @intCast(fbs.pos),

            .code => try self.code_section(r),
            .table => self.table_off = @intCast(fbs.pos),
            .element => self.element_off = @intCast(fbs.pos),
            .data => self.data_off = @intCast(fbs.pos),
            .custom => try self.custom_section(r, len),
            else => {
                // severe("NO {s}!\n", .{@tagName(kind)});
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
        const n_res = try readu(r);
        for (0..n_res) |_| {
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            dbg("{s}, ", .{@tagName(typ)});
        }
        dbg(")\n", .{});
    }
}

pub fn dbg_type(self: *Module, typeidx: u32) !void {
    if (self.types.len <= typeidx) return severe("[OUT OF BOUNDS]", .{});
    var fbs = self.fbs_at(self.types[typeidx]);
    const r = fbs.reader();
    const tag = try r.readByte();
    if (tag != 0x60) return error.InvalidFormat;
    const n_params = try readu(r);
    severe("[", .{});
    for (0..n_params) |i| {
        if (i > 0) severe(", ", .{});
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        severe("{s}", .{@tagName(typ)});
    }
    severe("] => [", .{});
    const n_res = try readu(r);
    for (0..n_res) |i| {
        if (i > 0) severe(", ", .{});
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        severe("{s}", .{@tagName(typ)});
    }
    severe("]\n", .{});
}

// currently we two-cycle the import section to first get the counts
pub fn import_section(self: *Module, r: Reader) !void {
    self.imports_off = @intCast(r.context.pos);
    const len = try readu(r);
    dbg("IMPORTS: {}\n", .{len});
    self.n_imports = len;
    for (0..len) |_| {
        _ = try readName(r);
        _ = try readName(r);
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        switch (kind) {
            .func => {
                _ = try readu(r);
                self.n_funcs_import += 1;
            },
            .table => {
                _ = try r.readByte();
                _ = try readLimits(r);
            },
            .mem => {
                _ = try readLimits(r);
            },
            .global => {
                _ = try r.readByte();
                _ = try r.readByte();
                self.n_globals_import += 1;
            },
        }
    }

    // if (dbg == severe) try self.dbg_imports();
}

pub fn dbg_imports(self: *Module) !void {
    var fbs = self.fbs_at(self.imports_off);
    const r = fbs.reader();
    const len = try readu(r);

    for (0..len) |_| {
        const mod = try readName(r);
        const name = try readName(r);
        severe("{s}:{s} = ", .{ mod, name });
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        switch (kind) {
            .func => {
                const idx = try readu(r);
                severe("func ", .{});
                try self.dbg_type(idx);
            },
            .table => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const limits = try readLimits(r);
                severe("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
            },
            .mem => {
                const limits = try readLimits(r);
                severe("mem w {}:{?}\n", .{ limits.min, limits.max });
            },
            .global => {
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                const mut = (try r.readByte()) > 0;
                severe("global {} {}\n", .{ typ, mut });
            },
        }
    }
}

pub fn dbg_exports(self: *Module) !void {
    var fbs = self.fbs_at(self.export_off);
    const r = fbs.reader();
    const len = try readu(r);
    severe("EXPORTS: {}\n", .{len});
    for (0..len) |_| {
        const name = try readName(r);
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const idx = try readu(r);
        severe("{s} = {s} {} ", .{ name, @tagName(kind), idx });
        if (kind == .func) {
            // TODO: reexport of imports allowed??
            try self.dbg_type(self.funcs_internal[idx - self.n_funcs_import].typeidx);
        }
        severe("\n", .{});
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

const Instance = @import("./Instance.zig");
pub fn init_imports(self: *Module, in: *Instance, imports: ?*ImportTable) !void {
    var fbs = self.fbs_at(self.imports_off);
    const r = fbs.reader();
    const len = try readu(r);

    var func_counter: u32 = 0;
    var global_counter: u32 = 0;

    for (0..len) |_| {
        const mod = try readName(r);
        const name = try readName(r);
        dbg("{s}:{s} = ", .{ mod, name });
        const kind: defs.ImportExportKind = @enumFromInt(try r.readByte());
        const imp = imports orelse return error.InvalidArgument;
        switch (kind) {
            .func => {
                const idx = try readu(r);
                const item = imp.funcs.get(name) orelse return error.MissingImport;
                const n_args, const n_res = try self.type_arity(idx);
                if (n_args != item.n_args or n_res != item.n_res) return error.ImportTypeMismatch;
                in.funcs_imported[func_counter] = item;
                func_counter += 1;
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
                in.globals_maybe_indir[global_counter] = .{ .indir = item.ref };
                global_counter += 1;
            },
        }
    }

    r.context.pos = self.globals_off;
    for (0..self.n_globals_internal) |i| {
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        _ = try r.readByte(); // WHO FUCKING CARES IF IT IS MUTABLE OR NOT

        in.globals_maybe_indir[self.n_globals_import + i] = try Interpreter.eval_constant_expr(r, typ, in.preglobals());
    }
}

pub fn type_arity(self: *const Module, type_idx: u32) !struct { u16, u16 } {
    var fbs_type = self.fbs_at(self.types[type_idx]);
    const r_type = fbs_type.reader();
    const tag = try r_type.readByte();
    if (tag != 0x60) return error.InvalidFormat;
    const n_params: u16 = @intCast(try readu(r_type));
    for (0..n_params) |_| {
        _ = try r_type.readByte(); // TEMP hack: while we don't validate runtime args
    }
    const n_res: u16 = @intCast(try readu(r_type));
    return .{ n_params, n_res };
}

pub fn table_section(self: *Module, in: *Instance) !void {
    var fbs = self.fbs_at(self.table_off);
    const r = fbs.reader();
    const len = try readu(r);
    dbg("Tables: {}\n", .{len});
    for (0..len) |_| {
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        const limits = try readLimits(r);
        dbg("table {s} w {}:{?}\n", .{ @tagName(typ), limits.min, limits.max });
        if (typ == .funcref and limits.min > 0) {
            if (in.funcref_table.len > 0) return error.NotImplemented;
            in.funcref_table = try self.allocator.alloc(u32, limits.min);
            @memset(in.funcref_table, defs.funcref_nil); // null
        }
    }
}

pub fn element_section(self: *Module, in: *Instance) !void {
    var fbs = self.fbs_at(self.element_off);
    const r = fbs.reader();
    const len = try readu(r);
    dbg("Elements: {}\n", .{len});
    for (0..len) |_| {
        const kinda = try readu(r);
        if (kinda == 0) {
            const offset: usize = @intCast((try Interpreter.eval_constant_expr(r, .i32, in.preglobals())).i32); // TODO: fail
            const elen = try readu(r);
            if (offset + len > in.funcref_table.len) {
                return error.InvalidFormat;
            }
            for (0..elen) |i| {
                const item = try readu(r);
                in.funcref_table[offset + i] = item;
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

pub fn custom_section(self: *Module, r: Reader, sec_len: usize) !void {
    const end_pos = r.context.pos + sec_len;
    const name = try readName(r);
    dbg("CUSTOM: {s} as {}\n", .{ name, sec_len });
    if (std.mem.eql(u8, name, "name")) {
        // LAYERING! they could just have used separate custom sections like
        // name.function and so on but that would have been too simple.
        while (r.context.pos < end_pos) {
            const kinda = try r.readByte();
            const size = try readu(r);

            if (kinda == 1 and self.funcs_internal.len > 0) { // function names, woho!
                const map_len = try readu(r);
                for (0..map_len) |_| {
                    const idx = try readu(r);
                    const funcname = try readName(r);
                    // dbg("waa {} is {s}\n", .{ idx, funcname });
                    if (idx >= self.n_funcs_import) {
                        const internal_idx = idx - self.n_funcs_import;
                        if (internal_idx < self.funcs_internal.len) {
                            self.funcs_internal[internal_idx].name = funcname;
                        }
                    }
                }
            } else {
                dbg("NAMM {} w size {}\n", .{ kinda, size });
                r.context.pos = @min(end_pos, r.context.pos + size);
            }
        }
    }
}

pub fn dump_counts(self: *Module) void {
    severe("\n\n", .{});
    for (self.funcs_internal) |i| {
        if (i.call_count > 0) {
            severe("{} : {s}\n", .{ i.call_count, i.name orelse "???" });
        }
    }

    severe("\n\n", .{});
    for (self.funcs_internal) |i| {
        if (i.control) |c| {
            for (c, 0..) |ci, cidx| {
                if (ci.count > 0) {
                    severe("{} : loop {} in {s}\n", .{ ci.count, cidx, i.name orelse "???" });
                }
            }
        }
    }
    severe("\n\n", .{});
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
