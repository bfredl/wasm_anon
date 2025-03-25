const Function = @This();
typeidx: u32,
n_params: u32 = undefined,
n_res: u32 = undefined,
codeoff: u32 = undefined,
control: ?[]ControlItem = null,

name: ?[]const u8 = null,

call_count: Counter = 0,

// need not be strict but can be an over-estimate
val_stack_max_height: u16 = 0,
const Counter = u64;

const std = @import("std");
const defs = @import("./defs.zig");
const ops = @import("./ops.zig");
const Module = @import("./Module.zig");
const Instance = @import("./Instance.zig");
const dbg = Module.dbg;
const severe = std.debug.print;

const read = @import("./read.zig");
const readLeb = read.readLeb;
const readu = read.readu;
const readName = read.readName;
const Reader = read.Reader;

const ControlItem = struct {
    off: u32, // this is absolute (relative to mod.raw)
    jmp_t: u16,
    count: Counter = 0,
};

pub fn ensure_parsed(self: *Function, mod: *Module) ![]ControlItem {
    if (self.control == null) {
        var fbs2 = mod.fbs_at(self.codeoff);
        const r_parse = fbs2.reader();
        try self.parse(mod, r_parse);
    }
    return self.control orelse @panic("how could you");
}

pub fn parse(self: *Function, mod: *Module, r: Reader) !void {
    self.n_params, self.n_res = try mod.type_arity(self.typeidx);

    self.codeoff = @intCast(r.context.pos);

    var n_locals: u32 = self.n_params;
    const n_local_defs = try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        n_locals += n_decl;
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        dbg("{} x {s}, ", .{ n_decl, @tagName(typ) });
    }
    dbg("\n", .{});

    try self.parse_body(mod, r, n_locals);
}

pub fn parse_body(self: *Function, mod: *Module, r: Reader, n_locals: u32) !void {
    var level: u32 = 1;

    var clist: std.ArrayList(ControlItem) = .init(mod.allocator);
    // these point to the entry point of each level. for if-else-end we put in else_ when we have seen it
    var cstack: std.ArrayList(struct { start: u16 }) = .init(mod.allocator);
    // TODO: this is a sentinel, might be eliminated (use jmp_t = 0xFFFF instead for "INVALID")
    // although having 0 as a "name" for the implicit entire-function block is useful..
    try clist.append(.{ .off = @intCast(r.context.pos), .jmp_t = 0 });
    try cstack.append(.{ .start = 0 });

    var val_stack_level: u16 = 0;
    var val_stack_height: u16 = 0;

    while (level >= 1) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        if (inst == .end or inst == .else_) level -= 1;

        dbg("{x:04}:", .{pos});
        for (0..level) |_| dbg("  ", .{});
        dbg("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                level += 1;
                const typ = try read.blocktype(r);
                dbg(" typ={}", .{typ});
                if (typ != .void) return error.NotImplemented;
                if (inst == .if_) val_stack_level -= 1;
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                try cstack.append(.{ .start = @intCast(clist.items.len - 1) });
            },
            .end => {
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                const start = &clist.items[cstack.items[cstack.items.len - 1].start];
                const start_op: defs.OpCode = @enumFromInt(r.context.buffer[start.off]);
                dbg(" (for {s} at {x:04})", .{ @tagName(start_op), start.off });
                start.jmp_t = @intCast(clist.items.len - 1);
                cstack.items.len -= 1;
            },
            .else_ => {
                level += 1;
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                dbg(" (for if_ at {x:04})", .{clist.items[cstack.items[cstack.items.len - 1].start].off});
                // "if_" jumps to "else_", and "else_" jumps to end
                clist.items[cstack.items[cstack.items.len - 1].start].jmp_t = @intCast(clist.items.len - 1);
                cstack.items[cstack.items.len - 1].start = @intCast(clist.items.len - 1);
            },
            .br, .br_if => {
                // try clist.append(.{ .off = pos, .jmp_t = 0 });
                const idx = try readu(r);
                if (inst == .br_if) val_stack_level -= 1;
                dbg(" {}", .{idx});
            },
            .br_table => {
                val_stack_level -= 1;
                const n = try readu(r);
                for (0..n) |_| {
                    _ = try readu(r);
                }
                _ = try readu(r); // default
            },
            .ret => {},
            .call => {
                const idx = try readLeb(r, u32);
                dbg(" {}", .{idx});
                const typ = if (idx < mod.n_funcs_import)
                    mod.funcs_imported_types[idx]
                else if (idx >= mod.n_funcs_import + mod.funcs_internal.len)
                    return error.InvalidFormat
                else
                    mod.funcs_internal[idx - mod.n_funcs_import].typeidx;
                const n_args, const n_res = try mod.type_arity(typ);
                val_stack_level -= n_args;
                val_stack_level += n_res;
            },
            .call_indirect => {
                const typeidx = try readLeb(r, u32);
                const tblidx = try readLeb(r, u32);
                dbg(" {}:{}", .{ typeidx, tblidx });
                const n_args, const n_res = try mod.type_arity(typeidx);
                val_stack_level -= n_args;
                val_stack_level += n_res;
            },
            .nop, .unreachable_ => {},
            .i32_const => {
                const val = try readLeb(r, i32);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .f32_const => {
                const val = try read.readf(r, f32);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .f64_const => {
                const val = try read.readf(r, f64);
                dbg(" {}", .{val});
                val_stack_level += 1;
            },
            .local_get, .local_set, .local_tee => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
                if (idx >= n_locals) return error.InvalidFormat;
                if (inst == .local_get) {
                    val_stack_level += 1;
                } else if (inst == .local_set) {
                    val_stack_level -= 1;
                }
            },
            .global_get, .global_set => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
                if (idx >= mod.n_globals_import + mod.n_globals_internal) return error.InvalidFormat;
                if (inst == .global_get) {
                    val_stack_level += 1;
                } else {
                    val_stack_level -= 1;
                }
            },
            .drop => val_stack_level -= 1,
            .select => val_stack_level -= 2,
            .select_t => {
                const num = try readu(r);
                if (num != 1) return error.InvalidFormat; // possible extension
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                dbg(" {}", .{typ});
                val_stack_level += 1;
            },
            .memory_size, .memory_grow => {
                if (try r.readByte() != 0) return error.InvalidFormat;
                if (inst == .memory_size) val_stack_level += 1;
            },
            .prefixed => {
                const code: defs.Prefixed = try read.prefix(r);
                dbg(":{s}", .{@tagName(code)});
                switch (code) {
                    .memory_fill => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        val_stack_level -= 3;
                    },
                    .memory_copy => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        val_stack_level -= 3;
                    },
                    else => {
                        if (@intFromEnum(code) < 8) {
                            // ok, converter
                        } else {
                            severe(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
                            return error.InvalidFormat;
                        }
                    },
                }
            },
            else => {
                const idx = @intFromEnum(inst);
                // TODO: maybe just a single "adj : [265]i8" for a lot of the cases
                if (idx >= 0x45 and idx <= 0x66) {
                    val_stack_level -= 1;
                } else if (idx >= 0x67 and idx <= 0x69) {
                    // ok, parameterless
                } else if (idx >= 0x6a and idx <= 0x78) {
                    val_stack_level -= 1;
                } else if (idx >= 0x79 and idx <= 0x7b) {
                    // ok, parameterless
                } else if (idx >= 0x7c and idx <= 0x8a) {
                    val_stack_level -= 1;
                } else if (idx >= 0x8b and idx <= 0x91) {
                    // ok, parameterless
                } else if (idx >= 0x92 and idx <= 0x98) {
                    val_stack_level -= 1;
                } else if (idx >= 0x99 and idx <= 0x9f) {
                    // ok, parameterless
                } else if (idx >= 0xa0 and idx <= 0xa6) {
                    val_stack_level -= 1;
                } else if (idx >= 0xa7 and idx <= 0xc4) {
                    // ok, parameterless
                } else if (idx >= 0x28 and idx <= 0x3e) {
                    const alignas = try readu(r);
                    const offset = try readu(r);
                    dbg(" a={} o={}", .{ alignas, offset });
                    if (idx >= 0x36) val_stack_level -= 1;
                } else {
                    severe("inst {s} TBD, aborting!\n", .{@tagName(inst)});
                    return error.NotImplemented;
                }
            },
        }

        val_stack_height = @max(val_stack_height, val_stack_level);
        dbg("\n", .{});
    }

    dbg("\n\n", .{});
    for (0.., clist.items) |i, c| {
        dbg("{:2}: {x:04} {}\n", .{ i, c.off, c.jmp_t });
    }

    self.control = try clist.toOwnedSlice();
    self.val_stack_max_height = val_stack_height;
}
