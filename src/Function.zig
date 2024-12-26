const Function = @This();
typeidx: u32,
n_params: u32, // TEMP hack: while we assume only i32 args
codeoff: u32,
control: ?[]ControlItem,

const std = @import("std");
const defs = @import("./defs.zig");
const Module = @import("./Module.zig");
const dbg_parse = Module.dbg;
const severe = std.debug.print;

const read = @import("./read.zig");
const readLeb = read.readLeb;
const readu = read.readu;
const readName = read.readName;
const Reader = read.Reader;

const ControlItem = struct {
    off: u32, // this is absolute (relative to mod.raw)
    jmp_t: u16,
};

pub fn parse(self: *Function, mod: *Module, r: Reader) !void {
    var fbs_type = mod.fbs_at(mod.types[self.typeidx]);
    const r_type = fbs_type.reader();

    const tag = try r_type.readByte();
    if (tag != 0x60) return error.InvalidFormat;
    self.n_params = try readu(r_type);

    self.codeoff = @intCast(r.context.pos);

    var n_locals: u32 = self.n_params;
    const n_local_defs = try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        n_locals += n_decl;
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        dbg_parse("{} x {s}, ", .{ n_decl, @tagName(typ) });
    }
    dbg_parse("\n", .{});

    var level: u32 = 1;

    var clist: std.ArrayList(ControlItem) = .init(mod.allocator);
    // these point to the entry point of each level. for if-else-end we put in else_ when we have seen it
    var cstack: std.ArrayList(struct { start: u16, else_: u16 = 0 }) = .init(mod.allocator);
    // TODO: this is a sentinel, might be eliminated (use jmp_t = 0xFFFF instead for "INVALID")
    try clist.append(.{ .off = @intCast(r.context.pos), .jmp_t = 0 });
    try cstack.append(.{ .start = 0 });

    while (level >= 1) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        if (inst == .end or inst == .else_) level -= 1;

        dbg_parse("{x:04}:", .{pos});
        for (0..level) |_| dbg_parse("  ", .{});
        dbg_parse("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                level += 1;
                const typ = try read.blocktype(r);
                dbg_parse(" typ={}", .{typ});
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                try cstack.append(.{ .start = @intCast(clist.items.len - 1) });
            },
            .end => {
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                const start = cstack.items[cstack.items.len - 1].start;
                const off = clist.items[start].off;
                const start_op: defs.OpCode = @enumFromInt(r.context.buffer[off]);
                dbg_parse(" (for {s} at {x:04})", .{ @tagName(start_op), off });
                cstack.items.len -= 1;
            },
            .ret => {},
            .else_ => {
                level += 1;
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                cstack.items[cstack.items.len - 1].else_ = @intCast(clist.items.len - 1);
            },
            .br, .br_if => {
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                const idx = try readu(r);
                dbg_parse(" {}", .{idx});
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                dbg_parse(" {}", .{val});
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                dbg_parse(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try readu(r);
                dbg_parse(" {}", .{idx});
                if (idx >= n_locals) return error.InvalidFormat;
            },
            .drop, .select => {},
            .prefixed => {
                const code: defs.Prefixed = @enumFromInt(try readu(r));
                dbg_parse(":{s}", .{@tagName(code)});
                switch (code) {
                    .memory_fill => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    .memory_copy => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    else => {
                        severe(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
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
                    dbg_parse(" a={} o={}", .{ alignas, offset });
                } else {
                    severe("inst {s} TBD, aborting!\n", .{@tagName(inst)});
                    return;
                }
            },
        }
        dbg_parse("\n", .{});
    }

    dbg_parse("\n\n", .{});
    for (0.., clist.items) |i, c| {
        dbg_parse("{:2}: {x:04} {}\n", .{ i, c.off, c.jmp_t });
    }

    self.control = try clist.toOwnedSlice();
}

pub fn top(stack: *std.ArrayList(i32)) !*i32 {
    if (stack.items.len < 1) return error.RuntimeError;
    return &stack.items[stack.items.len - 1];
}

pub fn pop_binop(stack: *std.ArrayList(i32)) !struct { *i32, i32 } {
    if (stack.items.len < 2) return error.RuntimeError;
    const src = stack.pop();
    return .{ &stack.items[stack.items.len - 1], src };
}

fn u(val: i32) u32 {
    return @bitCast(val);
}

pub fn execute(self: *Function, mod: *Module, params: []const i32) !i32 {
    var locals: [10]i32 = .{0} ** 10;
    if (params.len != self.n_params) return error.InvalidArgument;
    @memcpy(locals[0..self.n_params], params);
    var fbs = mod.fbs_at(self.codeoff);
    const r = fbs.reader();

    var n_locals: u32 = 1; // TODO
    const n_local_defs = try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        n_locals += n_decl;
        if (typ != .i32) return error.NotImplemented;
    }
    if (n_locals > 10) return error.NotImplemented;

    var value_stack: std.ArrayList(i32) = .init(mod.allocator);
    defer value_stack.deinit();
    var label_stack: std.ArrayList(u32) = .init(mod.allocator);
    defer label_stack.deinit();

    // fbs.pos is the insruction pointer which is a bit weird but works
    while (true) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        switch (inst) {
            .i32_const => {
                const val = try readLeb(r, i32);
                try value_stack.append(val);
            },
            .i32_add => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* +%= src;
            },
            .i32_sub => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* -%= src;
            },
            .i32_mul => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* *%= src;
            },
            .i32_div_s => {
                const dst, const src = try pop_binop(&value_stack);
                if (src == 0) return error.WASMTrap;
                if (dst.* == @as(i32, @bitCast(@as(u32, 0x80000000))) and src == -1) return error.WASMTrap;
                dst.* = @divTrunc(dst.*, src);
            },
            .i32_div_u => {
                const dst, const src = try pop_binop(&value_stack);
                if (src == 0) return error.WASMTrap;
                dst.* = @bitCast(@divTrunc(u(dst.*), u(src)));
            },
            .i32_rem_s => {
                const dst, const src = try pop_binop(&value_stack);
                if (src == 0) return error.WASMTrap;
                dst.* = if (dst.* == @as(i32, @bitCast(@as(u32, 0x80000000))) and src == -1) 0 else @rem(dst.*, src);
            },
            .i32_rem_u => {
                const dst, const src = try pop_binop(&value_stack);
                if (src == 0) return error.WASMTrap;
                dst.* = @bitCast(@rem(u(dst.*), u(src)));
            },
            .i32_and => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* &= src;
            },
            .i32_or => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* |= src;
            },
            .i32_xor => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* ^= src;
            },
            .i32_shl => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* <<= @truncate(u(src));
            },
            .i32_shr_s => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* >>= @truncate(u(src));
            },
            .i32_shr_u => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = @bitCast(u(dst.*) >> @truncate(u(src)));
            },
            .i32_rotl => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = @bitCast(std.math.rotl(u32, @bitCast(dst.*), src));
            },
            .i32_rotr => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = @bitCast(std.math.rotr(u32, @bitCast(dst.*), src));
            },
            .i32_clz => {
                const dst = try top(&value_stack);
                dst.* = @clz(u(dst.*));
            },
            .i32_ctz => {
                const dst = try top(&value_stack);
                dst.* = @ctz(u(dst.*));
            },
            .i32_popcnt => {
                const dst = try top(&value_stack);
                dst.* = @popCount(u(dst.*));
            },
            .i32_extend8_s => {
                const dst = try top(&value_stack);
                dst.* = @as(i8, @truncate(dst.*));
            },
            .i32_extend16_s => {
                const dst = try top(&value_stack);
                dst.* = @as(i16, @truncate(dst.*));
            },
            .i32_eqz => {
                const dst = try top(&value_stack);
                dst.* = if (dst.* == 0) 1 else 0;
            },
            .i32_eq => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (dst.* == src) 1 else 0;
            },
            .i32_ne => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (dst.* != src) 1 else 0;
            },
            .i32_lt_s => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (dst.* < src) 1 else 0;
            },
            .i32_lt_u => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (u(dst.*) < u(src)) 1 else 0;
            },
            .i32_le_s => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (dst.* <= src) 1 else 0;
            },
            .i32_le_u => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (u(dst.*) <= u(src)) 1 else 0;
            },
            .i32_gt_s => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (dst.* > src) 1 else 0;
            },
            .i32_gt_u => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (u(dst.*) > u(src)) 1 else 0;
            },
            .i32_ge_s => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (dst.* >= src) 1 else 0;
            },
            .i32_ge_u => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* = if (u(dst.*) >= u(src)) 1 else 0;
            },
            .local_get => {
                const idx = try readu(r);
                try value_stack.append(locals[idx]);
            },
            .local_set => {
                const idx = try readu(r);
                const val = value_stack.popOrNull() orelse return error.RuntimeError;
                locals[idx] = val;
            },
            .local_tee => {
                const idx = try readu(r);
                const val = value_stack.getLastOrNull() orelse return error.RuntimeError;
                locals[idx] = val;
            },
            .loop => {
                _ = try read.blocktype(r);
                // target: right after "loop"
                try label_stack.append(@intCast(r.context.pos));
            },
            .br_if => {
                const idx = try readu(r);
                if (idx != 0) return error.NotImplemented;
                const val = value_stack.popOrNull() orelse return error.RuntimeError;
                if (val != 0) {
                    const loc = label_stack.getLastOrNull() orelse return error.RuntimeError;
                    r.context.pos = loc;
                }
            },
            .end => {
                _ = label_stack.popOrNull() orelse break;
            },
            else => {
                severe("{}: {s}\n", .{ pos, @tagName(inst) });
                return error.NotImplemented;
            },
        }
    }
    if (value_stack.items.len != 1) return error.RuntimeError;

    return value_stack.items[0];
}
