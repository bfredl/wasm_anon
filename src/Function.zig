const Function = @This();
typeidx: u32,
codeoff: u32,
control: ?[]ControlItem,

const std = @import("std");
const defs = @import("./defs.zig");
const Module = @import("./Module.zig");
const dbg = std.debug.print;

const read = @import("./read.zig");
const readLeb = read.readLeb;
const readu = read.readu;
const readName = read.readName;
const Reader = read.Reader;

const ControlItem = struct {
    off: u32, // this is absolute (relative to mod.raw)
    jmp_t: u16,
};

pub fn parse(self: *Function, r: Reader, allocator: std.mem.Allocator) !void {
    self.codeoff = @intCast(r.context.pos);

    var n_locals: u32 = 1; // TODO
    const n_local_defs = try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        n_locals += n_decl;
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        dbg("{} x {s}, ", .{ n_decl, @tagName(typ) });
    }
    dbg("\n", .{});

    var level: u32 = 1;

    var clist: std.ArrayList(ControlItem) = .init(allocator);
    // these point to the entry point of each level. for if-else-end we put in else_ when we have seen it
    var cstack: std.ArrayList(struct { start: u16, else_: u16 = 0 }) = .init(allocator);
    // TODO: this is a sentinel, might be eliminated (use jmp_t = 0xFFFF instead for "INVALID")
    try clist.append(.{ .off = @intCast(r.context.pos), .jmp_t = 0 });
    try cstack.append(.{ .start = 0 });

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
                try read.blocktype(r);
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                try cstack.append(.{ .start = @intCast(clist.items.len - 1) });
            },
            .end => {
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                const start = cstack.items[cstack.items.len - 1].start;
                const off = clist.items[start].off;
                const start_op: defs.OpCode = @enumFromInt(r.context.buffer[off]);
                dbg(" (for {s} at {x:04})", .{ @tagName(start_op), off });
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
                dbg(" {}", .{idx});
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                dbg(" {}", .{val});
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                dbg(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
                if (idx >= n_locals) return error.InvalidFormat;
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

    dbg("\n\n", .{});
    for (0.., clist.items) |i, c| {
        dbg("{:2}: {x:04} {}\n", .{ i, c.off, c.jmp_t });
    }

    self.control = try clist.toOwnedSlice();
}

pub fn pop_binop(stack: *std.ArrayList(i32)) !struct { *i32, i32 } {
    if (stack.items.len < 2) return error.RuntimeError;
    const src = stack.pop();
    return .{ &stack.items[stack.items.len - 1], src };
}

pub fn execute(self: *Function, mod: *Module, param: i32) !i32 {
    var locals: [10]i32 = .{0} ** 10;
    locals[0] = param;
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
    var label_stack: std.ArrayList(u32) = .init(mod.allocator);

    // fbs.pos is the insruction pointer which is a bit weird but works
    while (true) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        dbg("{}: {s}\n", .{ pos, @tagName(inst) });
        switch (inst) {
            .i32_const => {
                const val = try readLeb(r, i32);
                try value_stack.append(val);
            },
            .i32_add => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* += src;
            },
            .i32_mul => {
                const dst, const src = try pop_binop(&value_stack);
                dst.* *= src;
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
                try read.blocktype(r);
                // target: right after "loop"
                try label_stack.append(@intCast(r.context.pos));
            },
            else => return error.NotImplemented,
        }
    }

    return error.NotImplemented;
}
