const Function = @This();
typeidx: u32,
n_params: u32 = undefined,
n_ret: u32 = undefined,
codeoff: u32 = undefined,
control: ?[]ControlItem = null,

const std = @import("std");
const defs = @import("./defs.zig");
const ops = @import("./ops.zig");
const Module = @import("./Module.zig");
const Instance = @import("./Instance.zig");
const dbg_parse = Module.dbg;
const severe = std.debug.print;
const dbg_rt = Module.nodbg;

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
    for (0..self.n_params) |_| {
        _ = try r_type.readByte(); // TEMP hack: while we don't validate runtime args
    }
    self.n_ret = try readu(r_type);

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
    var cstack: std.ArrayList(struct { start: u16 }) = .init(mod.allocator);
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
                const start = &clist.items[cstack.items[cstack.items.len - 1].start];
                const start_op: defs.OpCode = @enumFromInt(r.context.buffer[start.off]);
                dbg_parse(" (for {s} at {x:04})", .{ @tagName(start_op), start.off });
                start.jmp_t = @intCast(clist.items.len - 1);
                cstack.items.len -= 1;
            },
            .else_ => {
                level += 1;
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                dbg_parse(" (for if_ at {x:04})", .{clist.items[cstack.items[cstack.items.len - 1].start].off});
                // "if_" jumps to "else_", and "else_" jumps to end
                clist.items[cstack.items[cstack.items.len - 1].start].jmp_t = @intCast(clist.items.len - 1);
                cstack.items[cstack.items.len - 1].start = @intCast(clist.items.len - 1);
            },
            .br, .br_if => {
                // try clist.append(.{ .off = pos, .jmp_t = 0 });
                const idx = try readu(r);
                dbg_parse(" {}", .{idx});
            },
            .br_table => {
                const n = try readu(r);
                for (0..n) |_| {
                    _ = try readu(r);
                }
                _ = try readu(r); // default
            },
            .ret => {},
            .call => {
                const idx = try readLeb(r, u32);
                dbg_parse(" {}", .{idx});
            },
            .call_indirect => {
                const tblidx = try readLeb(r, u32);
                const idx = try readLeb(r, u32);
                dbg_parse(" {}:{}", .{ tblidx, idx });
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                dbg_parse(" {}", .{val});
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                dbg_parse(" {}", .{val});
            },
            .f32_const => {
                const ival = try r.readInt(u32, .little);
                const val: f32 = @bitCast(ival);
                dbg_parse(" {}", .{val});
            },
            .f64_const => {
                const ival = try r.readInt(u64, .little);
                const val: f64 = @bitCast(ival);
                dbg_parse(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try readu(r);
                dbg_parse(" {}", .{idx});
                if (idx >= n_locals) return error.InvalidFormat;
            },
            .global_get, .global_set => {
                const idx = try readu(r);
                dbg_parse(" {}", .{idx});
                if (idx >= mod.n_globals) return error.InvalidFormat;
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
                    return error.NotImplemented;
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

pub fn top(stack: *std.ArrayList(StackValue)) !*StackValue {
    if (stack.items.len < 1) return error.RuntimeError;
    return &stack.items[stack.items.len - 1];
}

pub fn pop_binop(stack: *std.ArrayList(StackValue)) !struct { *StackValue, StackValue } {
    if (stack.items.len < 2) return error.RuntimeError;
    const src = stack.pop();
    return .{ &stack.items[stack.items.len - 1], src };
}

fn u(val: i32) u32 {
    return @bitCast(val);
}

// type punning is NOT safe, this assumes validaded WASM code
pub const StackValue = extern union {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
};

pub const StackLabel = struct {
    c_ip: u32,
    n_args: u16,
    stack_level: u16,
};

pub fn execute(self: *Function, mod: *Module, in: *Instance, params: []const StackValue) !StackValue {
    if (self.control == null) {
        var fbs2 = mod.fbs_at(self.codeoff);
        const r_parse = fbs2.reader();
        try self.parse(mod, r_parse);
    }

    // TODO: typesafe init??
    var locals: [10]StackValue = .{StackValue{ .i32 = 0 }} ** 10;
    if (params.len != self.n_params) return error.InvalidArgument;
    @memcpy(locals[0..self.n_params], params);
    var fbs = mod.fbs_at(self.codeoff);
    const r = fbs.reader();

    const control = self.control orelse @panic("how could you");

    var n_locals: u32 = 1; // TODO
    const n_local_defs = try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        n_locals += n_decl;
        if (typ != .i32) return error.NotImplemented;
    }
    if (n_locals > 10) return error.NotImplemented;

    var value_stack: std.ArrayList(StackValue) = .init(mod.allocator);
    defer value_stack.deinit();
    var label_stack: std.ArrayList(StackLabel) = .init(mod.allocator);
    defer label_stack.deinit();

    var c_ip: u32 = 0;

    // entire body is implicitly a block, producing the return values
    try label_stack.append(.{ .c_ip = @intCast(control.len - 1), .stack_level = 0, .n_args = @intCast(self.n_ret) });

    // fbs.pos is the insruction pointer which is a bit weird but works
    while (true) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        dbg_rt("{x:04}: {s} (c={}, values={}, labels={})\n", .{ pos, @tagName(inst), c_ip, value_stack.items.len, label_stack.items.len });
        var label_target: ?u32 = null;
        switch (inst) {
            .drop => {
                _ = value_stack.popOrNull() orelse return error.RuntimeError;
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                try value_stack.append(.{ .i32 = val });
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                try value_stack.append(.{ .i64 = val });
            },
            .f32_const => {
                const ival = try r.readInt(u32, .little);
                try value_stack.append(.{ .f32 = @bitCast(ival) });
            },
            .f64_const => {
                const ival = try r.readInt(u64, .little);
                try value_stack.append(.{ .f64 = @bitCast(ival) });
            },
            .i32_eqz => {
                const dst = try top(&value_stack);
                dst.i32 = if (dst.i32 == 0) 1 else 0;
            },
            .i64_eqz => {
                const dst = try top(&value_stack);
                dst.i32 = if (dst.i64 == 0) 1 else 0;
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
            .global_get => {
                const idx = try readu(r);
                try value_stack.append(in.globals[idx]);
            },
            .global_set => {
                const idx = try readu(r);
                const val = value_stack.popOrNull() orelse return error.RuntimeError;
                in.globals[idx] = val;
            },
            .loop => {
                c_ip += 1;
                _ = try read.blocktype(r);
                // target: right after "loop"
                try label_stack.append(.{ .c_ip = c_ip, .stack_level = @intCast(value_stack.items.len), .n_args = 0 });
            },
            .br => {
                label_target = try readu(r);
            },
            .br_if => {
                const idx = try readu(r);
                if (idx + 1 > label_stack.items.len) return error.RuntimeError;
                const val = value_stack.popOrNull() orelse return error.RuntimeError;
                if (val.i32 != 0) {
                    label_target = idx;
                }
            },
            .br_table => {
                const val = value_stack.popOrNull() orelse return error.RuntimeError;
                const n = try readu(r);
                var target: ?u32 = null;
                for (0..n) |i| {
                    const ival = try readu(r);
                    if (val.i32 == i) target = ival;
                }
                const default = try readu(r);
                label_target = target orelse default;
            },
            .block => {
                c_ip += 1;
                const typ = try read.blocktype(r);
                const n_results = try typ.results();
                // target: right after "loop"
                if (control[c_ip].off != pos) @panic("MANIC FEAR");
                try label_stack.append(.{ .c_ip = control[c_ip].jmp_t, .stack_level = @intCast(value_stack.items.len), .n_args = n_results });
            },
            .if_ => {
                c_ip += 1;
                const typ = try read.blocktype(r);
                const n_results = try typ.results();
                if (control[c_ip].off != pos) @panic("TREMBLING FEAR");
                const val = value_stack.popOrNull() orelse return error.RuntimeError;
                if (val.i32 != 0) {
                    try label_stack.append(.{ .c_ip = control[c_ip].jmp_t, .stack_level = @intCast(value_stack.items.len), .n_args = n_results }); // we worry about else vs end when we get there..
                } else {
                    c_ip = control[c_ip].jmp_t;
                    r.context.pos = control[c_ip].off;
                    const c_inst: defs.OpCode = @enumFromInt(try r.readByte());
                    if (c_inst == .else_) {
                        try label_stack.append(.{ .c_ip = control[c_ip].jmp_t, .stack_level = @intCast(value_stack.items.len), .n_args = n_results });
                    } else {
                        if (c_inst != .end) @panic("SCREAMING FEAR");
                        // we already skipped over the "end"
                    }
                }
            },
            .else_ => {
                c_ip += 1;
                if (control[c_ip].off != pos) @panic("CONFLICKTED FEAR");
                // we can only reach/jump to else from inside the "then" block. time to exit!
                c_ip = control[c_ip].jmp_t;
                // execute the end inline
                r.context.pos = control[c_ip].off + 1;
                _ = label_stack.popOrNull() orelse break;
            },
            .end => {
                c_ip += 1;
                if (control[c_ip].off != pos) @panic("PANIKED FEAR");
                _ = label_stack.popOrNull() orelse @panic("RUSHED FEAR");
                // todo: cannot do this if we popped a "loop" header
                // if (value_stack.items.len != item.stack_level + item.n_args) @panic("SAD FEAR");
                if (label_stack.items.len == 0) {
                    if (value_stack.items.len != self.n_ret) return error.RuntimeError;
                    break;
                }
            },
            .ret => {
                break;
            },
            .call => {
                const idx = try readLeb(r, u32);
                _ = idx;
                severe("lies!\n", .{});
            },
            inline else => |tag| {
                const category = comptime defs.category(tag);
                const name = @tagName(tag);
                switch (category) {
                    .i32_unop => {
                        const dst = try top(&value_stack);
                        dst.i32 = @field(ops.iunop, name[4..])(i32, dst.i32);
                    },
                    .i32_binop => {
                        const dst, const src = try pop_binop(&value_stack);
                        dst.i32 = try @field(ops.ibinop, name[4..])(i32, dst.i32, src.i32);
                    },
                    .i32_relop => {
                        const dst, const src = try pop_binop(&value_stack);
                        dst.i32 = if (@field(ops.irelop, name[4..])(i32, dst.i32, src.i32)) 1 else 0;
                    },
                    .i64_unop => {
                        const dst = try top(&value_stack);
                        dst.i64 = @field(ops.iunop, name[4..])(i64, dst.i64);
                    },
                    .i64_binop => {
                        const dst, const src = try pop_binop(&value_stack);
                        dst.i64 = try @field(ops.ibinop, name[4..])(i64, dst.i64, src.i64);
                    },
                    .i64_relop => {
                        const dst, const src = try pop_binop(&value_stack);
                        dst.i32 = if (@field(ops.irelop, name[4..])(i64, dst.i64, src.i64)) 1 else 0;
                    },
                    .other => {
                        severe("{}: {s}\n", .{ pos, @tagName(inst) });
                        return error.NotImplemented;
                    },
                }
            },
        }
        if (label_target) |idx| {
            label_stack.items.len -= idx;
            const last = label_stack.getLastOrNull() orelse return error.RuntimeError;
            c_ip = last.c_ip;
            r.context.pos = control[c_ip].off;
            const new_level = last.stack_level + last.n_args;
            if (value_stack.items.len < new_level) @panic("DISASSOCIATING FEAR");
            if (value_stack.items.len > new_level) {
                const src = value_stack.items.len - last.n_args;
                std.mem.copyForwards(StackValue, value_stack.items[last.stack_level..][0..last.n_args], value_stack.items[src..][0..last.n_args]);
                value_stack.items.len = new_level;
            }
            // we don't want to rexec the loop header. however execute the "end"
            // target to clean-up the stack.
            if (r.context.buffer[r.context.pos] == @intFromEnum(defs.OpCode.loop)) {
                r.context.pos += 1;
                _ = try read.blocktype(r);
            } else {
                c_ip -= 1; // messy!
            }
        }
    }
    if (value_stack.items.len < self.n_ret) return error.RuntimeError;

    return if (self.n_ret > 0) value_stack.items[value_stack.items.len - 1] else .{ .i32 = 0x4EADBEAF };
}
