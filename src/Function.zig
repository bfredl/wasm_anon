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

    try self.parse_body(mod, r, n_locals);
}

pub fn parse_body(self: *Function, mod: *Module, r: Reader, n_locals: u32) !void {
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
                const val = try read.readf(r, f32);
                dbg_parse(" {}", .{val});
            },
            .f64_const => {
                const val = try read.readf(r, f64);
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

// evaluate expression at "off". This is like a function body but without locals for some reason
// This assumes that repeated instantiation of the same module with complex initializers is rare,
// i e you would likely use "start" or call to a real function with actual locals. Thus we don't save
// these parsed pseudo-functions
pub fn eval_expr(mod: *Module, r: Reader, typ: defs.ValType) !StackValue {
    const savepos = r.context.pos;
    // shortcut: `expr` is just "i??.const VAL end"
    quick_try: {
        const init_typ: defs.OpCode = @enumFromInt(try r.readByte());
        _ = typ;
        const val: StackValue = switch (init_typ) {
            .i32_const => .{ .i32 = try read.readLeb(r, i32) },
            .i64_const => .{ .i64 = try read.readLeb(r, i64) },
            .f32_const => .{ .f32 = try read.readf(r, f32) },
            .f64_const => .{ .f64 = try read.readf(r, f64) },
            else => break :quick_try,
        };
        if (try r.readByte() != 0x0b) break :quick_try;
        return val;
    }
    if (true) return error.NotImplemented;

    r.context.pos = savepos;

    var self: Function = .{ .n_params = 0, .n_ret = 1, .codeoff = savepos };
    self.parse_body(mod, r, 0);
    defer self.deinit();

    const init_in: *Instance = undefined;
    return self.execute(mod, init_in, &.{}, true);
}

fn u(val: i32) u32 {
    return @bitCast(val);
}

const StackValue = defs.StackValue;

pub const StackLabel = struct {
    c_ip: u32,
    n_vals: u16,
    stack_level: u16,
};

pub const StackMachine = struct {
    values: std.ArrayList(StackValue),
    labels: std.ArrayList(StackLabel),

    pub fn init(allocator: std.mem.Allocator) StackMachine {
        return .{
            .values = .init(allocator),
            .labels = .init(allocator),
        };
    }

    pub fn deinit(self: *StackMachine) void {
        self.values.deinit();
        self.labels.deinit();
    }

    pub fn push(self: *StackMachine, value: StackValue) !void {
        try self.values.append(value);
    }

    pub fn pop(self: *StackMachine) !StackValue {
        return self.values.popOrNull() orelse return error.RuntimeError;
    }

    pub fn push_label(self: *StackMachine, c_ip: u32, n_vals: u16) !void {
        try self.labels.append(.{ .c_ip = c_ip, .stack_level = @intCast(self.values.items.len), .n_vals = n_vals });
    }

    pub fn top(self: *StackMachine) !*StackValue {
        const stack = &self.values;
        if (stack.items.len < 1) return error.RuntimeError;
        return &stack.items[stack.items.len - 1];
    }

    pub fn pop_binop(self: *StackMachine) !struct { *StackValue, StackValue } {
        const stack = &self.values;
        if (stack.items.len < 2) return error.RuntimeError;
        const src = stack.pop();
        return .{ &stack.items[stack.items.len - 1], src };
    }

    pub fn pop_and_jump_labels(self: *StackMachine, levels: u32) !u32 {
        self.labels.items.len -= levels;
        const last = self.labels.getLastOrNull() orelse return error.RuntimeError;
        const c_ip = last.c_ip;
        const new_level = last.stack_level + last.n_vals;
        if (self.values.items.len < new_level) @panic("DISASSOCIATING FEAR");
        if (self.values.items.len > new_level) {
            const src = self.values.items.len - last.n_vals;
            std.mem.copyForwards(StackValue, self.values.items[last.stack_level..][0..last.n_vals], self.values.items[src..][0..last.n_vals]);
            self.values.items.len = new_level;
        }
        return c_ip;
    }
};

pub fn execute(self: *Function, mod: *Module, in: *Instance, params: []const StackValue, skip_locals: bool) !StackValue {
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

    var n_locals: u32 = self.n_params;
    const n_local_defs = if (skip_locals) 0 else try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        n_locals += n_decl;
        if (typ != .i32) return error.NotImplemented;
    }
    if (n_locals > 10) return error.NotImplemented;

    var stack: StackMachine = .init(mod.allocator);
    defer stack.deinit();

    var c_ip: u32 = 0;

    // entire body is implicitly a block, producing the return values
    try stack.push_label(@intCast(control.len - 1), @intCast(self.n_ret));

    // fbs.pos is the insruction pointer which is a bit weird but works
    while (true) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        dbg_rt("{x:04}: {s} (c={}, values={}, labels={})\n", .{ pos, @tagName(inst), c_ip, stack.values.items.len, stack.labels.items.len });
        var label_target: ?u32 = null;
        switch (inst) {
            .drop => {
                _ = try stack.pop();
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                try stack.push(.{ .i32 = val });
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                try stack.push(.{ .i64 = val });
            },
            .f32_const => {
                const val = try read.readf(r, f32);
                try stack.push(.{ .f32 = val });
            },
            .f64_const => {
                const val = try read.readf(r, f64);
                try stack.push(.{ .f64 = val });
            },
            .i32_eqz => {
                const dst = try stack.top();
                dst.i32 = if (dst.i32 == 0) 1 else 0;
            },
            .i64_eqz => {
                const dst = try stack.top();
                dst.i32 = if (dst.i64 == 0) 1 else 0;
            },
            .local_get => {
                const idx = try readu(r);
                try stack.push(locals[idx]);
            },
            .local_set => {
                const idx = try readu(r);
                const val = try stack.pop();
                locals[idx] = val;
            },
            .local_tee => {
                const idx = try readu(r);
                const val = try stack.top();
                locals[idx] = val.*;
            },
            .global_get => {
                const idx = try readu(r);
                try stack.push(in.globals[idx]);
            },
            .global_set => {
                const idx = try readu(r);
                const val = try stack.pop();
                in.globals[idx] = val;
            },
            .loop => {
                c_ip += 1;
                _ = try read.blocktype(r);
                // target: right after "loop"
                try stack.push_label(c_ip, 0);
            },
            .br => {
                label_target = try readu(r);
            },
            .br_if => {
                const idx = try readu(r);
                if (idx + 1 > stack.labels.items.len) return error.RuntimeError;
                const val = try stack.pop();
                if (val.i32 != 0) {
                    label_target = idx;
                }
            },
            .br_table => {
                const val = try stack.pop();
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
                try stack.push_label(control[c_ip].jmp_t, n_results);
            },
            .if_ => {
                c_ip += 1;
                const typ = try read.blocktype(r);
                const n_results = try typ.results();
                if (control[c_ip].off != pos) @panic("TREMBLING FEAR");
                const val = try stack.pop();
                if (val.i32 != 0) {
                    try stack.push_label(control[c_ip].jmp_t, n_results); // we worry about else vs end when we get there..
                } else {
                    c_ip = control[c_ip].jmp_t;
                    r.context.pos = control[c_ip].off;
                    const c_inst: defs.OpCode = @enumFromInt(try r.readByte());
                    if (c_inst == .else_) {
                        try stack.push_label(control[c_ip].jmp_t, n_results);
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
                // TODO: MYSKO
                _ = stack.labels.popOrNull() orelse break;
            },
            .end => {
                c_ip += 1;
                if (control[c_ip].off != pos) @panic("PANIKED FEAR");
                _ = stack.labels.popOrNull() orelse @panic("RUSHED FEAR");
                // todo: cannot do this if we popped a "loop" header
                // if (value_stack.items.len != item.stack_level + item.n_vals) @panic("SAD FEAR");
                if (stack.labels.items.len == 0) {
                    if (stack.values.items.len != self.n_ret) return error.RuntimeError;
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
                        const dst = try stack.top();
                        dst.i32 = @field(ops.iunop, name[4..])(i32, dst.i32);
                    },
                    .i32_binop => {
                        const dst, const src = try stack.pop_binop();
                        dst.i32 = try @field(ops.ibinop, name[4..])(i32, dst.i32, src.i32);
                    },
                    .i32_relop => {
                        const dst, const src = try stack.pop_binop();
                        dst.i32 = if (@field(ops.irelop, name[4..])(i32, dst.i32, src.i32)) 1 else 0;
                    },
                    .i64_unop => {
                        const dst = try stack.top();
                        dst.i64 = @field(ops.iunop, name[4..])(i64, dst.i64);
                    },
                    .i64_binop => {
                        const dst, const src = try stack.pop_binop();
                        dst.i64 = try @field(ops.ibinop, name[4..])(i64, dst.i64, src.i64);
                    },
                    .i64_relop => {
                        const dst, const src = try stack.pop_binop();
                        dst.i32 = if (@field(ops.irelop, name[4..])(i64, dst.i64, src.i64)) 1 else 0;
                    },
                    .f32_unop => {
                        const dst = try stack.top();
                        dst.f32 = try @field(ops.funop, name[4..])(f32, dst.f32);
                    },
                    .f32_binop => {
                        const dst, const src = try stack.pop_binop();
                        dst.f32 = try @field(ops.fbinop, name[4..])(f32, dst.f32, src.f32);
                    },
                    .f64_unop => {
                        const dst = try stack.top();
                        dst.f64 = try @field(ops.funop, name[4..])(f64, dst.f64);
                    },
                    .f64_binop => {
                        const dst, const src = try stack.pop_binop();
                        dst.f64 = try @field(ops.fbinop, name[4..])(f64, dst.f64, src.f64);
                    },
                    .convert => {
                        const dst = try stack.top();
                        dst.* = try @field(ops.convert, name)(dst.*);
                    },
                    .load => {
                        const alignas = try readu(r);
                        _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                        const offset = try readu(r);
                        const dst = try stack.top();
                        const ea = @as(u32, @bitCast(dst.i32)) + offset;
                        const memtype = defs.memtype(tag);
                        if (ea + @sizeOf(memtype) >= in.mem.items.len) return error.WasmTRAP;
                        var foo: memtype = undefined;
                        @memcpy(std.mem.asBytes(&foo), in.mem.items[ea..][0..@sizeOf(memtype)]);
                        @field(dst, name[0..3]) = foo;
                    },
                    .store => {
                        const alignas = try readu(r);
                        _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                        const offset = try readu(r);
                        const val = try stack.pop();
                        const dst = try stack.pop();
                        const ea = @as(u32, @bitCast(dst.i32)) + offset;
                        const memtype = defs.memtype(tag);
                        if (ea + @sizeOf(memtype) >= in.mem.items.len) return error.WasmTRAP;
                        const src = @field(val, name[0..3]);
                        const foo: memtype = if (@typeInfo(memtype) == .int) @truncate(src) else src;
                        @memcpy(in.mem.items[ea..][0..@sizeOf(memtype)], std.mem.asBytes(&foo));
                    },
                    .other => {
                        severe("{}: {s}\n", .{ pos, @tagName(inst) });
                        return error.NotImplemented;
                    },
                }
            },
        }
        if (label_target) |idx| {
            c_ip = try stack.pop_and_jump_labels(idx);
            r.context.pos = control[c_ip].off;

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
    if (stack.values.items.len < self.n_ret) return error.RuntimeError;

    return if (self.n_ret > 0) stack.values.items[stack.values.items.len - 1] else .{ .i32 = 0x4EADBEAF };
}
