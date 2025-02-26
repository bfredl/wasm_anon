const std = @import("std");
const defs = @import("./defs.zig");
const ops = @import("./ops.zig");
const Module = @import("./Module.zig");
const Instance = @import("./Instance.zig");
const Function = @import("./Function.zig");
const severe = std.debug.print;
const dbg = Module.nodbg;
const ControlItem = Function.ControlItem;

const read = @import("./read.zig");
const readLeb = read.readLeb;
const readu = read.readu;
const Reader = read.Reader;
const Interpreter = @This();

fn u(val: i32) u32 {
    return @bitCast(val);
}
const StackValue = defs.StackValue;

pub const StackLabel = struct {
    c_ip: u32,
    n_vals: u16,
    stack_level: u32,
};

pub const StackFrame = struct {
    r_ip: u32,
    c_ip: u32,
    frame_label: u32 = 0,
    locals_ptr: u32 = 0,
    frame_ptr: u32 = 0,
    func: *Function,
};

values: std.ArrayList(StackValue),
labels: std.ArrayList(StackLabel),
frames: std.ArrayList(StackFrame),

locals_ptr: u32 = 0,
frame_ptr: u32 = 0,

frame_label: u32 = 0,

pub fn init(allocator: std.mem.Allocator) Interpreter {
    return .{
        .values = .init(allocator),
        .labels = .init(allocator),
        .frames = .init(allocator),
    };
}

pub fn deinit(self: *Interpreter) void {
    self.values.deinit();
    self.labels.deinit();
    self.frames.deinit();
}

pub fn nvals(self: *Interpreter) u32 {
    return @intCast(self.values.items.len - self.frame_ptr);
}

pub fn push(self: *Interpreter, value: StackValue) !void {
    try self.values.append(value);
}

pub fn push_multiple(self: *Interpreter, values: []const StackValue) !void {
    try self.values.appendSlice(values);
}

pub fn pop(self: *Interpreter) !StackValue {
    if (self.values.items.len == self.frame_ptr) return error.RuntimeError;
    return self.values.pop() orelse return error.RuntimeError;
}

pub fn push_label(self: *Interpreter, c_ip: u32, n_vals: u16) !void {
    try self.labels.append(.{ .c_ip = c_ip, .stack_level = @intCast(self.values.items.len), .n_vals = n_vals });
}

pub fn push_frame(self: *Interpreter, r_ip: u32, c_ip: u32, func: *Function) !void {
    try self.frames.append(.{ .r_ip = r_ip, .c_ip = c_ip, .func = func, .locals_ptr = self.locals_ptr, .frame_ptr = self.frame_ptr, .frame_label = self.frame_label });
}

pub fn top(self: *Interpreter) !*StackValue {
    const stack = &self.values;
    if (stack.items.len < 1) return error.RuntimeError;
    return &stack.items[stack.items.len - 1];
}

pub fn pop_binop(self: *Interpreter) !struct { *StackValue, StackValue } {
    const stack = &self.values;
    if (self.nvals() < 2) return error.RuntimeError;
    const src = stack.pop().?;
    return .{ &stack.items[stack.items.len - 1], src };
}

pub fn pop_and_jump_labels(self: *Interpreter, levels: u32) !u32 {
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

pub fn enter_frame(self: *Interpreter) void {
    self.frame_ptr = @intCast(self.values.items.len);
    self.frame_label = @intCast(self.labels.items.len);
}

pub fn local(self: *Interpreter, idx: u32) *StackValue {
    if (self.locals_ptr + idx >= self.frame_ptr) @panic("monkaS FEAR");
    return &self.values.items[self.locals_ptr + idx];
}

pub fn init_locals(stack: *Interpreter, r: Reader) !void {
    const n_local_defs = try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        const init_val: StackValue = StackValue.default(typ) orelse return error.InvalidFormat;
        for (0..n_decl) |_| try stack.push(init_val);
    }
}

// TODO: this should be made flexible enough to allow ie a nested callback from a a host function
// TODO: use stack.values to pass args/ret ?
pub fn execute(stack: *Interpreter, self: *Function, mod: *Module, in: *Instance, params: []const StackValue, ret: []StackValue) !u32 {
    const control = try self.ensure_parsed(mod);

    // NB: in the spec all locals are bundled into a "frame" object as a single
    // entry on the stack. We do a little unbundling to keep stack object sizes
    // pretty much homogenous.

    stack.locals_ptr = @intCast(stack.values.items.len);
    if (params.len != self.n_params or ret.len < self.n_res) return error.InvalidArgument;
    try stack.push_multiple(params);
    // fbs.pos is the insruction pointer which is a bit weird but works
    var fbs = mod.fbs_at(self.codeoff);
    const r = fbs.reader();

    try stack.init_locals(r);
    stack.enter_frame();
    // entire body is implicitly a block, producing the return values
    try stack.push_label(@intCast(control.len - 1), @intCast(self.n_res));

    try stack.run_vm(in, r, self);
    if (stack.nvals() < self.n_res) return error.RuntimeError;

    @memcpy(ret[0..self.n_res], stack.values.items[stack.values.items.len - self.n_res ..]);
    return self.n_res;
}

fn run_vm(stack: *Interpreter, in: *Instance, r: Reader, entry_func: *Function) !void {
    var c_ip: u32 = 0;
    var func = entry_func;
    var control = func.control.?;
    const mod = in.mod;

    while (true) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        dbg("{x:04}: {s} (c={}, values={}, labels={})\n", .{ pos, @tagName(inst), c_ip, stack.values.items.len, stack.labels.items.len });
        var label_target: ?u32 = null;
        switch (inst) {
            .unreachable_ => {
                return error.WASMTrap;
            },
            .nop => {},
            .drop => {
                _ = try stack.pop();
            },
            .select, .select_t => {
                if (inst == .select_t) {
                    const num = try readu(r);
                    if (num != 1) return error.InvalidFormat; // possible extension
                    const typ: defs.ValType = @enumFromInt(try r.readByte());
                    _ = typ;
                }
                const pred = try stack.pop();
                const val1, const val2 = try stack.pop_binop();
                if (pred.i32 == 0) val1.* = val2;
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
                // TODO: they dun guufed value semantics if this was inline
                // or even `const val = stack.local(idx).*;`
                //
                // NICE JOB ZIG CORE DEVS
                var val: StackValue = undefined;
                val = stack.local(idx).*;
                try stack.push(val);
            },
            .local_set => {
                const idx = try readu(r);
                const val = try stack.pop();
                stack.local(idx).* = val;
            },
            .local_tee => {
                const idx = try readu(r);
                const val = try stack.top();
                stack.local(idx).* = val.*;
            },
            .global_get => {
                const idx = try readu(r);
                try stack.push(in.get_global(idx).*);
            },
            .global_set => {
                const idx = try readu(r);
                const val = try stack.pop();
                in.get_global(idx).* = val;
            },
            .memory_grow => {
                if (try r.readByte() != 0) return error.InvalidFormat;
                const size_res = try stack.top();
                const oldsize = in.mem.items.len;
                // TODO: validate total size doesn't exceed mod.mem_limits.max, or addressable size limit (2**32 ?)
                try in.mem.appendNTimes(0, 0x10000 * @as(usize, @intCast(size_res.i32)));
                size_res.i32 = @intCast(oldsize / 0x10000);
            },
            .memory_size => {
                if (try r.readByte() != 0) return error.InvalidFormat;
                const size: i32 = @intCast(in.mem.items.len / 0x10000);
                try stack.push(.{ .i32 = size });
            },
            .loop => {
                c_ip += 1;
                const typ = try read.blocktype(r);
                const n_args, const n_results = try typ.arity(mod);
                _ = n_results;
                // target: right after "loop"
                try stack.push_label(c_ip, n_args);
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
                const n_args, const n_results = try typ.arity(mod);
                if (n_args > 0) return error.NotImplemented;
                // target: right after "loop"
                if (control[c_ip].off != pos) @panic("MANIC FEAR");
                try stack.push_label(control[c_ip].jmp_t, n_results);
            },
            .if_ => {
                c_ip += 1;
                const typ = try read.blocktype(r);
                const n_args, const n_results = try typ.arity(mod);
                if (n_args > 0) return error.NotImplemented;
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
                _ = stack.labels.pop() orelse break;
            },
            .ret => {
                // TODO: a bit dubbel, make label_target just be the destination?
                label_target = @intCast(stack.labels.items.len - 1 - stack.frame_label);
            },
            .call, .call_indirect => {
                const idx, const chktyp = if (inst == .call_indirect) funcidx: {
                    const tblidx = try readLeb(r, u32);
                    _ = tblidx; // clown face emoji
                    const typidx = try readLeb(r, u32);
                    const eidx: u32 = @bitCast((try stack.pop()).i32);
                    if (eidx >= mod.funcref_table.len) return error.WASMTrap;
                    const funcidx = mod.funcref_table[eidx];
                    break :funcidx .{ funcidx, typidx };
                } else .{ try readLeb(r, u32), null };

                if (idx >= mod.n_funcs_import + mod.funcs_internal.len) @panic("SHAKING FEAR");
                if (idx < mod.n_funcs_import) {
                    const f = in.funcs_imported[idx];
                    const level = stack.values.items.len - f.n_args;
                    if (f.n_res > f.n_args) try stack.values.appendNTimes(.{ .i32 = 0x7001BEEF }, f.n_res - f.n_args);
                    try f.cb(stack.values.items[level..], in, f.data);
                    if (f.n_args > f.n_res) stack.values.items.len = level + f.n_res;
                    break;
                }

                const called = &mod.funcs_internal[idx - mod.n_funcs_import];
                if (chktyp) |typidx| {
                    if (typidx != called.typeidx) return error.WASMTrap;
                }

                const called_control = try called.ensure_parsed(mod);
                if (stack.nvals() < called.n_params) {
                    return error.RuntimeError;
                }

                // save current state as a frame
                // note: calls don't increment c_ip. If they were changed to do, r_ip would be redundant
                try stack.push_frame(@intCast(r.context.pos), c_ip, func);

                // enter new function
                stack.locals_ptr = @intCast(stack.values.items.len - called.n_params);
                r.context.pos = called.codeoff;
                try init_locals(stack, r);
                func = called;
                control = called_control;
                stack.enter_frame();
                // entire body is implicitly a block, producing the return values
                try stack.push_label(@intCast(control.len - 1), @intCast(func.n_res));
                c_ip = 0;
            },
            .end => {
                c_ip += 1;
                _ = stack.labels.pop() orelse @panic("RUSHED FEAR");
                // todo: cannot do this if we popped a "loop" header
                // if (value_stack.items.len != item.stack_level + item.n_vals) @panic("SAD FEAR");
                if (stack.labels.items.len == stack.frame_label) {
                    if (stack.frames.pop()) |f| {
                        const returned = func;
                        if (returned.n_res > 0) {
                            // these can end up overlapping
                            std.mem.copyForwards(
                                StackValue,
                                stack.values.items[stack.locals_ptr..][0..returned.n_res],
                                stack.values.items[stack.values.items.len - returned.n_res ..],
                            );
                        }
                        func = f.func;
                        control = func.control.?;
                        if (stack.values.items.len < stack.locals_ptr) @panic("ayyooooo");
                        stack.values.items.len = stack.locals_ptr + returned.n_res;
                        stack.frame_ptr = f.frame_ptr;
                        stack.locals_ptr = f.locals_ptr;
                        stack.frame_label = f.frame_label;

                        c_ip = f.c_ip;
                        r.context.pos = f.r_ip;
                    } else {
                        // top-level invoked function
                        return;
                    }
                } else {
                    if (c_ip >= control.len or control[c_ip].off != pos) @panic("PANIKED FEAR");
                }
                // TODO
                // if (stack.nvals() != self.n_res) return error.RuntimeError;
            },
            .prefixed => {
                const code: defs.Prefixed = try read.prefix(r);
                switch (code) {
                    .memory_fill => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        const n: u32 = @bitCast((try stack.pop()).i32);
                        const val: u32 = @bitCast((try stack.pop()).i32);
                        const d: u32 = @bitCast((try stack.pop()).i32);
                        const m = in.mem.items;
                        const truncval: u8 = @truncate(val);
                        if (@as(u64, d) + n > m.len) return error.WASMTrap;
                        @memset(m[d..][0..n], truncval);
                    },
                    .memory_copy => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        const n: u32 = @bitCast((try stack.pop()).i32);
                        const s: u32 = @bitCast((try stack.pop()).i32);
                        const d: u32 = @bitCast((try stack.pop()).i32);
                        const m = in.mem.items;
                        if (@as(u64, s) + n > m.len or @as(u64, d) + n > m.len) return error.WASMTrap;
                        if (d <= s) {
                            std.mem.copyForwards(u8, m[d..][0..n], m[s..][0..n]);
                        } else {
                            std.mem.copyBackwards(u8, m[d..][0..n], m[s..][0..n]);
                        }
                    },
                    inline else => |c| {
                        if (@intFromEnum(c) < 8) {
                            const dst = try stack.top();
                            dst.* = try @field(ops.convert, @tagName(c))(dst.*);
                        } else {
                            // TODO: @tagname(foo) but "_" safe?
                            severe("not implemented: prefixed:{s}, aborting!\n", .{@tagName(code)});
                            return error.NotImplemented;
                        }
                    },
                }
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
                    .f32_relop => {
                        const dst, const src = try stack.pop_binop();
                        dst.i32 = if (@field(ops.frelop, name[4..])(f32, dst.f32, src.f32)) 1 else 0;
                    },
                    .f64_unop => {
                        const dst = try stack.top();
                        dst.f64 = try @field(ops.funop, name[4..])(f64, dst.f64);
                    },
                    .f64_binop => {
                        const dst, const src = try stack.pop_binop();
                        dst.f64 = try @field(ops.fbinop, name[4..])(f64, dst.f64, src.f64);
                    },
                    .f64_relop => {
                        const dst, const src = try stack.pop_binop();
                        dst.i32 = if (@field(ops.frelop, name[4..])(f64, dst.f64, src.f64)) 1 else 0;
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
                        if (ea + @sizeOf(memtype) > in.mem.items.len) return error.WASMTrap;
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
                        if (ea + @sizeOf(memtype) > in.mem.items.len) return error.WASMTrap;
                        const src = @field(val, name[0..3]);
                        const foo: memtype = if (@typeInfo(memtype) == .int) @truncate(src) else src;
                        @memcpy(in.mem.items[ea..][0..@sizeOf(memtype)], std.mem.asBytes(&foo));
                    },
                    .other => {
                        severe("{}: not implemented: {s}\n", .{ pos, @tagName(inst) });
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
        if (c_ip == control.len) {
            @panic("allllllll");
        }
    }
}

pub fn eval_constant_expr(r: Reader, typ: defs.ValType, preglobals: []const StackValue) !StackValue {
    // shortcut: `expr` is just "i??.const VAL end"
    const eval = try r.readByte();
    const init_typ: defs.OpCode = @enumFromInt(eval);
    _ = typ;
    const val: StackValue = val: switch (init_typ) {
        .i32_const => .{ .i32 = try read.readLeb(r, i32) },
        .i64_const => .{ .i64 = try read.readLeb(r, i64) },
        .f32_const => .{ .f32 = try read.readf(r, f32) },
        .f64_const => .{ .f64 = try read.readf(r, f64) },
        .ref_null => {
            _ = try r.readByte();
            break :val .{ .ref = 0 };
        },
        .global_get => {
            const idx = try readu(r);
            if (idx >= preglobals.len) return error.InvalidFormat;
            break :val preglobals[idx].indir.*;
        },
        else => return error.InvalidFormat,
    };
    if (try r.readByte() != 0x0b) return error.InvalidFormat;
    return val;
}
