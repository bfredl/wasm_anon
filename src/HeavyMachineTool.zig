// AOT-style compiler (still in memory). This doesn't interact with Interpreter.zig at all
// but maps WASM stack frames directly to C-ABI stack frames. This will be used as a reference
// to map WASM to FLIR semantics and also to battle-test FLIR (as the IR is going to be quite
// larger than IR extracted from dynamic traces)

const forklift = @import("forklift");
const Instance = @import("./Instance.zig");
const Function = @import("./Function.zig");
const std = @import("std");
const X86Asm = forklift.X86Asm;
const IPReg = X86Asm.IPReg;
const FLIR = forklift.FLIR;
const dbg = std.debug.print;
const defs = @import("./defs.zig");

const HeavyMachineTool = @This();
flir: FLIR,
mod: forklift.CFOModule,

pub fn init(allocator: std.mem.Allocator) !HeavyMachineTool {
    return .{
        .mod = try .init(allocator),
        .flir = try .init(4, allocator),
    };
}

// why an Instance instead of a module? why not? why ask?
pub fn compileInstance(self: *HeavyMachineTool, in: *Instance) !void {
    const mod = in.mod;
    try mod.mark_exports(); // or already??
    for (0.., mod.funcs_internal) |i, *f| {
        self.compileFunc(in, i, f) catch |e| switch (e) {
            error.NotImplemented => {
                if (f.hmt_error == null) {
                    // fill in using error return trace?
                    f.hmt_error = "??UNKNOWN";
                }
                continue; // ok, note the error
            },
            else => return e,
        };
    }
    try X86Asm.dbg_nasm(&.{ .code = &self.mod.code }, in.mod.allocator);
    try self.mod.code.finalize();
}

const StackValue = defs.StackValue;
const TrampolineFn = *const fn (mem: [*]u8, mem_size: usize, params: [*]const StackValue, ret: [*]StackValue) callconv(.C) u32;
pub fn execute(self: *HeavyMachineTool, in: *Instance, idx: u32, params: []const StackValue, ret: []StackValue, logga: bool) !u32 {
    if (idx < in.mod.n_funcs_import or idx >= in.mod.n_imports + in.mod.funcs_internal.len) return error.OutOfRange;
    const func = &in.mod.funcs_internal[idx - in.mod.n_funcs_import];
    _ = logga;
    if (func.hmt_error) |err| {
        dbg("ERROR: {s}\n", .{err});
    }

    const trampoline_obj = func.hmt_trampoline orelse return error.NotImplemented;

    const f = try self.mod.get_func_ptr_id(trampoline_obj, TrampolineFn);
    const status = f(in.mem.items.ptr, in.mem.items.len, params.ptr, ret.ptr);
    if (status != 0) return error.WASMTrap;
    return func.n_res;
}

fn wide(typ: defs.ValType) !bool {
    return switch (typ) {
        .i32 => false,
        .i64 => true,
        else => error.NotImplemented,
    };
}

pub fn compileFunc(self: *HeavyMachineTool, in: *Instance, id: usize, f: *Function) !void {
    _ = id;
    const ir = &self.flir;
    const gpa = in.mod.allocator;
    _ = try f.ensure_parsed(in.mod);
    ir.reinit();
    var locals = try gpa.alloc(u16, f.local_types.len);
    var node = try ir.addNode();
    // I think FLIR can require all args to be first..
    for (0..f.n_params) |i| {
        locals[i] = try ir.arg();
    }
    if (f.args_mut != 0) {
        for (0..f.n_params) |i| {
            const mut = (f.args_mut & (@as(u64, 1) << @as(u6, @intCast((i & 63))))) != 0;
            if (mut) {
                const src = locals[i];
                locals[i] = try ir.variable(.{ .intptr = .dword });
                try ir.putvar(node, locals[i], src);
            }
        }
    }

    var c_ip: u32 = 0;
    var r = in.mod.reader_at(f.codeoff);

    {
        var i = f.n_params;
        const n_local_defs = try r.readu();
        for (0..n_local_defs) |_| {
            const n_decl = try r.readu();
            const typ: defs.ValType = @enumFromInt(try r.readByte());
            const init_val = StackValue.default(typ) orelse return error.InvalidFormat;
            if (typ != .i32) return error.NotImplemented;
            for (0..n_decl) |_| {
                locals[i] = try ir.variable(.{ .intptr = .dword });
                try ir.putvar(node, locals[i], try ir.const_uint(init_val.u32()));
                i += 1;
            }
        }
    }

    var value_stack: std.ArrayList(u16) = .init(in.mod.allocator);
    defer value_stack.deinit();

    var label_stack: std.ArrayList(struct { c_ip: u32, ir_target: u16 }) = .init(in.mod.allocator);
    defer label_stack.deinit();

    errdefer ir.debug_print(); // show what we got when it ends

    while (true) {
        // const pos = r.pos;
        const inst = try r.readOpCode();
        f.hmt_error = @tagName(inst); // NOT LIKE THIS
        switch (inst) {
            .i32_const => {
                const val = try r.readLeb(i32);
                try value_stack.append(try ir.const_uint(@bitCast(@as(i64, val))));
            },
            .local_set => {
                const idx = try r.readu();
                const src = value_stack.pop().?;
                try ir.putvar(node, locals[idx], src);
            },
            .local_tee => {
                const idx = try r.readu();
                const src = value_stack.items[value_stack.items.len - 1];
                try ir.putvar(node, locals[idx], src);
            },
            .local_get => {
                const idx = try r.readu();
                const val = try ir.read_ref(node, locals[idx]); // idempodent if locals[idx] is argument
                try value_stack.append(val);
            },
            .loop => {
                const typ = try r.blocktype();
                const n_args, const n_results = try typ.arity(in.mod);
                if (n_args != 0 or n_results != 0) return error.NotImplemented;
                c_ip += 1;
                const entry = try ir.addNodeAfter(node);
                node = entry;
                try label_stack.append(.{ .c_ip = c_ip, .ir_target = entry });
            },
            .i32_ne => {
                const peekinst: defs.OpCode = @enumFromInt(r.peekByte());
                if (peekinst != .br_if) return error.NotImplemented;
                _ = try r.readByte();
                const rhs = value_stack.pop().?;
                const lhs = value_stack.pop().?;
                const label = try r.readu();
                if (label > label_stack.items.len - 1) return error.InternalCompilerError;
                const target = label_stack.items[label_stack.items.len - label - 1];
                _ = try ir.icmp(node, .dword, .neq, lhs, rhs);
                try ir.addLink(node, 1, target.ir_target); // branch taken
                node = try ir.addNodeAfter(node);
            },
            .end => {
                if (label_stack.pop()) |label| {
                    _ = label; // in case of a blk, patch up!
                } else {
                    if (f.n_res != 1) return error.NotImplemented;
                    if (value_stack.items.len != 1) return error.InternalCompilerError;
                    try ir.ret(node, .{ .intptr = .dword }, value_stack.items[0]);
                    break;
                }
            },
            // TODO: this leads to some bloat - some things like binops could be done as a bulk
            inline else => |tag| {
                const category = comptime defs.category(tag);
                switch (category) {
                    .i32_binop => {
                        const rhs = value_stack.pop().?;
                        const lhs = value_stack.pop().?;
                        const flir_op: FLIR.IntBinOp = switch (tag) {
                            .i32_add => .mul,
                            .i32_sub => .sub,
                            .i32_mul => .mul,
                            // .i32_div_s => .div,
                            // .i32_div_u => .div,
                            else => {
                                dbg("inst {s} as {s} TBD, aborting!\n", .{ @tagName(tag), @tagName(category) });
                                return error.NotImplemented;
                            },
                        };
                        const res = try ir.ibinop(node, .dword, flir_op, lhs, rhs);
                        try value_stack.append(res);
                    },
                    else => |cat| {
                        dbg("inst {s} as {s} TBD, aborting!\n", .{ @tagName(tag), @tagName(cat) });
                        return error.NotImplemented;
                    },
                }
            },
        }
    }
    f.hmt_error = null; // NOT LIKE THIS
    ir.debug_print();

    try ir.test_analysis(FLIR.X86ABI, true);
    ir.print_intervals();
    ir.debug_print();

    // TODO: abstraction
    const target = try forklift.codegen_x86_64(ir, &self.mod.code, false);
    f.hmt_object = @intCast(self.mod.objs.items.len);
    try self.mod.objs.append(.{ .obj = .{ .func = .{ .code_start = target } }, .name = null });

    if (!f.exported) return;

    const max_args = 2;
    if (f.n_params > max_args) return error.NotImplemented;
    var local_types_buf: [max_args]defs.ValType = undefined;
    const ret_type = try in.mod.type_params(f.typeidx, &local_types_buf);
    const local_types = local_types_buf[0..f.n_params];

    const trampolin_target = self.mod.code.get_target();
    // trampolin,  *const fn (mem: [*]u8, mem_size: usize, params: [*]const StackValue, ret: [*]StackValue) u32;
    // arg 1: RDI = mem
    // arg2 : RSI = mem_size
    // arg3 : RDX = params
    // arg4: RCX = ret
    var cfo = X86Asm{ .code = &self.mod.code, .long_jump_mode = true };
    const frame = true;
    // try cfo.trap();
    if (frame) try cfo.enter();
    if (ret_type) |_| try cfo.push(.rcx);

    // TODO: offset with mem when we start using it
    const ireg: [max_args]IPReg = .{ .rdi, .rsi };

    for (local_types, 0..) |t, i| {
        const w = switch (t) {
            .i32 => false,
            .i64 => true,
            else => return error.NotImplemented,
        };
        try cfo.movrm(w, ireg[i], X86Asm.a(.rdx).o(@intCast(@sizeOf(defs.StackValue) * i)));
    }
    try cfo.call_rel(target);
    if (ret_type) |typ| {
        try cfo.pop(.rcx);
        const w = switch (typ) {
            .i32 => false,
            .i64 => true,
            else => return error.NotImplemented,
        };
        try cfo.movmr(w, X86Asm.a(.rcx), .rax); // only one
    }
    try cfo.zero(.rax); // TODO: error status
    if (frame) try cfo.leave();
    try cfo.ret();

    f.hmt_trampoline = @intCast(self.mod.objs.items.len);
    try self.mod.objs.append(.{ .obj = .{ .func = .{ .code_start = trampolin_target } }, .name = null });
}
