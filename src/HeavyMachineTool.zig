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
longjmp_func: u32 = undefined,

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
    try self.build_longjmp();
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
    longjmp_f = try self.mod.get_func_ptr_id(self.longjmp_func, @TypeOf(longjmp_f));

    globalExceptionHandler();
}

pub var jmp_buf: JmpBuf = undefined;
pub var jmp_active: bool = false;
pub const JmpBuf = struct { [8]u64 };
pub var longjmp_f: *const fn (status: usize, buf: *const JmpBuf) callconv(.C) void = undefined;
const StackValue = defs.StackValue;
const TrampolineFn = *const fn (mem: [*]u8, mem_size: usize, params: [*]const StackValue, ret: [*]StackValue, jmp_buf: *JmpBuf) callconv(.C) u32;
pub fn execute(self: *HeavyMachineTool, in: *Instance, idx: u32, params: []const StackValue, ret: []StackValue, logga: bool, err_ret: ?*?[]const u8) !u32 {
    if (idx < in.mod.n_funcs_import or idx >= in.mod.n_imports + in.mod.funcs_internal.len) return error.OutOfRange;
    const func = &in.mod.funcs_internal[idx - in.mod.n_funcs_import];
    _ = logga;
    if (func.hmt_error) |err| {
        if (err_ret) |ptr| {
            ptr.* = err;
        } else {
            dbg("ERROR: {s}\n", .{err});
        }
    }

    const trampoline_obj = func.hmt_trampoline orelse return error.NotImplemented;

    const f = try self.mod.get_func_ptr_id(trampoline_obj, TrampolineFn);
    jmp_active = true;
    // std.debug.print("info jmp buf: {x}={}\ntrampolin: {x}={}\n", .{ @intFromPtr(&jmp_buf), @intFromPtr(&jmp_buf), @intFromPtr(f), @intFromPtr(f) });
    // asm volatile ("int3");
    const status = f(in.mem.items.ptr, in.mem.items.len, params.ptr, ret.ptr, &jmp_buf);
    jmp_active = false;
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

pub fn globalExceptionHandler() void {
    const posix = std.posix;
    var act = posix.Sigaction{
        .handler = .{ .sigaction = sigHandler },
        .mask = posix.sigemptyset(),
        // don't mask out signal, as we are going to longjmp out of the handler
        .flags = (posix.SA.SIGINFO | posix.SA.RESTART | posix.SA.NODEFER),
    };

    posix.sigaction(posix.SIG.FPE, &act, null);
}

fn build_longjmp(self: *HeavyMachineTool) !void {
    self.longjmp_func = @intCast(self.mod.objs.items.len);
    const longjmp_target = self.mod.code.get_target();
    try self.mod.objs.append(.{ .obj = .{ .func = .{ .code_start = longjmp_target } }, .name = null });

    var cfo = X86Asm{ .code = &self.mod.code, .long_jump_mode = true };
    try cfo.mov(true, .rax, .rdi); // status = arg1
    const b = X86Asm.a(.rsi); // jmp_buf = arg2
    try cfo.movrm(true, .rbx, b);
    try cfo.movrm(true, .rbp, b.o(8));
    try cfo.movrm(true, .r12, b.o(16));
    try cfo.movrm(true, .r13, b.o(24));
    try cfo.movrm(true, .r14, b.o(32));
    try cfo.movrm(true, .r15, b.o(40));
    try cfo.movrm(true, .rsp, b.o(48)); // NOTE: as inline we don't need to adjust
    try cfo.jmpi_m(b.o(56));
}

fn sigHandler(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*const anyopaque) callconv(.C) void {
    _ = sig;
    _ = info;
    _ = ctx_ptr;
    if (jmp_active) {
        longjmp_f(7, &jmp_buf);
    } else {
        std.debug.print("very stupid!\n", .{});
        std.posix.exit(11);
    }
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

    // There might be trapping exits. later on these should be implemented like setjmp/longjmp
    // but for now..
    const exit_node = try ir.addNode();
    if (f.n_res != 1) return error.NotImplemented;
    const exit_var = try ir.variable(.{ .intptr = .dword });
    try ir.ret(exit_node, .{ .intptr = .dword }, exit_var);

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
                const cmpop: FLIR.IntCond = .neq;
                _ = try ir.icmp(node, .dword, cmpop, lhs, rhs);
                try ir.addLink(node, 1, target.ir_target); // branch taken
                node = try ir.addNodeAfter(node);
            },
            .end => {
                if (label_stack.pop()) |label| {
                    _ = label; // in case of a blk, patch up!
                } else {
                    if (f.n_res != 1) return error.Notimplemented;
                    if (value_stack.items.len != 1) return error.InternalCompilerError;
                    try ir.putvar(node, exit_var, value_stack.items[0]);
                    try ir.addLink(node, 0, exit_node);
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
                            .i32_add => .add,
                            .i32_sub => .sub,
                            .i32_mul => .mul,
                            .i32_div_s => .sdiv,
                            .i32_div_u => .udiv,
                            .i32_and => .@"and",
                            .i32_or => .@"or",
                            .i32_xor => .xor,
                            .i32_shl => .shl,
                            .i32_shr_s => .shr,
                            .i32_shr_u => .sar,
                            else => {
                                dbg("inst {s} as {s} TBD, aborting!\n", .{ @tagName(tag), @tagName(category) });
                                return error.NotImplemented;
                            },
                        };
                        // fail attempt, we handle SIGFPE instead
                        if (false and (flir_op == .sdiv or flir_op == .udiv)) {
                            _ = try ir.icmp(node, .dword, .eq, rhs, try ir.const_uint(0));
                            const exception_node = try ir.addNode();
                            try ir.addLink(node, 1, exception_node); // branch taken
                            node = try ir.addNodeAfter(node);

                            // TODO: bulll
                            try ir.putvar(exception_node, exit_var, try ir.const_uint(77));
                            try ir.addLink(exception_node, 0, exit_node); // branch taken
                        }
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

    var cfo = X86Asm{ .code = &self.mod.code, .long_jump_mode = true };
    const frame = true;

    // TODO: this could be after/shared/whatever?
    const error_exit_target = self.mod.code.get_target();
    try cfo.movri(false, .rax, 1); // TODO: do not. rax is the longjmp return code anyway
    if (frame) try cfo.leave();
    try cfo.ret();

    const trampolin_target = self.mod.code.get_target();
    // trampolin,  *const fn (mem: [*]u8, mem_size: usize, params: [*]const StackValue, ret: [*]StackValue, jmp_buf: *JmpBpub f) u32;
    // arg 1: RDI = mem
    // arg2 : RSI = mem_size
    // arg3 : RDX = params
    // arg4: RCX = ret
    // arg5: R8 = jmp_buf
    // try cfo.trap();
    if (frame) try cfo.enter();
    if (ret_type) |_| try cfo.push(.rcx);

    // inline setjmp
    const b = X86Asm.a(.r8);
    try cfo.movmr(true, b, .rbx);
    try cfo.movmr(true, b.o(8), .rbp);
    try cfo.movmr(true, b.o(16), .r12);
    try cfo.movmr(true, b.o(24), .r13);
    try cfo.movmr(true, b.o(32), .r14);
    try cfo.movmr(true, b.o(40), .r15);
    try cfo.movmr(true, b.o(48), .rsp); // NOTE: as inline we don't need to adjust
    try cfo.lea(.rax, X86Asm.rel(error_exit_target));
    try cfo.movmr(true, b.o(56), .rax);

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
    try cfo.zero(.rax); // non-error exit
    // as a silly trick, setjmp target here? nice for debugging
    if (frame) try cfo.leave();
    try cfo.ret();

    f.hmt_trampoline = @intCast(self.mod.objs.items.len);
    try self.mod.objs.append(.{ .obj = .{ .func = .{ .code_start = trampolin_target } }, .name = null });
}
