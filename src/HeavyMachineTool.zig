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
    for (0.., mod.funcs_internal) |i, *f| {
        try self.compileFunc(in, i, f);
    }
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
            const init_val: defs.StackValue = defs.StackValue.default(typ) orelse return error.InvalidFormat;
            if (typ != .i32) return error.NotImplemented;
            for (0..n_decl) |_| {
                locals[i] = try ir.variable(.{ .intptr = .dword });
                try ir.putvar(node, locals[i], try ir.const_uint(init_val.u32()));
                i += 1;
            }
        }
    }

    defer ir.debug_print(); // show what we got when it ends

    var value_stack: std.ArrayList(u16) = .init(in.mod.allocator);
    defer value_stack.deinit();

    var label_stack: std.ArrayList(struct { c_ip: u32, ir_target: u16 }) = .init(in.mod.allocator);
    defer label_stack.deinit();

    while (true) {
        // const pos = r.pos;
        const inst = try r.readOpCode();
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
            .i32_mul, .i32_add => {
                const rhs = value_stack.pop().?;
                const lhs = value_stack.pop().?;
                const res = try ir.ibinop(node, .dword, if (inst == .i32_mul) .mul else .add, lhs, rhs);
                try value_stack.append(res);
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
            else => {
                dbg("inst {s} TBD, aborting!\n", .{@tagName(inst)});
                return error.NotImplemented;
            },
        }
    }
}
