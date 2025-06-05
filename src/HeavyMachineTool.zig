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

pub fn compileFunc(self: *HeavyMachineTool, in: *Instance, id: usize, f: *Function) !void {
    _ = id;
    const ir = &self.flir;
    const gpa = in.mod.allocator;
    _ = try f.ensure_parsed(in.mod);
    self.flir.reinit();
    var locals = try gpa.alloc(u32, f.local_types.len);
    for (0..f.n_params) |i| {
        locals[i] = try ir.arg();
    }
    ir.debug_print();
}
