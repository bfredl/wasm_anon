// AOT-style compiler (still in memory). This doesn't interact with Interpreter.zig at all
// but maps WASM stack frames directly to C-ABI stack frames. This will be used as a reference
// to map WASM to FLIR semantics and also to battle-test FLIR (as the IR is going to be quite
// larger than IR extracted from dynamic traces)

const forklift = @import("forklift");
const Instance = @import("./Instance.zig");
const std = @import("std");
const X86Asm = forklift.X86Asm;
const IPReg = X86Asm.IPReg;
const FLIR = forklift.FLIR;

const HeavyMachineTool = @This();
flir: FLIR,
mod: forklift.CFOModule,

fn init(allocator: std) HeavyMachineTool {
    return .{
        .mod = .init(allocator),
        .flir = .init(allocator, 4),
    };
}

// why an Instance instead of a module? why not? why ask?
fn compileInstance(self: *HeavyMachineTool, in: *Instance) !void {
    const mod = in.mod;
    _ = mod;
    _ = self;
}
