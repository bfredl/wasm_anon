const std = @import("std");
const Instance = @This();
const Module = @import("./Module.zig");
const Function = @import("./Function.zig");

mod: *Module,
mem: std.ArrayList(u8),
globals: []Function.StackValue,

pub fn init(module: *Module) !Instance {
    return .{
        .mod = module,
        .mem = .init(module.allocator),
        .globals = try module.allocator.alloc(Function.StackValue, module.n_globals),
    };
}

pub fn deinit(self: *Instance) void {
    self.mem.deinit();
    self.mod.allocator.free(self.globals);
}

pub fn execute(self: *Instance, idx: u32, args: []const Function.StackValue) !Function.StackValue {
    if (idx >= self.mod.funcs.len) return error.OutOfRange;
    return self.mod.funcs[idx].execute(self.mod, self, args);
}
