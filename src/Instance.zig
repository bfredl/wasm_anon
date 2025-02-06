const std = @import("std");
const Instance = @This();
const Module = @import("./Module.zig");
const defs = @import("./defs.zig");
const Function = @import("./Function.zig");
const Interpreter = @import("./Interpreter.zig");

mod: *Module,
mem: std.ArrayList(u8),
globals: []defs.StackValue,

pub fn init(mod: *Module) !Instance {
    var self = Instance{
        .mod = mod,
        .mem = .init(mod.allocator),
        .globals = try mod.allocator.alloc(defs.StackValue, mod.n_globals),
    };

    try mod.init_globals(self.globals);
    if (mod.mem_limits.min > 0) {
        try self.mem.appendNTimes(0, 0x10000 * mod.mem_limits.min);
    }
    try mod.init_data(self.mem.items);
    return self;
}

pub fn deinit(self: *Instance) void {
    self.mem.deinit();
    self.mod.allocator.free(self.globals);
}

pub fn execute(self: *Instance, idx: u32, args: []const defs.StackValue, ret: []defs.StackValue) !u32 {
    if (idx >= self.mod.funcs.len) return error.OutOfRange;
    const func = &self.mod.funcs[idx];

    var stack: Interpreter = .init(self.mod.allocator);
    defer stack.deinit();
    return stack.execute(func, self.mod, self, args, ret, false);
}
