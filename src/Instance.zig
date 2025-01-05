const std = @import("std");
const Instance = @This();
const Module = @import("./Module.zig");
const defs = @import("./defs.zig");
const Function = @import("./Function.zig");

mod: *Module,
mem: std.ArrayList(u8),
globals: []defs.StackValue,

pub fn init(mod: *Module) !Instance {
    const self = Instance{
        .mod = mod,
        .mem = .init(mod.allocator),
        .globals = try mod.allocator.alloc(defs.StackValue, mod.n_globals),
    };

    try mod.init_globals(self.globals);
    return self;
}

pub fn deinit(self: *Instance) void {
    self.mem.deinit();
    self.mod.allocator.free(self.globals);
}

pub fn execute(self: *Instance, idx: u32, args: []const defs.StackValue) !defs.StackValue {
    if (idx >= self.mod.funcs.len) return error.OutOfRange;
    return self.mod.funcs[idx].execute(self.mod, self, args);
}
