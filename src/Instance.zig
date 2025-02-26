const std = @import("std");
const Instance = @This();
const Module = @import("./Module.zig");
const defs = @import("./defs.zig");
const Function = @import("./Function.zig");
const Interpreter = @import("./Interpreter.zig");
const ImportTable = @import("./ImportTable.zig");
const severe = std.debug.print;

mod: *Module,
mem: std.ArrayList(u8),
globals_maybe_indir: []defs.StackValue,
funcs_imported: []defs.HostFunc,

pub fn get_global(self: *Instance, idx: u32) *defs.StackValue {
    const g = &self.globals_maybe_indir[idx];
    return if (idx < self.mod.n_globals_import) g.indir else g;
}

pub fn init(mod: *Module, imports: ?ImportTable) !Instance {
    var self = Instance{
        .mod = mod,
        .mem = .init(mod.allocator),
        .globals_maybe_indir = try mod.allocator.alloc(defs.StackValue, mod.n_globals_import + mod.n_globals_internal),
        .funcs_imported = try mod.allocator.alloc(defs.HostFunc, mod.n_funcs_import),
    };

    if (mod.mem_limits.min > 0) {
        try self.mem.appendNTimes(0, 0x10000 * mod.mem_limits.min);
    }
    try mod.init_imports(&self, imports);
    try mod.init_data(self.mem.items, self.preglobals());
    return self;
}

pub fn deinit(self: *Instance) void {
    self.mem.deinit();
    self.mod.allocator.free(self.globals_maybe_indir);
    self.mod.allocator.free(self.funcs_imported);
}

pub fn preglobals(self: *Instance) []const defs.StackValue {
    return self.globals_maybe_indir[0..self.mod.n_globals_import];
}

pub fn execute(self: *Instance, idx: u32, args: []const defs.StackValue, ret: []defs.StackValue) !u32 {
    if (idx < self.mod.n_funcs_import or idx >= self.mod.n_imports + self.mod.funcs_internal.len) return error.OutOfRange;
    const func = &self.mod.funcs_internal[idx - self.mod.n_funcs_import];

    var stack: Interpreter = .init(self.mod.allocator);
    defer stack.deinit();
    return stack.execute(func, self.mod, self, args, ret);
}
