// NB: this is likely not the final design but good enough to satisfy the spec
// (table of values to import from)

const std = @import("std");
const defs = @import("./defs.zig");
const ImportTable = @This();

allocator: std.mem.Allocator,
// caller owns all key/item values, allocator is just for the hashmap arrays
globals: std.StringHashMapUnmanaged(GlobalImport),
funcs: std.StringHashMapUnmanaged(defs.HostFunc),

// bluff och båg: ONE function table
func_table_size: u32 = 0,

const GlobalImport = struct {
    ref: *defs.StackValue,
    typ: defs.ValType,
};

pub fn init(allocator: std.mem.Allocator) ImportTable {
    return .{
        .allocator = allocator,
        .globals = .{},
        .funcs = .{},
    };
}

pub fn deinit(self: *ImportTable) void {
    self.globals.deinit(self.allocator);
    self.funcs.deinit(self.allocator);
}

// Note: "ref" must point to a full StackValue. Full 64-bits will be overwritten even if the type is 32-bit internally
pub fn add_global(self: *ImportTable, name: []const u8, ref: *defs.StackValue, typ: defs.ValType) !void {
    try self.globals.put(self.allocator, name, .{ .ref = ref, .typ = typ });
}

// Note: "ref" must point to a full StackValue. Full 64-bits will be overwritten even if the type is 32-bit internally
pub fn add_func(self: *ImportTable, name: []const u8, def: defs.HostFunc) !void {
    try self.funcs.put(self.allocator, name, def);
}
