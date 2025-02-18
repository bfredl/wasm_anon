// NB: this is likely not the final design but good enough to satisfy the spec
// (table of values to import from)

const std = @import("std");
const defs = @import("./defs.zig");
const ImportTable = @This();

allocator: std.mem.Allocator,
// caller owns all key/item values, allocator is just for the hashmap arrays
globals: std.StringHashMapUnmanaged(GlobalImport),

const GlobalImport = struct {
    ref: *defs.StackValue,
    typ: defs.ValType,
};

pub fn init(allocator: std.mem.Allocator) ImportTable {
    return .{
        .allocator = allocator,
        .globals = .{},
    };
}

// Note: "ref" must point to a full StackValue. Full 64-bits will be overwritten even if the type is 32-bit internally
pub fn addGlobal(self: *ImportTable, name: []const u8, ref: *defs.StackValue, typ: defs.ValType) !void {
    try self.globals.put(self.allocator, name, .{ .ref = ref, .typ = typ });
}
