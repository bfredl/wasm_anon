// NB: this is likely not the final design but good enough to satisfy the spec
// (table of values to import from)

const std = @import("std");
const defs = @import("./defs.zig");

allocator: std.mem.Allocator,
// caller owns all key/item values, allocator is just for the hashmap arrays
globals: std.StringHashMap(GlobalImport),

const GlobalImport = struct {
    ptr: *defs.StackValue,
    typ: defs.ValType,
};
