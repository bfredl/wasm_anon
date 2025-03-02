const std = @import("std");
const dbg = std.debug.print;

const util = @import("./util.zig");
const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;
const Instance = wasm_shelf.Instance;

fn trap(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = args_ret;
    _ = data;
    _ = in;
    return error.WASMTrap;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const argv = std.os.argv;
    if (argv.len < 2) return dbg("module name?\n", .{});
    const filearg = std.mem.span(argv[1]);
    const buf = try util.readall(allocator, filearg);
    defer allocator.free(buf);

    var mod = try wasm_shelf.Module.parse(buf, allocator);
    defer mod.deinit();

    try mod.dbg_imports();
    try mod.dbg_exports();

    const dylink_info = try mod.get_dylink_info();
    if (dylink_info) |info| dbg("DYLING: {}\n", .{info});

    var imports: wasm_shelf.ImportTable = .init(allocator);
    defer imports.deinit();

    var stack_pointer: StackValue = .{ .i32 = 0 };
    var memory_base: StackValue = .{ .i32 = 0 };
    var table_base: StackValue = .{ .i32 = 0 };
    try imports.add_global("__stack_pointer", &stack_pointer, .i32);
    try imports.add_global("__memory_base", &memory_base, .i32);
    try imports.add_global("__table_base", &table_base, .i32);
    try imports.add_func("calloc", .{ .cb = &trap, .data = undefined, .n_args = 2, .n_res = 1 });
    try imports.add_func("towupper", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("iswspace", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("strlen", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("memcmp", .{ .cb = &trap, .data = undefined, .n_args = 3, .n_res = 1 });
    try imports.add_func("free", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 0 });
    try imports.add_func("realloc", .{ .cb = &trap, .data = undefined, .n_args = 2, .n_res = 1 });
    try imports.add_func("malloc", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });
    try imports.add_func("__assert_fail", .{ .cb = &trap, .data = undefined, .n_args = 4, .n_res = 0 });
    try imports.add_func("strncpy", .{ .cb = &trap, .data = undefined, .n_args = 3, .n_res = 1 });
    try imports.add_func("iswalnum", .{ .cb = &trap, .data = undefined, .n_args = 1, .n_res = 1 });

    var in = try Instance.init(&mod, &imports);
    defer in.deinit();
}
