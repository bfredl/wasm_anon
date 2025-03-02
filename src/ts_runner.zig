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

    var imports: wasm_shelf.ImportTable = .init(allocator);
    defer imports.deinit();

    var in = try Instance.init(&mod, &imports);
    defer in.deinit();
}
