const std = @import("std");
const dbg = std.debug.print;

const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;
const Instance = wasm_shelf.Instance;

pub fn usage() void {
    dbg("Read the source code.\n", .{});
}

pub fn readall(allocator: std.mem.Allocator, filename: []u8) ![]u8 {
    const fil = try std.fs.cwd().openFile(filename, .{});
    const stat = try std.posix.fstat(fil.handle);
    const size = std.math.cast(usize, stat.size) orelse return error.FileTooBig;
    const buf = try allocator.alloc(u8, size);
    if (try fil.readAll(buf) < size) {
        return error.IOError;
    }
    return buf;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const argv = std.os.argv;
    if (argv.len < 2) return usage();
    const filearg = std.mem.span(argv[1]);
    const buf = try readall(allocator, filearg);
    defer allocator.free(buf);

    var mod = try wasm_shelf.Module.parse(buf, allocator);
    defer mod.deinit();

    if (argv.len == 2) {
        try mod.dbg_imports();
        try mod.dbg_exports();
        return usage();
    }

    const callname = std.mem.span(argv[2]);
    if (std.mem.eql(u8, callname, "--wasi")) {
        return wasi_run(&mod, allocator);
    }

    if (argv.len < 4) return usage();

    var in = try Instance.init(&mod, null);
    defer in.deinit();

    const sym = try mod.lookup_export(callname) orelse
        return dbg("not found :pensive:\n", .{});

    dbg("SYM: {}\n", .{sym});
    if (sym.kind != .func) return dbg("not a function :(\n", .{});

    const num = try std.fmt.parseInt(i32, std.mem.span(argv[3]), 10);
    var res: [1]StackValue = undefined;
    const n_res = try in.execute(sym.idx, &.{.{ .i32 = num }}, &res);
    if (n_res != 1) dbg("TODO: n_res\n", .{});
    dbg("{s}({}) == {}\n", .{ std.mem.span(argv[2]), num, res[0].i32 });
}

fn wasi_proc_exit(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    _ = in;
    const arg = args_ret[0].i32;
    dbg("wasi exit: {}\n", .{arg});
    return error.WASMTrap;
}

fn wasi_fd_read(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    _ = in;
    _ = args_ret;
    return error.WASMTrap;
}

fn wasi_fd_write(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    const fd = args_ret[0].i32;
    const iovs: u32 = @intCast(args_ret[1].i32);
    const iovs_len: u32 = @intCast(args_ret[2].i32);
    const res_size_ptr: u32 = @intCast(args_ret[3].i32);

    if (fd != 2) return error.WASMTrap;

    dbg("print to {} with {}:{} and {}\n", .{ fd, iovs, iovs_len, res_size_ptr });

    const raw_iovec = try in.mem_get_bytes(iovs, iovs_len * 8);
    //const iovec = try in.mem_get_as([2]u32, iovs, iovs_len);
    var cumulative: u32 = 0;
    for (0..@as(usize, @intCast(iovs_len))) |i| {
        const pos = 8 * i;
        const iptr = std.mem.readInt(u32, raw_iovec[pos..][0..4], .little);
        const ilen = std.mem.readInt(u32, raw_iovec[pos + 4 ..][0..4], .little);

        // TODO: actually use ioKVÄCK of the underlying platform
        const aout = try in.mem_get_bytes(iptr, ilen);
        std.debug.print("{s}", .{aout});
        cumulative += ilen;
    }

    const raw_ret = try in.mem_get_bytes(res_size_ptr, 4);
    std.mem.writeInt(u32, raw_ret[0..4], cumulative, .little);
    args_ret[0] = .{ .i32 = 0 }; // SUCCESS
}

fn wasi_clock_time_get(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    _ = args_ret;
    _ = in;
    return error.WASMTrap;
}

fn wasi_run(mod: *wasm_shelf.Module, allocator: std.mem.Allocator) !void {
    var imports: wasm_shelf.ImportTable = .init(allocator);
    defer imports.deinit();

    try imports.add_func("proc_exit", .{ .cb = &wasi_proc_exit, .n_args = 1, .n_res = 0 });
    try imports.add_func("fd_read", .{ .cb = &wasi_fd_read, .n_args = 4, .n_res = 1 });
    try imports.add_func("fd_write", .{ .cb = &wasi_fd_write, .n_args = 4, .n_res = 1 });
    try imports.add_func("clock_time_get", .{ .cb = &wasi_clock_time_get, .n_args = 3, .n_res = 1 });

    var in = try wasm_shelf.Instance.init(mod, &imports);

    const sym = try mod.lookup_export("_start") orelse
        return dbg("not a wasi module? :pensive:\n", .{});

    if (sym.kind != .func) return dbg("not a function :(\n", .{});
    _ = try in.execute(sym.idx, &.{}, &.{});

    defer in.deinit();
}
