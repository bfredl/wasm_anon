const std = @import("std");
const dbg = std.debug.print;

const util = @import("./util.zig");
const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;
const Instance = wasm_shelf.Instance;
const clap = @import("clap");

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-f, --func <str>       call function
    \\-i, --inspect          inspect imports and exports
    \\--stdin <str>          override wasi stdin
    \\<str>
    \\
);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const argv = std.os.argv;
    var diag = clap.Diagnostic{};
    var param = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = gpa.allocator(),
    }) catch |err| {
        // Report useful error and exit.
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer param.deinit();

    const filearg = param.positionals[0] orelse @panic("usage");
    const buf = try util.readall(allocator, filearg);
    defer allocator.free(buf);

    var mod = try wasm_shelf.Module.parse(buf, allocator);
    defer mod.deinit();
    defer mod.dump_counts();

    if (param.args.inspect > 0) {
        try mod.dbg_imports();
        try mod.dbg_exports();
        return;
    }

    if (param.args.func) |func| {
        _ = func;
        @panic("NYI");
    } else {
        return wasi_run(&mod, allocator, @ptrCast(param.args.stdin));
    }

    const callname = std.mem.span(argv[2]);

    var in = try Instance.init(&mod, null);
    defer in.deinit();

    const sym = try mod.lookup_export(callname) orelse
        return dbg("not found :pensive:\n", .{});

    dbg("SYM: {}\n", .{sym});
    if (sym.kind != .func) return dbg("not a function :(\n", .{});

    const num = try std.fmt.parseInt(i32, std.mem.span(argv[3]), 10);
    var res: [1]StackValue = undefined;
    const n_res = try in.execute(sym.idx, &.{.{ .i32 = num }}, &res, true);
    if (n_res != 1) dbg("TODO: n_res\n", .{});
    dbg("{s}({}) == {}\n", .{ std.mem.span(argv[2]), num, res[0].i32 });
}

fn wasi_proc_exit(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    _ = in;
    const arg = args_ret[0].i32;
    dbg("wasi exit: {}\n", .{arg});
    // std.posix.exit(0); // for benchmarking which expect 0 ret..
    return error.WASMTrap;
}

fn wasi_fd_read(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    const fd = args_ret[0].i32;
    const iovs: u32 = @intCast(args_ret[1].i32);
    const iovs_len: u32 = @intCast(args_ret[2].i32);
    const res_size_ptr: u32 = @intCast(args_ret[3].i32);

    if (fd != 0) return error.WASMTrap;

    const raw_iovec = try in.mem_get_bytes(iovs, iovs_len * 8);
    //const iovec = try in.mem_get_as([2]u32, iovs, iovs_len);
    var cumulative: u32 = 0;
    for (0..@as(usize, @intCast(iovs_len))) |i| {
        const pos = 8 * i;
        const iptr = std.mem.readInt(u32, raw_iovec[pos..][0..4], .little);
        const ilen = std.mem.readInt(u32, raw_iovec[pos + 4 ..][0..4], .little);

        // TODO: actually use ioKVÄCK of the underlying platform
        const ain = try in.mem_get_bytes(iptr, ilen);
        const rlen = std.posix.read(fd, ain) catch return error.WASMTrap;
        cumulative += @intCast(rlen);
    }

    const raw_ret = try in.mem_get_bytes(res_size_ptr, 4);
    std.mem.writeInt(u32, raw_ret[0..4], cumulative, .little);
    args_ret[0] = .{ .i32 = 0 }; // SUCCESS
}

fn wasi_fd_write(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    _ = data;
    const fd = args_ret[0].i32;
    const iovs: u32 = @intCast(args_ret[1].i32);
    const iovs_len: u32 = @intCast(args_ret[2].i32);
    const res_size_ptr: u32 = @intCast(args_ret[3].i32);

    if (fd != 2 and fd != 1) return error.WASMTrap;

    dbg("print to {}:\n", .{fd});

    const raw_iovec = try in.mem_get_bytes(iovs, iovs_len * 8);
    //const iovec = try in.mem_get_as([2]u32, iovs, iovs_len);
    var cumulative: u32 = 0;
    for (0..@as(usize, @intCast(iovs_len))) |i| {
        const pos = 8 * i;
        const iptr = std.mem.readInt(u32, raw_iovec[pos..][0..4], .little);
        const ilen = std.mem.readInt(u32, raw_iovec[pos + 4 ..][0..4], .little);

        // TODO: actually use ioKVÄCK of the underlying platform
        const aout = try in.mem_get_bytes(iptr, ilen);
        if (fd == 1) {
            _ = std.io.getStdOut().write(aout) catch return error.WASMTrap;
        } else {
            std.debug.print("{s}", .{aout});
        }
        cumulative += ilen;
    }

    const raw_ret = try in.mem_get_bytes(res_size_ptr, 4);
    std.mem.writeInt(u32, raw_ret[0..4], cumulative, .little);
    args_ret[0] = .{ .i32 = 0 }; // SUCCESS
}

fn wasi_clock_time_get(args_ret: []StackValue, in: *Instance, data: *anyopaque) !void {
    const klocka: *std.time.Timer = @ptrCast(@alignCast(data));
    const id = args_ret[0].i32;
    const res_timestamp = try in.mem_get_bytes(args_ret[2].u32(), 8);

    if (id == 1) {
        const time = klocka.read(); // PRESENT DAY, PRESENT TIME
        std.mem.writeInt(u64, res_timestamp[0..8], time, .little);
    } else {
        dbg("hey guys check this out: {}\n", .{id});
        return error.WASMTrap;
    }
}

fn wasi_run(mod: *wasm_shelf.Module, allocator: std.mem.Allocator, stdin: ?[:0]const u8) !void {
    if (stdin) |path| {
        const fd = try std.posix.openZ(path, .{ .ACCMODE = .RDONLY }, 0);
        try std.posix.dup2(fd, 0);
    }

    var imports: wasm_shelf.ImportTable = .init(allocator);
    defer imports.deinit();

    var klocka = try std.time.Timer.start();

    try imports.add_func("proc_exit", .{ .cb = &wasi_proc_exit, .n_args = 1, .n_res = 0 });
    try imports.add_func("fd_read", .{ .cb = &wasi_fd_read, .n_args = 4, .n_res = 1 });
    try imports.add_func("fd_write", .{ .cb = &wasi_fd_write, .n_args = 4, .n_res = 1 });
    try imports.add_func("clock_time_get", .{ .cb = &wasi_clock_time_get, .n_args = 3, .n_res = 1, .data = @ptrCast(&klocka) });

    var in = try wasm_shelf.Instance.init(mod, &imports);

    const sym = try mod.lookup_export("_start") orelse
        return dbg("not a wasi module? :pensive:\n", .{});

    if (sym.kind != .func) return dbg("not a function :(\n", .{});

    _ = try in.execute(sym.idx, &.{}, &.{}, true);

    defer in.deinit();
}
