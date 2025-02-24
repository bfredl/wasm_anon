const std = @import("std");
const dbg = std.debug.print;

const wasm_shelf = @import("wasm_shelf");
const StackValue = wasm_shelf.StackValue;

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
        return wasi_run(&mod);
    }

    if (argv.len < 4) return usage();

    var in = try wasm_shelf.Instance.init(&mod, null);
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

fn wasi_run(mod: *wasm_shelf.Module) !void {
    var in = try wasm_shelf.Instance.init(mod, null);

    const sym = try mod.lookup_export("_start") orelse
        return dbg("not a wasi module? :pensive:\n", .{});

    if (sym.kind != .func) return dbg("not a function :(\n", .{});
    _ = try in.execute(sym.idx, &.{}, &.{});

    defer in.deinit();
}
