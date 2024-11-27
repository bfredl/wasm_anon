//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");
const dbg = std.debug.print;

const wasm_shelf = @import("wasm_shelf");

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

    if (try mod.lookup_export("_start")) |sym| {
        dbg("SYM: {}\n", .{sym});
    } else {
        dbg("not found :pensive:\n", .{});
    }
}
