const std = @import("std");
const testing = std.testing;
const dbg = std.debug.print;

fn readLeb(r: anytype, comptime T: type) !T {
    return switch (@typeInfo(T).int.signedness) {
        .signed => std.leb.readILEB128(T, r),
        .unsigned => std.leb.readULEB128(T, r),
    };
}

const SectionKind = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    export_ = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_count = 12,
    _,
};

pub fn parse(module: []const u8) !void {
    if (module.len < 8) return error.InvalidFormat;
    if (!std.mem.eql(u8, module[0..8], &.{ 0, 'a', 's', 'm', 1, 0, 0, 0 })) return error.InvalidFormat;

    var fbs = std.io.FixedBufferStream([]const u8){ .pos = 8, .buffer = module };
    const r = fbs.reader();

    while (true) {
        const id = r.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        const kind: SectionKind = @enumFromInt(id);

        const len = try readLeb(&r, u32);
        dbg("SECTION: {s} ({}) with len {}\n", .{ @tagName(kind), id, len });
        try r.skipBytes(len, .{});
    }
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
