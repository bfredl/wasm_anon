const std = @import("std");
const testing = std.testing;

pub fn parse(module: []u8) !void {
    if (module.len < 8) return error.InvalidFormat;
    if (!std.mem.eql(u8, module[0..8], &.{ 0, 'a', 's', 'm', 1, 0, 0, 0 })) return error.InvalidFormat;
}

test "basic functionality" {
    try testing.expect(11 == 10);
}
