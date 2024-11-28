const std = @import("std");
const defs = @import("./defs.zig");
pub const Reader = std.io.FixedBufferStream([]const u8).Reader;
const dbg = std.debug.print;

pub fn readLeb(r: Reader, comptime T: type) !T {
    return switch (@typeInfo(T).int.signedness) {
        .signed => std.leb.readILEB128(T, r),
        .unsigned => std.leb.readULEB128(T, r),
    };
}

pub fn readu(r: Reader) !u32 {
    return readLeb(r, u32);
}

pub fn readName(r: Reader) ![]const u8 {
    const len = try readu(r);
    const str = r.context.buffer[r.context.pos..][0..len];
    r.context.pos += len;
    return str;
}

pub fn peekByte(r: Reader) u8 {
    return r.context.buffer[r.context.pos];
}

pub fn blocktype(r: Reader) !void {
    // TODO: just readLeb(r, i33) directly and "interpret" negative values might be simpler?
    const nextByte = peekByte(r);
    if ((nextByte & 0xc0) == 0x40) {
        const t: defs.ValType = @enumFromInt(try r.readByte());
        dbg(" typ={s}", .{@tagName(t)});
    } else {
        const tidx: u32 = @intCast(try readLeb(r, i33));
        dbg(" typid={}", .{tidx});
    }
}
