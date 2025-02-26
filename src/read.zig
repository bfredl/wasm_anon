const std = @import("std");
const defs = @import("./defs.zig");
const Module = @import("./Module.zig");
pub const Reader = std.io.FixedBufferStream([]const u8).Reader;

pub fn readLeb(r: Reader, comptime T: type) !T {
    return switch (@typeInfo(T).int.signedness) {
        .signed => std.leb.readILEB128(T, r),
        .unsigned => std.leb.readULEB128(T, r),
    };
}

pub fn readu(r: Reader) !u32 {
    return readLeb(r, u32);
}

pub fn readf(r: Reader, comptime T: type) !T {
    const ival = try r.readInt(if (T == f32) u32 else u64, .little);
    return @bitCast(ival);
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

pub const BlockType = union(enum) {
    simple: defs.ValType,
    complex_idx: u32,

    // args, results
    pub fn arity(self: BlockType, mod: *const Module) !struct { u16, u16 } {
        return switch (self) {
            .simple => |vt| .{ 0, if (vt == .void) 0 else 1 },
            .complex_idx => |idx| mod.type_arity(idx),
        };
    }
};

pub fn blocktype(r: Reader) !BlockType {
    // TODO: just readLeb(r, i33) directly and "interpret" negative values might be simpler?
    const nextByte = peekByte(r);
    if ((nextByte & 0xc0) == 0x40) {
        const t: defs.ValType = @enumFromInt(try r.readByte());
        return .{ .simple = t };
    } else {
        const tidx: u32 = @intCast(try readLeb(r, i33));
        return .{ .complex_idx = tidx };
    }
}

// throws on unknown prefix
pub fn prefix(r: Reader) !defs.Prefixed {
    const byte = try readu(r);
    if (byte > defs.max_prefixed) return error.NotImplemented;
    return @enumFromInt(byte);
}
