const WASMError = error{WASMTrap};
const std = @import("std");

fn utype(comptime t: type) type {
    return if (t == i32) u32 else if (t == i64) u64 else unreachable;
}

fn u(val: anytype) utype(@TypeOf(val)) {
    return @bitCast(val);
}

pub const ibinop = struct {
    pub fn add(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs +% rhs;
    }
    pub fn sub(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs -% rhs;
    }
    pub fn mul(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs *% rhs;
    }
    pub fn div_s(comptime t: type, lhs: t, rhs: t) WASMError!t {
        if (rhs == 0) return error.WASMTrap;
        if (lhs == std.math.minInt(t) and rhs == -1) return error.WASMTrap;
        return @divTrunc(lhs, rhs);
    }
    pub fn div_u(comptime t: type, lhs: t, rhs: t) WASMError!t {
        if (rhs == 0) return error.WASMTrap;
        return @bitCast(@divTrunc(u(lhs), u(rhs)));
    }
    pub fn rem_s(comptime t: type, lhs: t, rhs: t) WASMError!t {
        if (rhs == 0) return error.WASMTrap;
        if (lhs == std.math.minInt(t) and rhs == -1) return 0;
        return @rem(lhs, rhs);
    }
    pub fn rem_u(comptime t: type, lhs: t, rhs: t) WASMError!t {
        if (rhs == 0) return error.WASMTrap;
        return @bitCast(@rem(u(lhs), u(rhs)));
    }
    pub fn @"and"(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs & rhs;
    }
    pub fn @"or"(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs | rhs;
    }
    pub fn xor(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs ^ rhs;
    }
    pub fn shl(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs << @truncate(u(rhs));
    }
    pub fn shr_s(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs >> @truncate(u(rhs));
    }
    pub fn shr_u(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return @bitCast(u(lhs) >> @truncate(u(rhs)));
    }
    pub fn rotl(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return @bitCast(std.math.rotl(utype(t), @bitCast(lhs), rhs));
    }
    pub fn rotr(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return @bitCast(std.math.rotr(utype(t), @bitCast(lhs), rhs));
    }
};

pub const irelop = struct {
    pub fn eq(comptime t: type, lhs: t, rhs: t) bool {
        return lhs == rhs;
    }
    pub fn ne(comptime t: type, lhs: t, rhs: t) bool {
        return lhs != rhs;
    }
    pub fn lt_s(comptime t: type, lhs: t, rhs: t) bool {
        return lhs < rhs;
    }
    pub fn lt_u(comptime t: type, lhs: t, rhs: t) bool {
        return u(lhs) < u(rhs);
    }
    pub fn le_s(comptime t: type, lhs: t, rhs: t) bool {
        return lhs <= rhs;
    }
    pub fn le_u(comptime t: type, lhs: t, rhs: t) bool {
        return u(lhs) <= u(rhs);
    }
    pub fn gt_s(comptime t: type, lhs: t, rhs: t) bool {
        return lhs > rhs;
    }
    pub fn gt_u(comptime t: type, lhs: t, rhs: t) bool {
        return u(lhs) > u(rhs);
    }
    pub fn ge_s(comptime t: type, lhs: t, rhs: t) bool {
        return lhs >= rhs;
    }
    pub fn ge_u(comptime t: type, lhs: t, rhs: t) bool {
        return u(lhs) >= u(rhs);
    }
};

pub const iunop = struct {
    pub fn clz(comptime t: type, val: t) t {
        return @clz(u(val));
    }
    pub fn ctz(comptime t: type, val: t) t {
        return @ctz(u(val));
    }
    pub fn popcnt(comptime t: type, val: t) t {
        return @popCount(u(val));
    }
    pub fn extend8_s(comptime t: type, val: t) t {
        return @as(i8, @truncate(val));
    }
    pub fn extend16_s(comptime t: type, val: t) t {
        return @as(i16, @truncate(val));
    }
    pub fn extend32_s(comptime t: type, val: t) t {
        return @as(i32, @truncate(val));
    }
};

pub const fbinop = struct {
    pub fn add(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs + rhs;
    }
    pub fn sub(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs - rhs;
    }
    pub fn mul(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs * rhs;
    }
    pub fn div(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs / rhs;
    }
    pub fn min(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return @min(lhs, rhs);
    }
    pub fn max(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return @max(lhs, rhs);
    }
    pub fn copysign(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs * @abs(rhs);
    }
};

pub const funop = struct {
    pub fn abs(comptime t: type, val: t) WASMError!t {
        return @abs(val);
    }
    pub fn neg(comptime t: type, val: t) WASMError!t {
        return -val;
    }
    pub fn ceil(comptime t: type, val: t) WASMError!t {
        return @ceil(val);
    }
    pub fn floor(comptime t: type, val: t) WASMError!t {
        return @floor(val);
    }
    pub fn trunc(comptime t: type, val: t) WASMError!t {
        return @trunc(val);
    }
    pub fn nearest(comptime t: type, val: t) WASMError!t {
        // TODO: the bulllll rule to round towards even integer
        return @round(val);
    }
    pub fn sqrt(comptime t: type, val: t) WASMError!t {
        return @sqrt(val);
    }
};
