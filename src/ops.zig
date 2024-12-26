const WASMError = error{WASMTrap};
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
    pub fn @"and"(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs & rhs;
    }
    pub fn @"or"(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs | rhs;
    }
    pub fn xor(comptime t: type, lhs: t, rhs: t) WASMError!t {
        return lhs ^ rhs;
    }
};
