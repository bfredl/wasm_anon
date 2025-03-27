const Module = @import("./Module.zig");
const Function = @import("./Function.zig");
const defs = @import("./defs.zig");
const dbg = std.debug.print;
const std = @import("std");
const read = @import("./read.zig");
const readu = read.readu;
const readLeb = read.readLeb;

const forklift = @import("forklift");
const X86Asm = forklift.X86Asm;
const IPReg = X86Asm.IPReg;

// "one pass" compiler, although we might cheat and allow the Function.ensure_parsed()
// pass to pre-collect some required data.
//
// TODO: eleminate stack_base as this is should be a compile time known offset from frame_base

// c calling convention:
const frame_base: IPReg = .rdi; // arg1: pointer to first local (then stack)
const stack_base: IPReg = .rsi; // arg2: pointer to first stack slot to use (TODO: fold into arg1)
const mem_start: IPReg = .rdx;
// arg4: rcx = memory_len

const bo = X86Asm.bo;
pub fn local_slot(idx: u32) X86Asm.EAddr {
    return bo(frame_base, @intCast(8 * idx));
}
pub fn stack_slot(idx: u32) X86Asm.EAddr {
    return bo(stack_base, @intCast(8 * idx));
}

const ThunderLightning = @This();
cfo: X86Asm,

// keep track of up to two stack levels. the states are essentially
// 0. all of stack is in stack_base[]
// 1. top of stack is in virt_state[0]
// 2. top of stack is in virt_state[1], next in virst_state[0]
// in state 2: virt_state[0] cannot be a memory address

num_tracked: u8 = 0,
virt_state: [2]ValueState = undefined, // only used with rax_state=2

val_stack_level: u16 = 0, // wasm stack level, regardless if actually stored or not

const ValueState = union(enum) {
    // only a blessed set, currently rax and r10
    reg: IPReg,
    imm: defs.StackValue,
    local: u32,
    on_stack: void, // a bit redudant but useful is some edge cases
};

// get_foo_bar() implies "make it so"! these might emit an instruction to
// ensure the proper state of the top

// if we are in state 2, transition to state 1.
// return if rax is free to use already
pub fn push_value_prepare(self: *ThunderLightning) !u8 {
    self.val_stack_level += 1;
    if (self.num_tracked == 2) {
        try switch (self.virt_state[0]) {
            .on_stack => {},
            .reg => |reg| self.cfo.movmr(stack_slot(self.val_stack_level - 3), reg),
            .imm => |val| self.cfo.movmi(stack_slot(self.val_stack_level - 3), val.i32),
            .local => @panic("should not happen:p"),
        };
        self.virt_state[0] = self.virt_state[1];
        self.num_tracked = 1; // FALLTHROUGH;
    }
    if (self.num_tracked == 1) {
        switch (self.virt_state[0]) {
            .local => |idx| {
                try self.cfo.movrm(.rax, local_slot(idx));
                self.virt_state[0] = .{ .reg = .rax };
            },
            .reg, .imm, .on_stack => {},
        }
        self.num_tracked = 2;
        return 1;
    }
    self.num_tracked = 1;
    return 0;
}

// TODO: "commutative" flag
pub fn pop_as_reg_virt(self: *ThunderLightning) !struct { IPReg, ValueState } {
    self.val_stack_level -= 1;
    if (self.num_tracked == 0 or (self.num_tracked == 1 and self.virt_state[0] == .on_stack)) {
        try self.cfo.movrm(.rax, stack_slot(self.val_stack_level)); // note: pre-adjusted
        self.num_tracked = 1;
        self.virt_state[0] = .{ .reg = .rax };
        return .{ .rax, .{ .on_stack = {} } };
    } else if (self.num_tracked == 1) {
        const alt = self.virt_state[0];
        const freereg: IPReg = if (switch (alt) {
            .reg => |reg| reg != .rax,
            else => true,
        }) .rax else .r10;
        try self.cfo.movrm(freereg, stack_slot(self.val_stack_level)); // note: pre-adjusted
        self.virt_state[0] = .{ .reg = freereg };
        return .{ freereg, alt };
    } else {
        self.num_tracked = 1;
        const alt = self.virt_state[1];
        const freereg: IPReg = if (switch (alt) {
            .reg => |reg| reg != .rax,
            else => true,
        }) .rax else .r10;
        try switch (self.virt_state[0]) {
            .on_stack => self.cfo.movrm(freereg, stack_slot(self.val_stack_level - 1)),
            .reg => |reg| return .{ reg, alt },
            .imm => |val| self.cfo.movri(freereg, val.i32),
            .local => @panic("should not happen:p"),
        };
        self.virt_state[0] = .{ .reg = freereg };
        return .{ freereg, alt };
    }
}

pub fn top_as_reg(self: *ThunderLightning) !IPReg {
    if (self.num_tracked == 0 or (self.num_tracked == 1 and self.virt_state[0] == .on_stack)) {
        try self.cfo.movrm(.rax, stack_slot(self.val_stack_level - 1));
        self.num_tracked = 1;
        self.virt_state[0] = .{ .reg = .rax };
        return .rax;
    } else if (self.num_tracked == 1) {
        try switch (self.virt_state[0]) {
            .on_stack => unreachable, // handled above
            .reg => |reg| return reg,
            .imm => |val| self.cfo.movri(.rax, val.i32),
            .local => |idx| self.cfo.movrm(.rax, local_slot(idx)),
        };
        self.virt_state[0] = .{ .reg = .rax };
        return .rax;
    } else { // num_tracked == 2
        const alt = self.virt_state[0];
        const freereg: IPReg = if (switch (alt) {
            .reg => |reg| reg != .rax,
            else => true,
        }) .rax else .r10;

        try switch (self.virt_state[1]) {
            .on_stack => self.cfo.movrm(freereg, stack_slot(self.val_stack_level - 1)),
            .reg => |reg| return reg,
            .imm => |val| self.cfo.movri(freereg, val.i32),
            .local => |idx| self.cfo.movrm(freereg, local_slot(idx)),
        };
        self.virt_state[1] = .{ .reg = freereg };
        return freereg;
    }
}

pub fn pop_as_reg(self: *ThunderLightning) !IPReg {
    const reg = self.top_as_reg();
    self.val_stack_level -= 1;
    self.num_tracked -= 1;
    return reg;
}

pub fn pop2_as_reg_regimm(self: *ThunderLightning) !struct { IPReg, ValueState } {
    self.val_stack_level -= 1;
    const reg, const virt = try self.pop_as_reg_virt();
    if (self.num_tracked != 1) @panic("do not");
    self.num_tracked = 0;
    const freereg: IPReg = if (reg == .rax) .r10 else .rax;
    try switch (virt) {
        .reg, .imm => return .{ reg, virt },
        .local => |idx| self.cfo.movrm(freereg, local_slot(idx)),
        .on_stack => self.cfo.movrm(freereg, stack_slot(self.val_stack_level + 1)), // luring, load what is above the stack
    };
    return .{ reg, .{ .reg = freereg } };
}

// blk_idx is into the control array of function
pub fn compile_block(mod: *Module, func: *Function, blk_idx: u32) !void {
    const c = try func.ensure_parsed(mod);
    const blk_off = c[blk_idx].off;

    var code = try forklift.CodeBuffer.init(mod.allocator);
    var self: ThunderLightning = .{ .cfo = X86Asm{ .code = &code, .long_jump_mode = true } };
    const cfo = &self.cfo;

    var fbs = mod.fbs_at(blk_off);
    const r = fbs.reader();

    if ((try r.readByte()) != @intFromEnum(defs.OpCode.loop)) return error.NotImplemented;
    const top_args, const top_ret = try (try read.blocktype(r)).arity(mod);
    if (top_args != 0 or top_ret != 0) return error.NotImplemented;

    // TODO: control should know the start level
    var level: u16 = 1;

    _ = stack_base;

    errdefer cfo.dbg_nasm(mod.allocator) catch unreachable;

    while (true) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        if (inst == .end or inst == .else_) level -= 1;

        dbg("{x:04} => {x:04}:", .{ pos, code.get_target() });
        for (0..level) |_| dbg("  ", .{});
        dbg("[{} {}] {s}\n", .{ self.val_stack_level, self.num_tracked, @tagName(inst) });
        switch (inst) {
            .local_get => {
                const idx = try readu(r);
                const slot = try self.push_value_prepare();
                self.virt_state[slot] = .{ .local = idx };
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                const slot = try self.push_value_prepare();
                self.virt_state[slot] = .{ .imm = .{ .i32 = val } };
            },
            .local_set, .local_tee => {
                const idx = try readu(r);
                // TODO: top_as_regimm??
                const src = try if (inst == .local_tee) self.top_as_reg() else self.pop_as_reg();
                try cfo.movmr(local_slot(idx), src);
            },
            .i32_add => {
                const dst, const src = try self.pop_as_reg_virt();
                try switch (src) {
                    .imm => |val| cfo.aritri(.add, dst, val.i32),
                    .reg => |reg| cfo.arit(.add, dst, reg),
                    .local => |idx| cfo.aritrm(.add, dst, local_slot(idx)),
                    .on_stack => cfo.aritrm(.add, dst, stack_slot(self.val_stack_level)),
                };
            },
            .i64_load => {
                const dst = try self.top_as_reg();
                const alignas = try readu(r);
                _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                const offset = try readu(r);
                // TODO: baaaunds checking
                try cfo.movrm(dst, X86Asm.bi(mem_start, dst).o(@intCast(offset)));
            },
            .i64_store => {
                const dst, const src = try self.pop2_as_reg_regimm();
                const alignas = try readu(r);
                _ = alignas; // "The alignment in load and store instructions does not affect the semantics."
                const offset = try readu(r);
                // TODO: refactor forklift to use .decl constructors for EAddr!
                const dstaddr = X86Asm.bi(mem_start, dst).o(@intCast(offset));
                try switch (src) {
                    .imm => |val| cfo.movmi(dstaddr, val.i32),
                    .reg => |reg| cfo.movmr(dstaddr, reg),
                    .local, .on_stack => unreachable, // TODO: separate type for reg/imm only?
                };
            },
            else => {
                return error.NotImplemented;
            },
        }
    }
}
