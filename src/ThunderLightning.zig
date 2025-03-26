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
// arg3: rdx = memory_start
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
// 1. top of stack is in rax
// 2. element below top is in rax, top is either imm, R10 or a local slot

rax_state: u8 = 0,
virt_state: VirtState = undefined, // only used with rax_state=2

val_stack_level: u16 = 0, // wasm stack level, regardless if actually stored or not

const VirtState = union(enum) {
    // TODO: refactor .register(choice)
    reg_2nd: void,
    imm: defs.StackValue,
    local: u32,
};

// get_foo_bar() implies "make it so"! these might emit an instruction to
// ensure the proper state of the top

// if we are in state 2, transition to state 1.
// return if rax is free to use already
pub fn push_value_prepare(self: *ThunderLightning) !bool {
    if (self.rax_state == 2) {
        try self.cfo.movmr(stack_slot(self.val_stack_level - 1), .rax);
        try switch (self.virt_state) {
            .imm => |val| self.cfo.movri(.rax, val.i32), // TODO: types!!
            .reg_2nd => self.cfo.mov(.rax, .r10),
            .local => |idx| self.cfo.movrm(.rax, local_slot(idx)),
        };
    } else {
        self.rax_state += 1;
    }
    self.val_stack_level += 1;
    return self.rax_state == 1;
}

pub fn pop_as_virtual(self: *ThunderLightning) !VirtState {
    self.val_stack_level -= 1;
    if (self.rax_state != 2) return error.NotImplemented;
    self.rax_state = 1;
    return self.virt_state;
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
        dbg("{s}\n", .{@tagName(inst)});
        switch (inst) {
            .local_get => {
                const idx = try readu(r);
                if (try self.push_value_prepare()) {
                    // TODO: use eax for 32-bit locals to not get junk
                    try cfo.movrm(.rax, local_slot(idx));
                } else {
                    self.virt_state = .{ .local = idx };
                }
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                if (try self.push_value_prepare()) {
                    // TODO: use eax for 32-bit locals to not get junk
                    try cfo.movri(.rax, val);
                } else {
                    self.virt_state = .{ .imm = .{ .i32 = val } };
                }
            },
            .i32_add => {
                try switch (try self.pop_as_virtual()) {
                    .imm => |val| cfo.aritri(.add, .rax, val.i32),
                    .reg_2nd => cfo.arit(.add, .rax, .r10),
                    .local => |idx| cfo.aritrm(.add, .rax, local_slot(idx)),
                };
            },
            else => {
                return error.NotImplemented;
            },
        }
    }
}
