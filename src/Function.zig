const Function = @This();
typeidx: u32,
codeoff: u32,
control: ?[]ControlItem,

const std = @import("std");
const defs = @import("./defs.zig");
const dbg = std.debug.print;

const read = @import("./read.zig");
const readLeb = read.readLeb;
const readu = read.readu;
const readName = read.readName;
const Reader = read.Reader;

const ControlItem = struct {
    off: u32, // this is absolute (relative to mod.raw)
    jmp_t: u16,
};

pub fn parse(self: *Function, r: Reader, allocator: std.heap.Allocator) !void {
    self.codeoff = @intCast(r.context.pos);

    const n_locals = try readu(r);
    for (0..n_locals) |_| {
        const n_decl = try readu(r);
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        dbg("{} x {s}, ", .{ n_decl, @tagName(typ) });
    }
    dbg("\n", .{});

    var level: u32 = 1;

    const clist: std.ArrayList(ControlItem) = .init(allocator);
    // these point to the entry point of each level. for if-else-end we put in else_ when we have seen it
    const cstack: std.ArrayList(struct { start: u16, else_: u16 = 0 }) = .init(allocator);
    // TODO: this is a sentinel, might be eliminated (use jmp_t = 0xFFFF instead for "INVALID")
    try clist.append(.{ .off = 0, .jmp_t = 0 });

    while (level >= 1) {
        const pos = r.context.pos;
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        if (inst == .end or inst == .else_) level -= 1;

        for (0..level) |_| dbg("  ", .{});
        dbg("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                level += 1;
                try read.blocktype(r);
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                try cstack.append(clist.items.len - 1);
            },
            .end => {
                cstack.items.len -= 1;
            },
            .ret => {},
            .else_ => {
                level += 1;
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                clist.items[cstack.items[cstack.items.len - 1]].else_ = clist.items.len - 1;
            },
            .br, .br_if => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                dbg(" {}", .{val});
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                dbg(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
            },
            .drop, .select => {},
            .prefixed => {
                const code: defs.Prefixed = @enumFromInt(try readu(r));
                dbg(":{s}", .{@tagName(code)});
                switch (code) {
                    .memory_fill => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    .memory_copy => {
                        if (try r.readByte() != 0) return error.InvalidFormat;
                        if (try r.readByte() != 0) return error.InvalidFormat;
                    },
                    else => {
                        dbg(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
                        return;
                    },
                }
            },
            else => {
                const idx = @intFromEnum(inst);
                if (idx >= 0x45 and idx <= 0xc4) {
                    // ok, parameterless
                } else if (idx >= 0x28 and idx <= 0x3e) {
                    const alignas = try readu(r);
                    const offset = try readu(r);
                    dbg(" a={} o={}", .{ alignas, offset });
                } else {
                    dbg(" TBD, aborting!\n", .{});
                    return;
                }
            },
        }
        dbg("\n", .{});
    }
}
