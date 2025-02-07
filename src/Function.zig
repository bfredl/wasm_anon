const Function = @This();
typeidx: u32,
n_params: u32 = undefined,
n_ret: u32 = undefined,
codeoff: u32 = undefined,
control: ?[]ControlItem = null,

const std = @import("std");
const defs = @import("./defs.zig");
const ops = @import("./ops.zig");
const Module = @import("./Module.zig");
const Instance = @import("./Instance.zig");
const dbg = Module.dbg;
const severe = std.debug.print;

const read = @import("./read.zig");
const readLeb = read.readLeb;
const readu = read.readu;
const readName = read.readName;
const Reader = read.Reader;

const ControlItem = struct {
    off: u32, // this is absolute (relative to mod.raw)
    jmp_t: u16,
};

pub fn ensure_parsed(self: *Function, mod: *Module) ![]ControlItem {
    if (self.control == null) {
        var fbs2 = mod.fbs_at(self.codeoff);
        const r_parse = fbs2.reader();
        try self.parse(mod, r_parse);
    }
    return self.control orelse @panic("how could you");
}

pub fn parse(self: *Function, mod: *Module, r: Reader) !void {
    var fbs_type = mod.fbs_at(mod.types[self.typeidx]);
    const r_type = fbs_type.reader();

    const tag = try r_type.readByte();
    if (tag != 0x60) return error.InvalidFormat;
    self.n_params = try readu(r_type);
    for (0..self.n_params) |_| {
        _ = try r_type.readByte(); // TEMP hack: while we don't validate runtime args
    }
    self.n_ret = try readu(r_type);

    self.codeoff = @intCast(r.context.pos);

    var n_locals: u32 = self.n_params;
    const n_local_defs = try readu(r);
    for (0..n_local_defs) |_| {
        const n_decl = try readu(r);
        n_locals += n_decl;
        const typ: defs.ValType = @enumFromInt(try r.readByte());
        dbg("{} x {s}, ", .{ n_decl, @tagName(typ) });
    }
    dbg("\n", .{});

    try self.parse_body(mod, r, n_locals);
}

pub fn parse_body(self: *Function, mod: *Module, r: Reader, n_locals: u32) !void {
    var level: u32 = 1;

    var clist: std.ArrayList(ControlItem) = .init(mod.allocator);
    // these point to the entry point of each level. for if-else-end we put in else_ when we have seen it
    var cstack: std.ArrayList(struct { start: u16 }) = .init(mod.allocator);
    // TODO: this is a sentinel, might be eliminated (use jmp_t = 0xFFFF instead for "INVALID")
    try clist.append(.{ .off = @intCast(r.context.pos), .jmp_t = 0 });
    try cstack.append(.{ .start = 0 });

    while (level >= 1) {
        const pos: u32 = @intCast(r.context.pos);
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        if (inst == .end or inst == .else_) level -= 1;

        dbg("{x:04}:", .{pos});
        for (0..level) |_| dbg("  ", .{});
        dbg("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                level += 1;
                const typ = try read.blocktype(r);
                dbg(" typ={}", .{typ});
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                try cstack.append(.{ .start = @intCast(clist.items.len - 1) });
            },
            .end => {
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                const start = &clist.items[cstack.items[cstack.items.len - 1].start];
                const start_op: defs.OpCode = @enumFromInt(r.context.buffer[start.off]);
                dbg(" (for {s} at {x:04})", .{ @tagName(start_op), start.off });
                start.jmp_t = @intCast(clist.items.len - 1);
                cstack.items.len -= 1;
            },
            .else_ => {
                level += 1;
                try clist.append(.{ .off = pos, .jmp_t = 0 });
                dbg(" (for if_ at {x:04})", .{clist.items[cstack.items[cstack.items.len - 1].start].off});
                // "if_" jumps to "else_", and "else_" jumps to end
                clist.items[cstack.items[cstack.items.len - 1].start].jmp_t = @intCast(clist.items.len - 1);
                cstack.items[cstack.items.len - 1].start = @intCast(clist.items.len - 1);
            },
            .br, .br_if => {
                // try clist.append(.{ .off = pos, .jmp_t = 0 });
                const idx = try readu(r);
                dbg(" {}", .{idx});
            },
            .br_table => {
                const n = try readu(r);
                for (0..n) |_| {
                    _ = try readu(r);
                }
                _ = try readu(r); // default
            },
            .ret => {},
            .call => {
                const idx = try readLeb(r, u32);
                dbg(" {}", .{idx});
            },
            .call_indirect => {
                const tblidx = try readLeb(r, u32);
                const idx = try readLeb(r, u32);
                dbg(" {}:{}", .{ tblidx, idx });
            },
            .i32_const => {
                const val = try readLeb(r, i32);
                dbg(" {}", .{val});
            },
            .i64_const => {
                const val = try readLeb(r, i64);
                dbg(" {}", .{val});
            },
            .f32_const => {
                const val = try read.readf(r, f32);
                dbg(" {}", .{val});
            },
            .f64_const => {
                const val = try read.readf(r, f64);
                dbg(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
                if (idx >= n_locals) return error.InvalidFormat;
            },
            .global_get, .global_set => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
                if (idx >= mod.n_globals) return error.InvalidFormat;
            },
            .drop, .select => {},
            .select_t => {
                const num = try readu(r);
                if (num != 1) return error.InvalidFormat; // possible extension
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                dbg(" {}", .{typ});
            },
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
                        if (@intFromEnum(code) < 8) {
                            // ok, converter
                        } else {
                            severe(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
                            return error.InvalidFormat;
                        }
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
                    severe("inst {s} TBD, aborting!\n", .{@tagName(inst)});
                    return error.NotImplemented;
                }
            },
        }
        dbg("\n", .{});
    }

    dbg("\n\n", .{});
    for (0.., clist.items) |i, c| {
        dbg("{:2}: {x:04} {}\n", .{ i, c.off, c.jmp_t });
    }

    self.control = try clist.toOwnedSlice();
}
