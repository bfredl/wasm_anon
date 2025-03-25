const Module = @import("./Module.zig");
const defs = @import("./defs.zig");
const dbg = std.debug.print;
const std = @import("std");
const read = @import("./read.zig");
const readu = read.readu;
const readLeb = read.readLeb;

// TODO: this should take in optional "post-mortem" info from a function
pub fn disasm_block(mod: *Module, blk_off: u32, in_block: bool) !void {
    var level: u32 = 1; // non-zero in case we disasm a "end" or "else_"

    var fbs = mod.fbs_at(blk_off);
    const r = fbs.reader();
    while (true) {
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
            },
            .end, .ret => {},
            .else_ => {
                level += 1;
            },
            .br, .br_if => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
            },
            .br_table => {
                const n = try readu(r);
                for (0..n) |_| {
                    const idx = try readu(r);
                    dbg(" {}", .{idx});
                }
                const dflt = try readu(r); // default
                dbg(": {}", .{dflt});
            },
            .call => {
                const idx = try readLeb(r, u32);
                dbg(" {}", .{idx});
                // TODO: find the type
            },
            .call_indirect => {
                const typidx = try readLeb(r, u32);
                const tblidx = try readLeb(r, u32);
                dbg("table:{} typ:", .{tblidx});
                _ = typidx;
            },
            .nop, .unreachable_ => {},
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
            },
            .global_get, .global_set => {
                const idx = try readu(r);
                dbg(" {}", .{idx});
            },
            .drop, .select => {},
            .select_t => {
                const num = try readu(r);
                if (num != 1) return error.InvalidFormat; // possible extension
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                dbg(" {}", .{typ});
            },
            .memory_size, .memory_grow => {
                if (try r.readByte() != 0) return error.InvalidFormat;
            },
            .prefixed => {
                const code: defs.Prefixed = try read.prefix(r);
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
                            dbg(" UNKNOWN: {}, aborting!", .{@intFromEnum(code)});
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
                    dbg("inst {s} TBD, aborting!\n", .{@tagName(inst)});
                    return error.NotImplemented;
                }
            },
        }
        dbg("\n", .{});

        if (level <= @as(u32, if (in_block) 0 else 1)) break;
    }
}
