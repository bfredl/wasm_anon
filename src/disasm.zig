const Module = @import("./Module.zig");
const defs = @import("./defs.zig");
const dbg = std.debug.print;
const std = @import("std");
const Reader = @import("./Reader.zig");

// TODO: this should take in optional "post-mortem" info from a function
pub fn disasm_block(mod: *Module, blk_off: u32, in_block: bool) !void {
    var level: u32 = 1; // non-zero in case we disasm a "end" or "else_"

    var r = mod.reader_at(blk_off);
    while (true) {
        const pos = r.pos;
        const inst: defs.OpCode = @enumFromInt(try r.readByte());
        if (inst == .end or inst == .else_) level -= 1;

        dbg("{x:04}:", .{pos});
        for (0..level) |_| dbg("  ", .{});
        dbg("{s}", .{@tagName(inst)});
        switch (inst) {
            .block, .loop, .if_ => {
                level += 1;
                const typ = try r.blocktype();
                dbg(" typ={}", .{typ});
            },
            .end, .ret => {},
            .else_ => {
                level += 1;
            },
            .br, .br_if => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
            },
            .br_table => {
                const n = try r.readu();
                for (0..n) |_| {
                    const idx = try r.readu();
                    dbg(" {}", .{idx});
                }
                const dflt = try r.readu(); // default
                dbg(": {}", .{dflt});
            },
            .call => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
                // TODO: find the type
            },
            .call_indirect => {
                const typidx = try r.readu();
                const tblidx = try r.readu();
                dbg("table:{} typ:", .{tblidx});
                _ = typidx;
            },
            .nop, .unreachable_ => {},
            .i32_const => {
                const val = try r.readLeb(i32);
                dbg(" {}", .{val});
            },
            .i64_const => {
                const val = try r.readLeb(i64);
                dbg(" {}", .{val});
            },
            .f32_const => {
                const val = try r.readf(f32);
                dbg(" {}", .{val});
            },
            .f64_const => {
                const val = try r.readf(f64);
                dbg(" {}", .{val});
            },
            .local_get, .local_set, .local_tee => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
            },
            .global_get, .global_set => {
                const idx = try r.readu();
                dbg(" {}", .{idx});
            },
            .drop, .select => {},
            .select_t => {
                const num = try r.readu();
                if (num != 1) return error.InvalidFormat; // possible extension
                const typ: defs.ValType = @enumFromInt(try r.readByte());
                dbg(" {}", .{typ});
            },
            .memory_size, .memory_grow => {
                if (try r.readByte() != 0) return error.InvalidFormat;
            },
            .prefixed => {
                const code = try r.prefix();
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
                    const alignas = try r.readu();
                    const offset = try r.readu();
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
