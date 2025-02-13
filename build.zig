const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const llvm = b.option(bool, "llvm", "use llvm");

    const wasm_shelf = b.createModule(.{
        .root_source_file = b.path("src/wasm_shelf.zig"),
    });

    const exe = b.addExecutable(.{
        .name = "wasm_run",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    if (llvm) |val| exe.use_llvm = val;

    exe.root_module.addImport("wasm_shelf", wasm_shelf);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const wast_exe = b.addExecutable(.{
        .name = "run_wast_tests",
        .root_source_file = b.path("src/wast_runner.zig"),
        .target = target,
        .optimize = optimize,
    });
    if (llvm) |val| wast_exe.use_llvm = val;

    wast_exe.root_module.addImport("wasm_shelf", wasm_shelf);
    b.installArtifact(wast_exe);

    const wast_run_cmd = b.addRunArtifact(wast_exe);
    wast_run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        wast_run_cmd.addArgs(args);
    }
    const wast_run_step = b.step("wast", "Run the app");
    wast_run_step.dependOn(&wast_run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/wasm_shelf.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const use_upstream = true; // can we lazyDependency only for some b.step() ?
    const maybe_spec_dep = if (use_upstream) b.lazyDependency("spec", .{}) else null;
    const run_spec_tests = b.step("spectest", "Run spec tests");

    if (maybe_spec_dep) |spec_dep| {
        const upstream_specs = [_]struct { []const u8, u32 }{
            .{ "i32", 0 },
            .{ "i64", 0 },
            .{ "f32", 2 },
            .{ "f64", 2 },
            .{ "f32_cmp", 0 },
            .{ "f64_cmp", 0 },
            .{ "labels", 0 },
            .{ "br_if", 0 },
        };
        for (upstream_specs) |item| {
            const name, const fail = item;
            const spec_step = b.addRunArtifact(wast_exe);
            spec_step.addFileArg(spec_dep.path(b.fmt("test/core/{s}.wast", .{name})));
            spec_step.addArg(name);
            spec_step.addArg(b.fmt("{}", .{fail}));
            run_spec_tests.dependOn(&spec_step.step);
        }
    }

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
