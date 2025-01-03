const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const wasm_shelf = b.createModule(.{
        .root_source_file = b.path("src/wasm_shelf.zig"),
    });

    const exe = b.addExecutable(.{
        .name = "wasm_run",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

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

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
