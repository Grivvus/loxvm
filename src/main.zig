const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const bytecode = @import("bytecode.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;
const Chunk = bytecode.Chunk;
const OpCodes = bytecode.OpCode;

pub fn main(init: std.process.Init) !void {
    var arena = init.arena;

    const args = try init.minimal.args.toSlice(init.arena.allocator());

    var vm = try VM.init(arena.allocator(), init.gpa, init.io);
    defer vm.deinit();
    if (args.len == 1) {
        try repl(&vm);
    } else if (args.len == 2) {
        try runFile(&vm, args[1], arena.allocator());
    } else {
        try std.Io.File.stdout()
            .writeStreamingAll(init.io, "Usage: clox [path]\n");
        @panic("\n");
    }
}

fn repl(vm: *VM) !void {
    const stdin = std.Io.File.stdin();
    var line_buffer: [1024]u8 = undefined;
    while (true) {
        std.debug.print("> ", .{});
        var stdin_reader = stdin.reader(vm.io, &line_buffer);
        const reader = &stdin_reader.interface;
        var read = reader.takeSentinel('\n') catch @panic("Buffer out");
        std.debug.print("{s}", .{read});
        std.debug.print("\n", .{});

        // somehow scanner doesn't see ';' if i read code from REPL
        read[read.len] = ';';
        read.len += 1;

        const result = try vm.interpret(read);
        if (result != .INTERPRET_OK) {
            std.debug.print("Interpreter error\n", .{});
            @panic("");
        }
    }
}

fn runFile(vm: *VM, filename: []const u8, allocator: Allocator) !void {
    const cwd = std.Io.Dir.cwd();
    const file = try cwd.openFile(vm.io, filename, .{});
    defer file.close(vm.io);
    var file_reader = file.reader(vm.io, &.{});
    const reader = &file_reader.interface;
    const fstat = try file.stat(vm.io);
    const source = try reader.readAlloc(allocator, fstat.size);
    if (builtin.mode == .Debug) {
        std.debug.print("{s}", .{source});
    }
    const result = try vm.interpret(source);
    if (result == .INTERPRET_OK) {
        std.debug.print("Interpret without errors\n", .{});
    } else {
        std.debug.print("Interpreter error {any}\n", .{result});
        @panic("");
    }
}
