const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const bytecode = @import("bytecode.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;
const Chunk = bytecode.Chunk;
const OpCodes = bytecode.OpCode;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const args = try std.process.argsAlloc(arena_alloc);
    defer std.process.argsFree(arena_alloc, args);

    var vm = try VM.init(arena_alloc);
    defer vm.deinit();
    if (args.len == 1) {
        try repl(&vm);
    } else if (args.len == 2) {
        try runFile(&vm, args[1], arena_alloc);
    } else {
        std.debug.print("Usage: clox [path]\n", .{});
        @panic("\n");
    }
}

fn repl(vm: *VM) !void {
    const stdin = std.fs.File.stdin();
    var line_buffer: [1024]u8 = undefined;
    while (true) {
        std.debug.print("> ", .{});
        _ = stdin.readAll(&line_buffer) catch @panic("Buffer out");
        std.debug.print("{s}\n", .{line_buffer});
        std.debug.print("\n", .{});
        const result = try vm.interpret(&line_buffer);
        if (result != .INTERPRET_OK) {
            std.debug.print("Interpreter error\n", .{});
            @panic("");
        }
    }
}

fn runFile(vm: *VM, filename: []const u8, allocator: Allocator) !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    const source = try file.readToEndAlloc(allocator, 100_000);
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
