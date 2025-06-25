const std = @import("std");

const bytecode = @import("bytecode.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;
const Chunk = bytecode.Chunk;
const OpCodes = bytecode.OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer {
        const check = gpa.deinit();
        if (check == std.heap.Check.leak) {
            std.debug.print("Warning, found memory leaks in allocator\n", .{});
            @panic("Memory leaks found");
        } else {
            std.debug.print("No memory leaks found\n", .{});
        }
    }

    const allocator = gpa.allocator();
    var ch = Chunk.init(allocator);
    defer ch.deinit();

    var index = try ch.addConstant(1.2);
    var index_u8: u8 = @intCast(index);
    try ch.write(@intFromEnum(OpCodes.OP_CONSTANT), 123);
    try ch.write(index_u8, 123);
    try ch.write(@intFromEnum(OpCodes.OP_NEGATE), 123);

    index = try ch.addConstant(5.5);
    index_u8 = @intCast(index);
    try ch.write(@intFromEnum(OpCodes.OP_CONSTANT), 123);
    try ch.write(index_u8, 123);
    index = try ch.addConstant(6.5);
    index_u8 = @intCast(index);
    try ch.write(@intFromEnum(OpCodes.OP_CONSTANT), 123);
    try ch.write(index_u8, 123);
    try ch.write(@intFromEnum(OpCodes.OP_ADD), 123);

    try ch.write(@intFromEnum(OpCodes.OP_RETURN), 123);

    var vm = VM.init(&ch, allocator);
    defer vm.deinit();
    const res = vm.interpret(&ch);
    if (res == .INTERPRET_OK) {
        std.debug.print("Interpreted succesfully\n", .{});
    } else {
        std.debug.print("Unexpected error while interpreting\n", .{});
    }
}
