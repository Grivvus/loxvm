const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = f64;

pub const ValueArray = struct {
    values: ArrayList(Value),

    pub fn init(allocator: Allocator) ValueArray {
        return ValueArray{ .values = ArrayList(Value).init(allocator) };
    }

    pub fn deinit(self: *ValueArray) void {
        self.values.deinit();
    }

    pub fn write(self: *ValueArray, value: Value) !void {
        try self.values.append(value);
    }
};
