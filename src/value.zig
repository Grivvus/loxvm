const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const ValueType = enum {
    BOOL,
    NIL,
    NUMBER,
};

const ValueUnion = union {
    boolean: bool,
    number: f64,
};

pub const Value = struct {
    vt: ValueType,
    val: ValueUnion,
    pub fn initBoolean(val: bool) Value {
        return Value{ .vt = .BOOL, .val = .{ .boolean = val } };
    }
    pub fn initNumber(val: f64) Value {
        return Value{ .vt = .NUMBER, .val = .{ .number = val } };
    }
    pub fn initNil() Value {
        return Value{ .vt = .NIL, .val = .{ .boolean = false } };
    }
    pub fn asBoolean(self: Value) bool {
        return self.val.boolean;
    }
    pub fn asNumber(self: Value) f64 {
        return self.val.number;
    }
    pub fn isBoolean(self: Value) bool {
        return self.vt == .BOOL;
    }
    pub fn isNumber(self: Value) bool {
        return self.vt == .NUMBER;
    }
    pub fn isNil(self: Value) bool {
        return self.vt == .NIL;
    }
};

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
