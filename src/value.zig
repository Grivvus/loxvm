const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const object = @import("object.zig");

const ValueType = enum {
    BOOL,
    NIL,
    NUMBER,
    OBJECT,
};

const ValueUnion = union {
    boolean: bool,
    number: f64,
    obj: object.Object,
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
    pub fn initObject(val: object.Object) Value {
        return Value{ .vt = .OBJECT, .val = .{ .obj = val } };
    }
    pub fn asBoolean(self: Value) bool {
        return self.val.boolean;
    }
    pub fn asNumber(self: Value) f64 {
        return self.val.number;
    }
    pub fn asObject(self: Value) object.Object {
        return self.val.obj;
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
    pub fn isObject(self: Value) bool {
        return self.vt == .OBJECT;
    }
    pub fn isEqual(v1: Value, v2: Value) bool {
        if (v1.vt != v2.vt) {
            return false;
        }
        return switch (v1.vt) {
            .BOOL => v1.asBoolean() == v2.asBoolean(),
            .NIL => true, // v1 and v2 have same type, so nil == nil
            .NUMBER => v1.asNumber() == v2.asNumber(),
            .OBJECT => switch (v1.asObject().type_) {
                .OBJ_STRING => {
                    const v1_obj = v1.asObject();
                    const v2_obj = v2.asObject();
                    const v1_objstr = v1_obj.asObjString();
                    const v2_objstr = v2_obj.asObjString();
                    return std.mem.eql(u8, v1_objstr.str, v2_objstr.str);
                },
            },
        };
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
