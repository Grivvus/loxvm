const std = @import("std");
const object = @import("object.zig");
const value = @import("value.zig");
const HashMap = std.HashMap;

pub const HashMapContext = struct {
    pub fn hash(self: @This(), s: *object.ObjString) u64 {
        _ = self;
        return s.hash();
    }
    pub fn eql(self: @This(), a: *object.ObjString, b: *object.ObjString) bool {
        _ = self;
        return std.mem.eql(u8, a.str, b.str);
    }
};

pub const Table = HashMap(
    *object.ObjString,
    value.Value,
    HashMapContext,
    std.hash_map.default_max_load_percentage,
);
