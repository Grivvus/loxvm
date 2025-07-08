const std = @import("std");

pub const ObjType = enum {
    OBJ_STRING,
};

pub const Object = struct {
    type_: ObjType,
    mem: *anyopaque,
    pub fn initObjString(src: []const u8, alloc: std.mem.Allocator) !Object {
        return Object{ .type_ = .OBJ_STRING, .mem = try ObjString.init(src, alloc) };
    }
    pub fn deinitObjString(self: Object, alloc: std.mem.Allocator) void {
        if (self.type_ != .OBJ_STRING) {
            @panic("trying to deallocate wrong object");
        }
        const objstring_ptr: *ObjString = self.asObjString();
        objstring_ptr.deinit(alloc);
    }
    pub fn isObjString(self: Object) bool {
        return self.type_ == .OBJ_STRING;
    }
    pub fn asObjString(self: Object) *ObjString {
        if (self.type_ != .OBJ_STRING) {
            @panic("This object should be a string");
        }
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn printObject(self: Object) void {
        switch (self.type_) {
            .OBJ_STRING => std.debug.print("{s}", .{self.asObjString().str}),
        }
    }
};

pub const ObjString = struct {
    str: []u8,
    pub fn init(source: []const u8, alloc: std.mem.Allocator) !*ObjString {
        const obj = try alloc.create(ObjString);
        const alloc_str = try alloc.alloc(u8, source.len);
        std.mem.copyBackwards(u8, alloc_str, source);
        obj.*.str = alloc_str;
        return obj;
    }
    pub fn deinit(self: *ObjString, alloc: std.mem.Allocator) void {
        alloc.free(self.str);
        alloc.destroy(self);
    }
};
