const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;

pub const ObjType = enum {
    OBJ_STRING,
    OBJ_FUNCTION,
};

pub const Object = struct {
    type_: ObjType,
    mem: *anyopaque,
    pub fn initObjString(src: []const u8, alloc: std.mem.Allocator) !Object {
        return Object{
            .type_ = .OBJ_STRING,
            .mem = try ObjString.init(src, alloc),
        };
    }
    pub fn deinitObjString(self: Object, alloc: std.mem.Allocator) void {
        if (self.type_ != .OBJ_STRING) {
            @panic("trying to deallocate wrong object");
        }
        const objstring_ptr: *ObjString = self.asObjString();
        objstring_ptr.deinit(alloc);
    }
    pub fn initObjFunction(alloc: std.mem.Allocator, name: ?*ObjString) !Object {
        return Object{
            .type_ = .OBJ_FUNCTION,
            .mem = try ObjFunction.init(alloc, name),
        };
    }
    pub fn fromFunction(function: *ObjFunction) Object {
        return Object{
            .type_ = .OBJ_FUNCTION,
            .mem = function,
        };
    }
    pub fn deinitObjFunction(self: *Object) void {
        if (!self.isObjFunction()) {
            @panic("This object is not a ObjFunction");
        }
        self.asObjFunction().deinit();
    }
    pub fn isObjString(self: Object) bool {
        return self.type_ == .OBJ_STRING;
    }
    pub fn isObjFunction(self: Object) bool {
        return self.type_ == .OBJ_FUNCTION;
    }

    pub fn asObjString(self: Object) *ObjString {
        if (self.type_ != .OBJ_STRING) {
            @panic("This object should be a string");
        }
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn asObjFunction(self: Object) *ObjFunction {
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn printObject(self: Object) void {
        switch (self.type_) {
            .OBJ_STRING => std.debug.print("{s}", .{self.asObjString().str}),
            .OBJ_FUNCTION => self.asObjFunction().print(),
        }
    }
};

pub const ObjString = struct {
    str: []u8,
    alloc: std.mem.Allocator,
    pub fn init(source: []const u8, alloc: std.mem.Allocator) !*ObjString {
        const obj = try alloc.create(ObjString);
        const alloc_str = try alloc.alloc(u8, source.len);
        std.mem.copyBackwards(u8, alloc_str, source);
        obj.*.str = alloc_str;
        obj.*.alloc = alloc;
        return obj;
    }
    pub fn deinit(self: *ObjString) void {
        self.alloc.free(self.str);
        self.destroy(self);
    }
};

pub const ObjFunction = struct {
    arity: u32,
    chunk: Chunk,
    name: ?*ObjString,
    alloc: std.mem.Allocator,
    pub fn init(alloc: std.mem.Allocator, name: ?*ObjString) !*ObjFunction {
        var function = try alloc.create(ObjFunction);
        const chunk = Chunk.init(alloc);
        function.chunk = chunk;
        function.alloc = alloc;
        function.arity = 0;
        function.name = name;
        return function;
    }
    pub fn deinit(self: *ObjFunction) void {
        self.chunk.deinit();
        if (self.name != null) {
            self.name.deinit();
        }
        self.alloc.destroy(self);
    }

    pub fn print(self: *ObjFunction) void {
        if (self.name != null) {
            std.debug.print("<fn {s}>", .{self.name.?.str});
        } else {
            std.debug.print("<script>", .{});
        }
    }
};
