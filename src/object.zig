const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;
const Value = @import("value.zig").Value;

pub const ObjType = enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_CLOSURE,
    OBJ_NATIVE,
    OBJ_UPVALUE,
};

pub const NativeFn = *const (fn (argc: usize, args: []Value) Value);

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
    pub fn initObjClosure(alloc: std.mem.Allocator, function: *ObjFunction) !Object {
        return Object{
            .type_ = .OBJ_CLOSURE,
            .mem = try ObjClosure.init(alloc, function),
        };
    }
    pub fn fromClosure(closure: *ObjClosure) Object {
        return Object{
            .type_ = .OBJ_CLOSURE,
            .mem = closure,
        };
    }
    pub fn deinitObjClosure(self: *Object) void {
        self.asObjClosure().deinit();
    }
    pub fn initObjNative(
        alloc: std.mem.Allocator,
        function: NativeFn,
    ) !Object {
        return Object{
            .type_ = .OBJ_NATIVE,
            .mem = try ObjNative.init(alloc, function),
        };
    }
    pub fn deinitObjNative(self: *Object) void {
        self.asObjFunction().deinit();
    }
    pub fn isObjString(self: Object) bool {
        return self.type_ == .OBJ_STRING;
    }
    pub fn isObjFunction(self: Object) bool {
        return self.type_ == .OBJ_FUNCTION;
    }
    pub fn isObjClosure(self: Object) bool {
        return self.type_ == .OBJ_CLOSURE;
    }
    pub fn isObjNative(self: Object) bool {
        return self.type_ == .OBJ_NATIVE;
    }
    pub fn isObjUpvalue(self: Object) bool {
        return self.type_ == .OBJ_UPVALUE;
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
    pub fn asObjClosure(self: Object) *ObjClosure {
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn asObjNative(self: Object) *ObjNative {
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn asObjUpvalue(self: Object) *ObjUpvalue {
        return @ptrCast(@alignCast(self.mem));
    }

    pub fn printObject(self: Object) void {
        switch (self.type_) {
            .OBJ_STRING => std.debug.print("{s}", .{self.asObjString().str}),
            .OBJ_FUNCTION => self.asObjFunction().print(),
            .OBJ_CLOSURE => self.asObjClosure().function.print(),
            .OBJ_NATIVE => std.debug.print("<native fn>", .{}),
            .OBJ_UPVALUE => std.debug.print("upvalue", .{}),
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
    upvalue_cnt: usize,
    chunk: Chunk,
    name: ?*ObjString,
    alloc: std.mem.Allocator,
    pub fn init(alloc: std.mem.Allocator, name: ?*ObjString) !*ObjFunction {
        var function = try alloc.create(ObjFunction);
        const chunk = Chunk.init(alloc);
        function.chunk = chunk;
        function.alloc = alloc;
        function.arity = 0;
        function.upvalue_cnt = 0;
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

pub const ObjNative = struct {
    function: NativeFn,
    alloc: std.mem.Allocator,
    fn init(alloc: std.mem.Allocator, function: NativeFn) !*ObjNative {
        const obj_native = try alloc.create(ObjNative);
        obj_native.*.function = function;
        obj_native.*.alloc = alloc;
        return obj_native;
    }

    fn deinit(self: *ObjNative) void {
        self.alloc.destroy(self);
    }
};

pub const ObjClosure = struct {
    function: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalue_cnt: usize,
    allocator: std.mem.Allocator,
    pub fn init(alloc: std.mem.Allocator, function: *ObjFunction) !*ObjClosure {
        const closure = try alloc.create(ObjClosure);
        const upvalues = try alloc.alloc(*ObjUpvalue, function.upvalue_cnt);
        closure.* = .{
            .function = function,
            .upvalues = upvalues,
            .upvalue_cnt = function.upvalue_cnt,
            .allocator = alloc,
        };
        return closure;
    }
    pub fn deinit(self: *ObjClosure) void {
        self.allocator.free(self.upvalues);
        self.allocator.destroy(self);
    }
};

pub const ObjUpvalue = struct {
    location: *Value,
    allocator: std.mem.Allocator,
    pub fn init(alloc: std.mem.Allocator, location: *Value) !*ObjUpvalue {
        const upvalue = try alloc.create(ObjUpvalue);
        upvalue.* = .{
            .location = location,
            .allocator = alloc,
        };
        return upvalue;
    }
    pub fn deinit(self: *ObjUpvalue) void {
        self.allocator.destroy(self);
    }
};
