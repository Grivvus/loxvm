const std = @import("std");
const VM = @import("vm.zig").VM;
const Chunk = @import("bytecode.zig").Chunk;
const Value = @import("value.zig").Value;

const assert = std.debug.assert;

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
    is_marked: bool,
    next: ?[*]Object,
    pub fn deinit(self: *Object, allocator: std.mem.Allocator) void {
        switch (self.type_) {
            .OBJ_STRING => self.as(ObjString).deinit(allocator),
            .OBJ_FUNCTION => self.as(ObjFunction).deinit(allocator),
            .OBJ_CLOSURE => self.as(ObjClosure).deinit(allocator),
            .OBJ_NATIVE => self.as(ObjNative).deinit(allocator),
            .OBJ_UPVALUE => self.as(ObjUpvalue).deinit(allocator),
        }
        allocator.destroy(self);
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

    pub fn as(self: *Object, comptime T: type) *T {
        return @fieldParentPtr("object", self);
    }

    pub fn printObject(self: *Object) void {
        switch (self.type_) {
            .OBJ_STRING => std.debug.print("{s}", .{self.as(ObjString).str}),
            .OBJ_FUNCTION => self.as(ObjFunction).print(),
            .OBJ_CLOSURE => self.as(ObjClosure).function.print(),
            .OBJ_NATIVE => std.debug.print("<native fn>", .{}),
            .OBJ_UPVALUE => std.debug.print("upvalue", .{}),
        }
    }
};

pub const ObjString = struct {
    object: Object,
    str: []u8,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        source: []const u8,
    ) *ObjString {
        var obj_str = allocator.create(ObjString) catch {
            @panic("Allocation error");
        };
        const alloc_str = allocator.alloc(u8, source.len) catch {
            @panic("Allocation error");
        };
        std.mem.copyBackwards(u8, alloc_str, source);
        obj_str.str = alloc_str;
        obj_str.object = .{
            .type_ = .OBJ_STRING,
            .next = vm.objects,
            .is_marked = false,
        };
        return obj_str;
    }
    pub fn deinit(self: *ObjString, allocator: std.mem.Allocator) void {
        allocator.free(self.str);
        allocator.destroy(self);
    }
    pub fn hash(self: *ObjString) u64 {
        return std.hash.Murmur2_64.hash(self.str);
    }
};

pub const ObjFunction = struct {
    object: Object,
    arity: u32,
    upvalue_cnt: usize,
    chunk: Chunk,
    name: ?*ObjString,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        name: ?*ObjString,
    ) *ObjFunction {
        var function = allocator.create(ObjFunction) catch {
            @panic("Allocation error");
        };
        const chunk = Chunk.init(allocator);
        function.chunk = chunk;
        function.arity = 0;
        function.upvalue_cnt = 0;
        function.name = name;
        function.object = .{
            .type_ = .OBJ_FUNCTION,
            .next = vm.objects,
            .is_marked = false,
        };
        return function;
    }
    pub fn deinit(self: *ObjFunction, allocator: std.mem.Allocator) void {
        self.chunk.deinit(allocator);
        if (self.name != null) {
            self.name.deinit(allocator);
        }
        allocator.destroy(self);
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
    object: Object,
    function: NativeFn,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        function: NativeFn,
    ) *ObjNative {
        var obj_native = allocator.create(ObjNative) catch {
            @panic("Allocation error");
        };
        obj_native.function = function;
        obj_native.object = .{
            .type_ = .OBJ_NATIVE,
            .next = vm.objects,
            .is_marked = false,
        };

        return obj_native;
    }

    pub fn deinit(self: *ObjNative, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

pub const ObjClosure = struct {
    object: Object,
    function: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalue_cnt: usize,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        function: *ObjFunction,
    ) *ObjClosure {
        const closure = allocator.create(ObjClosure) catch {
            @panic("Allocation error");
        };
        const upvalues = allocator.alloc(*ObjUpvalue, function.upvalue_cnt) catch {
            @panic("Allocation error");
        };
        closure.* = .{
            .function = function,
            .upvalues = upvalues,
            .upvalue_cnt = function.upvalue_cnt,
            .object = .{
                .type_ = .OBJ_CLOSURE,
                .next = vm.objects,
                .is_marked = false,
            },
        };
        return closure;
    }
    pub fn deinit(self: *ObjClosure, allocator: std.mem.Allocator) void {
        allocator.free(self.upvalues);
        allocator.destroy(self);
    }
};

pub const ObjUpvalue = struct {
    object: Object,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        location: *Value,
    ) *ObjUpvalue {
        const upvalue = allocator.create(ObjUpvalue) catch {
            @panic("Allocation error");
        };
        upvalue.* = .{
            .location = location,
            .next = null,
            .closed = undefined,
            .object = .{
                .type_ = .OBJ_UPVALUE,
                .next = vm.objects,
                .is_marked = false,
            },
        };
        return upvalue;
    }
    pub fn deinit(self: *ObjUpvalue, allocator: std.mem.Allocator) void {
        if (self.next != null) {
            self.next.?.deinit(allocator);
        }
        allocator.destroy(self);
    }
};
