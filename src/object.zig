const std = @import("std");
const VM = @import("vm.zig").VM;
const Chunk = @import("bytecode.zig").Chunk;
const Value = @import("value.zig").Value;
const Table = @import("table.zig").Table;

const build_mode = @import("builtin").mode;

const assert = std.debug.assert;

pub const ObjType = enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_CLOSURE,
    OBJ_NATIVE,
    OBJ_UPVALUE,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_BOUND_METHOD,
};

pub const NativeFn = *const (fn (argc: usize, args: []Value) Value);

pub const Object = struct {
    type_: ObjType,
    is_marked: bool = false,
    next: ?*Object,
    pub fn deinit(self: *Object) usize {
        if (build_mode == .Debug) {
            std.debug.print("{x} deleting ", .{@intFromPtr(self)});
            Value.initObject(self).printValue();
            std.debug.print("\n", .{});
        }
        return switch (self.type_) {
            .OBJ_STRING => self.as(ObjString).deinit(),
            .OBJ_FUNCTION => self.as(ObjFunction).deinit(),
            .OBJ_CLOSURE => self.as(ObjClosure).deinit(),
            .OBJ_NATIVE => self.as(ObjNative).deinit(),
            .OBJ_UPVALUE => self.as(ObjUpvalue).deinit(),
            .OBJ_CLASS => self.as(ObjClass).deinit(),
            .OBJ_INSTANCE => self.as(ObjInstance).deinit(),
            .OBJ_BOUND_METHOD => self.as(ObjBoundMethod).deinit(),
        };
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
    pub fn isObjClass(self: Object) bool {
        return self.type_ == .OBJ_CLASS;
    }
    pub fn isObjInstance(self: Object) bool {
        return self.type_ == .OBJ_INSTANCE;
    }
    pub fn isObjBoundMethod(self: Object) bool {
        return self.type_ == .OBJ_BOUND_METHOD;
    }

    pub fn as(self: *Object, comptime T: type) *T {
        switch (self.type_) {
            .OBJ_STRING => std.debug.assert(self.isObjString()),
            .OBJ_FUNCTION => std.debug.assert(self.isObjFunction()),
            .OBJ_CLOSURE => std.debug.assert(self.isObjClosure()),
            .OBJ_NATIVE => std.debug.assert(self.isObjNative()),
            .OBJ_UPVALUE => std.debug.assert(self.isObjUpvalue()),
            .OBJ_CLASS => std.debug.assert(self.isObjClass()),
            .OBJ_INSTANCE => std.debug.assert(self.isObjInstance()),
            .OBJ_BOUND_METHOD => std.debug.assert(self.isObjBoundMethod()),
        }
        return @alignCast(@fieldParentPtr("object", self));
    }

    pub fn printObject(self: *Object) void {
        switch (self.type_) {
            .OBJ_STRING => std.debug.print("{s}", .{self.as(ObjString).str}),
            .OBJ_FUNCTION => self.as(ObjFunction).print(),
            .OBJ_CLOSURE => self.as(ObjClosure).function.print(),
            .OBJ_NATIVE => std.debug.print("<native fn>", .{}),
            .OBJ_UPVALUE => std.debug.print("upvalue", .{}),
            .OBJ_CLASS => std.debug.print("class <{s}>", .{self.as(ObjClass).name.str}),
            .OBJ_INSTANCE => std.debug.print("instance {d} of class {s}", .{
                @intFromPtr(self.as(ObjInstance)),
                self.as(ObjInstance).class.name.str,
            }),
            .OBJ_BOUND_METHOD => self.as(ObjBoundMethod).method.function.print(),
        }
    }
};

pub const ObjString = struct {
    object: Object,
    str: []u8,
    allocator: std.mem.Allocator,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        source: []const u8,
    ) *ObjString {
        var obj_str = allocator.create(ObjString) catch {
            @panic("Allocation error");
        };
        vm.bytes_allocated += @sizeOf(ObjString);
        vm.bytes_allocated += source.len;
        obj_str.str = allocator.dupe(u8, source) catch {
            @panic("Allocation error");
        };
        obj_str.allocator = allocator;
        obj_str.object = .{
            .type_ = .OBJ_STRING,
            .next = vm.objects,
        };
        vm.objects = &obj_str.object;
        return obj_str;
    }
    pub fn deinit(self: *ObjString) usize {
        const freed = self.str.len + @sizeOf(ObjString);
        if (build_mode == .Debug) {
            for (self.str) |elem| {
                std.debug.print("{d} ", .{elem});
            }
            std.debug.print("\n", .{});
        }
        self.allocator.free(self.str);
        self.allocator.destroy(self);
        return freed;
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
    allocator: std.mem.Allocator,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        name: ?*ObjString,
    ) *ObjFunction {
        var function = allocator.create(ObjFunction) catch {
            @panic("Allocation error");
        };
        const chunk = Chunk.init(allocator);
        vm.bytes_allocated += @sizeOf(ObjFunction);
        function.chunk = chunk;
        function.arity = 0;
        function.upvalue_cnt = 0;
        function.name = name;
        function.object = .{
            .type_ = .OBJ_FUNCTION,
            .next = vm.objects,
        };
        function.allocator = allocator;
        vm.objects = &function.object;
        return function;
    }
    pub fn deinit(self: *ObjFunction) usize {
        const freed = @sizeOf(ObjFunction);
        self.chunk.deinit();
        self.allocator.destroy(self);
        return freed;
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
    allocator: std.mem.Allocator,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        function: NativeFn,
    ) *ObjNative {
        var obj_native = allocator.create(ObjNative) catch {
            @panic("Allocation error");
        };
        obj_native.function = function;
        obj_native.allocator = allocator;
        obj_native.object = .{
            .type_ = .OBJ_NATIVE,
            .next = vm.objects,
        };
        vm.objects = &obj_native.object;
        vm.bytes_allocated += @sizeOf(ObjNative);

        return obj_native;
    }

    pub fn deinit(self: *ObjNative) usize {
        const freed = @sizeOf(ObjNative);
        self.allocator.destroy(self);
        return freed;
    }
};

pub const ObjClosure = struct {
    object: Object,
    function: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalue_cnt: usize,
    allocator: std.mem.Allocator,
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
        vm.bytes_allocated += (@sizeOf(ObjClosure) + (function.upvalue_cnt * @sizeOf(*ObjUpvalue)));
        closure.* = .{
            .function = function,
            .upvalues = upvalues,
            .upvalue_cnt = function.upvalue_cnt,
            .allocator = allocator,
            .object = .{
                .type_ = .OBJ_CLOSURE,
                .next = vm.objects,
            },
        };
        vm.objects = &closure.object;
        return closure;
    }
    pub fn deinit(self: *ObjClosure) usize {
        const freed = @sizeOf(ObjClosure) + (self.upvalue_cnt * @sizeOf(*ObjUpvalue));
        self.allocator.free(self.upvalues);
        self.allocator.destroy(self);
        return freed;
    }
};

pub const ObjUpvalue = struct {
    object: Object,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
    allocator: std.mem.Allocator,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        location: *Value,
    ) *ObjUpvalue {
        const upvalue = allocator.create(ObjUpvalue) catch {
            @panic("Allocation error");
        };
        vm.bytes_allocated += @sizeOf(ObjUpvalue);
        upvalue.* = .{
            .location = location,
            .next = null,
            .closed = Value.initNil(),
            .allocator = allocator,
            .object = .{
                .type_ = .OBJ_UPVALUE,
                .next = vm.objects,
            },
        };
        vm.objects = &upvalue.object;
        return upvalue;
    }
    pub fn deinit(self: *ObjUpvalue) usize {
        const freed = @sizeOf(ObjUpvalue);
        self.allocator.destroy(self);
        return freed;
    }
};

pub const ObjClass = struct {
    object: Object,
    name: *ObjString,
    methods: Table,
    allocator: std.mem.Allocator,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        name: *ObjString,
    ) *ObjClass {
        var class = allocator.create(ObjClass) catch {
            @panic("Allocation error");
        };
        class.allocator = allocator;
        class.name = name;
        class.methods = Table.init(allocator);
        class.object = .{
            .type_ = .OBJ_CLASS,
            .next = vm.objects,
        };
        vm.objects = &class.object;
        vm.bytes_allocated += @sizeOf(ObjClass);
        return class;
    }
    pub fn deinit(self: *ObjClass) usize {
        const freed = @sizeOf(ObjClass);
        self.methods.deinit();
        self.allocator.destroy(self);
        return freed;
    }
};

pub const ObjInstance = struct {
    object: Object,
    class: *ObjClass,
    fields: Table,
    allocator: std.mem.Allocator,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        class: *ObjClass,
    ) *ObjInstance {
        var instance = allocator.create(ObjInstance) catch {
            @panic("Allocation error");
        };
        vm.bytes_allocated += @sizeOf(ObjInstance);
        instance.class = class;
        instance.allocator = allocator;
        instance.fields = Table.init(allocator);
        instance.object = .{
            .type_ = .OBJ_INSTANCE,
            .next = vm.objects,
        };
        vm.objects = &instance.object;
        return instance;
    }
    pub fn deinit(self: *ObjInstance) usize {
        const freed = @sizeOf(ObjInstance);
        self.fields.deinit();
        self.allocator.destroy(self);
        return freed;
    }
};

pub const ObjBoundMethod = struct {
    object: Object,
    reciever: Value,
    method: *ObjClosure,
    allocator: std.mem.Allocator,
    pub fn init(
        vm: *VM,
        allocator: std.mem.Allocator,
        reciever: Value,
        method: *ObjClosure,
    ) *ObjBoundMethod {
        var bound_method = allocator.create(ObjBoundMethod) catch {
            @panic("Allocation error");
        };
        bound_method.allocator = allocator;
        bound_method.reciever = reciever;
        bound_method.method = method;
        bound_method.object = .{
            .type_ = .OBJ_BOUND_METHOD,
            .next = vm.objects,
        };
        vm.bytes_allocated += @sizeOf(ObjBoundMethod);
        vm.objects = &bound_method.object;
        return bound_method;
    }
    pub fn deinit(self: *ObjBoundMethod) usize {
        const freed = @sizeOf(ObjBoundMethod);
        self.allocator.destroy(self);
        return freed;
    }
};
