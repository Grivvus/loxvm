const std = @import("std");
const vm_mod = @import("vm.zig");
const build_mode = @import("builtin").mode;
const value_mod = @import("value.zig");
const object_mod = @import("object.zig");
const compiler_mod = @import("compiler.zig");

const VM = vm_mod.VM;
const Context = vm_mod.HashMapContext;
const Value = value_mod.Value;
const Object = object_mod.Object;
const ObjString = object_mod.ObjString;
const markCompilerRoots = compiler_mod.markCompilerRoots;

const DEBUG_STRESS_GC = ((build_mode == .Debug) and true);

pub fn collectGarbage() void {
    if (build_mode == .Debug) {
        std.debug.print("-- gc begin\n", .{});
    }

    if (build_mode == .Debug) {
        std.debug.print("-- gc end\n", .{});
    }
}

pub fn markRoots(vm: *VM) void {
    for (0..vm.stack_top_index) |i| {
        markValue(vm.stack[i]);
    }

    for (0..vm.frame_count) |i| {
        markObject(&vm.frames[i].closure.object);
    }

    var upvalue_iter = vm.open_upvalues;
    while (upvalue_iter != null) {
        markObject(&upvalue_iter.?.object);
        upvalue_iter = upvalue_iter.?.next;
    }

    markTable(vm.globals);

    markCompilerRoots();
}

fn markValue(value: Value) void {
    if (value.isObject()) {
        markObject(value.asObject());
    }
}

pub fn markObject(object: *Object) void {
    if (build_mode == .Debug) {
        std.debug.print("{d} mark ", .{@intFromPtr(object)});
        Value.initObject(object).printValue();
        std.debug.print("\n", .{});
    }
    object.is_marked = true;
}

fn markTable(table: std.HashMap(*ObjString, Value, Context, 80)) void {
    var iter = table.keyIterator();
    while (iter.next()) |key| {
        markObject(key.object);
        markValue(table.get(key.*));
    }
}
