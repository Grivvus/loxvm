const std = @import("std");
const vm_mod = @import("vm.zig");
const build_mode = @import("builtin").mode;
const value_mod = @import("value.zig");
const object_mod = @import("object.zig");
const compiler_mod = @import("compiler.zig");
const table_mod = @import("table.zig");

const VM = vm_mod.VM;
const Table = table_mod.Table;
const Value = value_mod.Value;
const ValueArray = value_mod.ValueArray;
const Object = object_mod.Object;
const ObjString = object_mod.ObjString;
const ObjUpvalue = object_mod.ObjUpvalue;
const ObjClosure = object_mod.ObjClosure;
const ObjFunction = object_mod.ObjFunction;
const ObjClass = object_mod.ObjClass;
const ObjInstance = object_mod.ObjInstance;
const ObjBoundMethod = object_mod.ObjBoundMethod;
const markCompilerRoots = compiler_mod.markCompilerRoots;

pub const DEBUG_STRESS_GC = ((build_mode == .Debug) and false);
const GC_HEAP_GROW_FACTOR = if (DEBUG_STRESS_GC) 1 else 2;

pub fn collectGarbage(vm: *VM) void {
    if (build_mode == .Debug) {
        std.debug.print("-- gc begin\n", .{});
    }

    markRoots(vm);
    traceReference(vm);
    sweep(vm);

    vm.next_gc *= GC_HEAP_GROW_FACTOR;

    if (build_mode == .Debug) {
        std.debug.print("-- gc end\n", .{});
    }
}

pub fn markRoots(vm: *VM) void {
    for (0..vm.stack_top_index) |i| {
        markValue(vm, vm.stack[i]);
    }

    for (0..vm.frame_count) |i| {
        markObject(vm, &vm.frames[i].closure.object);
    }

    var upvalue_iter = vm.open_upvalues;
    while (upvalue_iter != null) {
        markObject(vm, &upvalue_iter.?.object);
        upvalue_iter = upvalue_iter.?.next;
    }

    markObject(vm, &vm.init_str.object);

    markTable(vm, &vm.globals);

    markCompilerRoots();
}

fn markValue(vm: *VM, value: Value) void {
    if (value.isObject()) {
        markObject(vm, value.asObject());
    }
}

pub fn markObject(vm: *VM, object: *Object) void {
    if (object.is_marked) {
        return;
    }
    if (build_mode == .Debug) {
        std.debug.print("{x} mark ", .{@intFromPtr(object)});
        Value.initObject(object).printValue();
        std.debug.print("\n", .{});
    }
    object.is_marked = true;
    vm.gray_stack.append(object) catch {
        @panic("Allocation error");
    };
}

fn markTable(vm: *VM, table: *Table) void {
    var iter = table.keyIterator();
    while (iter.next()) |key| {
        markObject(vm, &key.*.object);
        markValue(vm, table.get(key.*).?);
    }
}

fn markArray(vm: *VM, array: ValueArray) void {
    for (0..array.values.items.len) |i| {
        markValue(vm, array.values.items[i]);
    }
}

fn traceReference(vm: *VM) void {
    while (vm.gray_stack.pop()) |elem| {
        blackenObject(vm, elem);
    }
}

fn blackenObject(vm: *VM, object: *Object) void {
    if (build_mode == .Debug) {
        std.debug.print("{x} blacken ", .{@intFromPtr(object)});
        Value.initObject(object).printValue();
        std.debug.print("\n", .{});
    }
    switch (object.type_) {
        .OBJ_UPVALUE => {
            const upvalue: *ObjUpvalue = @fieldParentPtr("object", object);
            markValue(vm, upvalue.closed);
        },
        .OBJ_FUNCTION => {
            const function: *ObjFunction = @fieldParentPtr("object", object);
            if (function.name != null) {
                markObject(vm, &function.name.?.object);
            }
            markArray(vm, function.chunk.constants);
        },
        .OBJ_CLOSURE => {
            const closure: *ObjClosure = @fieldParentPtr("object", object);
            markObject(vm, &closure.function.object);
            for (0..closure.upvalue_cnt) |i| {
                markObject(vm, &closure.upvalues[i].object);
            }
        },
        .OBJ_CLASS => {
            const class: *ObjClass = @fieldParentPtr("object", object);
            markObject(vm, &class.name.object);
            markTable(vm, &class.methods);
        },
        .OBJ_INSTANCE => {
            const instance: *ObjInstance = @fieldParentPtr("object", object);
            markObject(vm, &instance.class.object);
            markTable(vm, &instance.fields);
        },
        .OBJ_BOUND_METHOD => {
            const bound_method: *ObjBoundMethod = @fieldParentPtr("object", object);
            markObject(vm, &bound_method.method.object);
            markValue(vm, bound_method.reciever);
        },
        .OBJ_STRING, .OBJ_NATIVE => {},
    }
}

fn sweep(vm: *VM) void {
    var prev: ?*Object = null;
    var curr: ?*Object = vm.objects;
    while (curr) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            prev = obj;
            curr = obj.next;
        } else {
            const next = obj.next;
            if (prev) |p| {
                p.next = next;
            } else {
                vm.objects = next;
            }
            const freed = obj.deinit();
            vm.bytes_allocated -= freed;
            curr = next;
        }
    }
}
