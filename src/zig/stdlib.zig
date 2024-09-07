const std = @import("std");

const expression = @import("expression.zig");
const functions = @import("stdlib/functions.zig");
const Scope = @import("scope.zig");

pub const Error = error{
    NotImplemented,
    FunctionNotFound,
    BuiltinsNotInitialized,
} || std.mem.Allocator.Error;

const EvalError = functions.Error;

const Expression = expression.Expression;

pub const FunctionContext = struct {
    function: Type,
    arity: u32,

    const Type = *const fn ([]const Expression, *Scope) EvalError!void;
};

// TODO: we may want to prefer a comptime hashmap since
//  all builtin functions are known at compile time
//      (see scala implementation)
//      (see `std.static_string_map.StaticStringMap` in docs)
const BuiltinsHashMap = std.StringHashMap(FunctionContext);

var builtins: ?BuiltinsHashMap = null;

pub fn initBuiltins(allocator: std.mem.Allocator) Error!void {
    builtins = BuiltinsHashMap.init(allocator);

    if (builtins) |*b| {
        try b.put("len", .{ .function = &functions.length, .arity = 1 });
        try b.put("map", .{ .function = &functions.map, .arity = 2 });
        try b.put("reduce", .{ .function = &functions.reduce, .arity = 2 });
        try b.put("print3", .{ .function = &functions.print3, .arity = 1 });
    } else unreachable;
}

pub fn deinitBuiltins() void {
    if (builtins) |b| {
        b.deinit();
    }
}

pub fn getBuiltin(name: []const u8) Error!FunctionContext {
    if (builtins) |b| {
        if (b.get(name)) |fn_ctxt| {
            return fn_ctxt;
        } else return Error.FunctionNotFound;
    } else return Error.BuiltinsNotInitialized;
}
