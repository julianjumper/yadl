const std = @import("std");

const expression = @import("expression.zig");
const liberror = @import("stdlib/error.zig");
const functions = @import("stdlib/functions.zig");
pub const conversions = @import("stdlib/conversions.zig");
const Scope = @import("scope.zig");

pub const Error = error{
    NotImplemented,
    FunctionNotFound,
    BuiltinsNotInitialized,
} || std.mem.Allocator.Error;

const EvalError = liberror.Error;

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
        try b.put("last", .{ .function = &functions.last, .arity = 3 });
        try b.put("first", .{ .function = &functions.first, .arity = 3 });
        try b.put("type", .{ .function = &functions._type, .arity = 1 });

        // conversions
        try b.put("bool", .{ .function = &conversions.toBoolean, .arity = 1 });
        try b.put("number", .{ .function = &conversions.toNumber, .arity = 1 });
        try b.put("string", .{ .function = &conversions.toString, .arity = 1 });

        // iterable operations
        try b.put("map", .{ .function = &functions.map, .arity = 2 });
        try b.put("zip", .{ .function = &functions.zip, .arity = 2 });
        try b.put("reduce", .{ .function = &functions.reduce, .arity = 2 });
        try b.put("count", .{ .function = &functions.count, .arity = 2 });
        try b.put("check_all", .{ .function = &functions.check_all, .arity = 2 });
        try b.put("check_any", .{ .function = &functions.check_any, .arity = 2 });
        try b.put("check_none", .{ .function = &functions.check_none, .arity = 2 });
        try b.put("filter", .{ .function = &functions.filter, .arity = 2 });

        // TODO: we may want to remove this one
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
