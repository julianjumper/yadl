const std = @import("std");

const expression = @import("expression.zig");

pub const Error = error{
    NotImplemented,
    FunctionNotFound,
    BuiltinsNotInitialized,
};

const Expression = expression.Expression;

pub const FunctionContext = struct {
    function: Type,
    arity: u32,

    const Type = *const fn ([]const Expression) Expression;
};

const BuiltinsHashMap = std.StringHashMap(FunctionContext);

var builtins: ?BuiltinsHashMap = null;

pub fn initBuiltins(allocator: std.mem.Allocator) void {
    builtins = BuiltinsHashMap.init(allocator);
    // return Error.NotImplemented;
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
