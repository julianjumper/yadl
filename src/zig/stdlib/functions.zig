const std = @import("std");

const expression = @import("../expression.zig");
const interpreter = @import("../interpreter.zig");
const Scope = @import("../scope.zig");
const Expression = expression.Expression;

pub const Error = error{
    NotImplemented,
} || std.mem.Allocator.Error || interpreter.Error;

pub fn length(args: []const Expression, scope: *Scope) Error!void {
    switch (args[0]) {
        .array => |a| {
            scope.return_result = try expression.Number.init(scope.allocator, i64, @intCast(a.elements.len));
        },
        .dictionary => |d| {
            scope.return_result = try expression.Number.init(scope.allocator, i64, @intCast(d.entries.len));
        },
        else => unreachable,
    }
}

pub fn map(args: []const Expression, scope: *Scope) Error!void {
    const elements = args[0];
    const callable = args[1];
    std.debug.assert(callable == .function);

    switch (elements) {
        .array => |a| {
            const tmp = try scope.allocator.alloc(Expression, a.elements.len);
            const func = callable.function;
            for (a.elements, tmp) |e, *t| {
                var tmp2 = try scope.allocator.alloc(Expression, 1);
                tmp2[0] = e;
                var tmpScope = try Scope.init(scope.allocator, scope.out, scope, func.args, tmp2);
                for (func.body) |st| {
                    try interpreter.evalStatement(st, &tmpScope);
                }
                if (tmpScope.result()) |r| {
                    t.* = r.*;
                } else {
                    return Error.ValueNotFound;
                }
            }
            scope.return_result = try expression.Array.init(scope.allocator, tmp);
        },
        else => return Error.NotImplemented,
    }
}

pub fn print3(args: []const Expression, scope: *Scope) Error!void {
    try interpreter.printValue(args[0], scope);
}
