const std = @import("std");

const expression = @import("../expression.zig");
const Scope = @import("../scope.zig");
const Expression = expression.Expression;

const Error = @import("error.zig").Error;

pub fn toBoolean(args: []const Expression, scope: *Scope) Error!void {
    switch (args[0]) {
        .boolean => |b| scope.return_result = try expression.Boolean.init(scope.allocator, b.value),
        .number => |n| scope.return_result = try switch (n) {
            .integer => |i| expression.Boolean.init(scope.allocator, i != 0),
            .float => |f| expression.Boolean.init(scope.allocator, f != 0.0),
        },
        .array => |a| scope.return_result = try expression.Boolean.init(scope.allocator, a.elements.len != 0),
        .dictionary => |d| scope.return_result = try expression.Boolean.init(scope.allocator, d.entries.len != 0),
        .string => |s| scope.return_result = try expression.Boolean.init(scope.allocator, s.value.len != 0),
        else => return Error.NotImplemented,
    }
}

pub fn toNumber(args: []const Expression, scope: *Scope) Error!void {
    const expr = args[0];
    switch (expr) {
        .boolean => |b| scope.return_result = try expression.Number.init(scope.allocator, i64, @intFromBool(b.value)),
        .number => scope.return_result = try expr.clone(scope.allocator),
        else => return Error.NotImplemented,
    }
}

pub fn toString(args: []const Expression, scope: *Scope) Error!void {
    const expr = args[0];
    switch (args[0]) {
        .boolean => |b| {
            const buffer = try scope.allocator.alloc(u8, 16);
            const out = std.fmt.bufPrint(buffer, "{}", .{b.value}) catch return Error.IOWrite;
            scope.return_result = try expression.String.init(scope.allocator, out);
        },
        .number => |n| switch (n) {
            .integer => |i| {
                const buffer = try scope.allocator.alloc(u8, 16);
                const out = std.fmt.bufPrint(buffer, "{}", .{i}) catch return Error.IOWrite;
                scope.return_result = try expression.String.init(scope.allocator, out);
            },
            .float => |f| {
                const buffer = try scope.allocator.alloc(u8, 16);
                const out = std.fmt.bufPrint(buffer, "{}", .{f}) catch return Error.IOWrite;
                scope.return_result = try expression.String.init(scope.allocator, out);
            },
        },
        // .array => |a| scope.return_result = try expression.Boolean.init(scope.allocator, a.elements.len != 0),
        // .dictionary => |d| scope.return_result = try expression.Boolean.init(scope.allocator, d.entries.len != 0),
        .string => scope.return_result = try expr.clone(scope.allocator),
        else => return Error.NotImplemented,
    }
}
