const std = @import("std");

const expression = @import("../expression.zig");
const Expression = expression.Expression;

pub const Error = error{
    NotImplemented,
};

pub fn length(args: []const Expression) Error!Expression {
    switch (args[0]) {
        .array => |a| {
            return .{ .number = .{ .integer = @intCast(a.elements.len) } };
        },
        .dictionary => |d| {
            return .{ .number = .{ .integer = @intCast(d.entries.len) } };
        },
        else => unreachable,
    }
}
