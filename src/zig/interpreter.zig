const std = @import("std");
const stmt = @import("statement.zig");
const expr = @import("expression.zig");

const Scope = @import("scope.zig");

const Expression = expr.Expression;
const Statement = stmt.Statement;

pub fn evalStatement(statement: Statement, scope: *Scope) !void {
    if (scope.hasResult()) {
        return;
    }

    switch (statement) {
        .assignment => |a| {
            try evalExpression(a.value, scope);
            try scope.update(a.varName, scope.result() orelse unreachable);
        },
        else => |st| {
            std.debug.print("TODO: unhandled case in eval expr: {}\n", .{st});
            unreachable;
        },
    }
}

fn evalExpression(value: *Expression, scope: *Scope) !void {
    switch (value.*) {
        .identifier => |id| {
            const v = try scope.lookup(id) orelse {
                std.debug.print("ERROR: no value found for '{s}'\n", .{id.name});
                std.debug.assert(false);
                unreachable;
            };
            scope.return_result = v;
        },
        else => |v| {
            std.debug.print("TODO: unhandled case in eval expr: {}\n", .{v});
            unreachable;
        },
    }
}
