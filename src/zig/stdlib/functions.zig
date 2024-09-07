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
            scope.return_result = try expression.Number.init(
                scope.allocator,
                i64,
                @intCast(a.elements.len),
            );
        },
        .dictionary => |d| {
            scope.return_result = try expression.Number.init(
                scope.allocator,
                i64,
                @intCast(d.entries.len),
            );
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
                defer scope.allocator.free(tmp2);
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

pub fn reduce(args: []const Expression, scope: *Scope) Error!void {
    const elements = args[0];
    const callable = args[1];
    std.debug.assert(callable == .function);
    if (callable.function.args.len != 2) {
        std.debug.print("ERROR: the provided function has {} arguments\n", .{callable.function.args.len});
        std.debug.print("   needed are {}\n", .{2});
        std.process.exit(1);
    }
    const func = callable.function;

    switch (elements) {
        .array => |a| {
            var acc = a.elements[0];
            for (a.elements[1..]) |e| {
                var tmp = try scope.allocator.alloc(Expression, 2);
                tmp[0] = acc;
                tmp[1] = e;
                var tmpScope = try Scope.init(scope.allocator, scope.out, scope, func.args, tmp);
                for (func.body) |st| {
                    try interpreter.evalStatement(st, &tmpScope);
                }
                if (tmpScope.result()) |r| {
                    acc = r.*;
                } else {
                    return Error.ValueNotFound;
                }
                scope.allocator.free(tmp);
            }
            const out = try scope.allocator.create(Expression);
            out.* = acc;
            scope.return_result = out;
        },
        else => return Error.NotImplemented,
    }
}

const Context = struct {
    operation: *const fn (OutType, bool) OutType,
    initial: OutType,

    const OutType = union(enum) {
        number: i64,
        boolean: bool,
    };
};

fn count_op(acc: Context.OutType, value: bool) Context.OutType {
    std.debug.assert(acc == .number);
    return .{ .number = acc.number + @intFromBool(value) };
}
const Count_context: Context = .{
    .operation = &count_op,
    .initial = .{ .number = 0 },
};

fn check_all_op(acc: Context.OutType, value: bool) Context.OutType {
    std.debug.assert(acc == .boolean);
    return .{ .boolean = acc.boolean and value };
}
const All_context: Context = .{
    .operation = &check_all_op,
    .initial = .{ .boolean = true },
};

fn check_any_op(acc: Context.OutType, value: bool) Context.OutType {
    std.debug.assert(acc == .boolean);
    return .{ .boolean = acc.boolean or value };
}
const Any_context: Context = .{
    .operation = &check_any_op,
    .initial = .{ .boolean = false },
};

fn check_none_op(acc: Context.OutType, value: bool) Context.OutType {
    std.debug.assert(acc == .boolean);
    return .{ .boolean = acc.boolean and !value };
}
const None_context: Context = .{
    .operation = &check_none_op,
    .initial = .{ .boolean = true },
};

fn check(context: Context, args: []const Expression, scope: *Scope) Error!void {
    const elements = args[0];
    const callable = args[1];

    std.debug.assert(callable == .function);
    if (callable.function.args.len != 1) {
        std.debug.print("ERROR: the provided function has {} arguments\n", .{callable.function.args.len});
        std.debug.print("   needed are {}\n", .{1});
        std.process.exit(1);
    }
    const func = callable.function;

    switch (elements) {
        .array => |a| {
            var acc: Context.OutType = context.initial;
            for (a.elements) |e| {
                var tmp = try scope.allocator.alloc(Expression, 1);
                tmp[0] = e;
                var tmpScope = try Scope.init(scope.allocator, scope.out, scope, func.args, tmp);
                for (func.body) |st| {
                    try interpreter.evalStatement(st, &tmpScope);
                }
                if (tmpScope.result()) |r| {
                    if (r.* == .boolean) {
                        acc = context.operation(acc, r.boolean.value);
                    } else if (r.* != .boolean) {
                        std.debug.print("ERROR: returned value of function in count is not a boolean\n", .{});
                        return Error.InvalidExpressoinType;
                    }
                } else {
                    return Error.ValueNotFound;
                }
                scope.allocator.free(tmp);
            }
            scope.return_result = switch (acc) {
                .boolean => |v| try expression.Boolean.init(scope.allocator, v),
                .number => |v| try expression.Number.init(scope.allocator, i64, v),
            };
        },
        else => return Error.NotImplemented,
    }
}

pub fn count(args: []const Expression, scope: *Scope) Error!void {
    try check(Count_context, args, scope);
}

pub fn check_all(args: []const Expression, scope: *Scope) Error!void {
    try check(All_context, args, scope);
}

pub fn check_any(args: []const Expression, scope: *Scope) Error!void {
    try check(Any_context, args, scope);
}

pub fn check_none(args: []const Expression, scope: *Scope) Error!void {
    try check(None_context, args, scope);
}

pub fn print3(args: []const Expression, scope: *Scope) Error!void {
    try interpreter.printValue(args[0], scope);
}
