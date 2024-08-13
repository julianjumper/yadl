const std = @import("std");
const stmt = @import("statement.zig");
const expr = @import("expression.zig");

const Expression = expr.Expression;
const Statement = stmt.Statement;

const Bindings = std.StringHashMap(*Expression);
const Functions = std.StringHashMap(expr.Function);

const Scope = @This();

allocator: std.mem.Allocator,
parent: ?*Scope = null,
locals: Bindings,
functions: Functions,
return_result: ?*Expression = null,

pub fn empty(alloc: std.mem.Allocator) Scope {
    return .{
        .allocator = alloc,
        .locals = Bindings.init(alloc),
        .functions = Functions.init(alloc),
    };
}

pub fn init(alloc: std.mem.Allocator, parent: *Scope, vars: []const expr.Identifier, exprs: []const Expression) !Scope {
    var tmp: Scope = .{
        .parent = parent,
        .locals = Bindings.init(alloc),
        .functions = Functions.init(alloc),
    };
    for (vars, exprs) |v, e| {
        try tmp.locals.put(v.name, e);
    }
    return tmp;
}

pub fn hasResult(self: Scope) bool {
    return if (self.return_result) |_| true else false;
}

pub fn isGlobal(self: Scope) bool {
    return if (self.parent) |_| false else true;
}

pub fn lookup(self: Scope, ident: expr.Identifier) ?Expression {
    if (self.locals.get(ident.name)) |ex| {
        return if (ex != .{ .identifier = ident }) ex else null;
    } else {
        return self.lookupInParent(ident) orelse self.lookupFunction(ident);
    }
}

pub fn lookupFunction(self: Scope, ident: expr.Identifier) ?expr.Function {
    if (self.functions.get(ident.name)) |func| {
        return func;
    } else {
        return self.lookupFunctionInParent(ident);
    }
}

fn lookupInParent(self: Scope, ident: expr.Identifier) ?Expression {
    if (self.parent) |p| {
        return p.lookup(ident);
    } else return null;
}

fn lookupFunctionInParent(self: Scope, ident: expr.Identifier) ?expr.Function {
    if (self.parent) |p| {
        if (p.functions.get(ident.name)) |func| {
            return func;
        } else {
            return p.lookupFunctionInParent(ident);
        }
    } else return null;
}

pub fn update(self: Scope, ident: expr.Identifier, value: *Expression) !void {
    if (value == .function) {
        if (self.functions.get(ident)) |func| {
            _ = func;
            std.debug.print("TODO: updating function in scope", .{});
            unreachable;
        } else {
            try self.functions.put(ident.name, value.function);
        }
    } else {
        try self.locals.put(ident.name, value);
    }
}
