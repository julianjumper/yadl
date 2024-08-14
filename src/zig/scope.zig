const std = @import("std");
const stmt = @import("statement.zig");
const expr = @import("expression.zig");

const Expression = expr.Expression;
const Statement = stmt.Statement;

const Bindings = std.StringHashMap(*Expression);
const Functions = std.StringHashMap(expr.Function);

const Scope = @This();

const Error = std.mem.Allocator.Error;

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

pub fn result(self: *Scope) ?*Expression {
    if (self.return_result) |res| {
        self.return_result = null;
        return res;
    } else return null;
}

pub fn isGlobal(self: Scope) bool {
    return if (self.parent) |_| false else true;
}

pub fn lookup(self: Scope, ident: expr.Identifier) Error!?*Expression {
    if (self.locals.get(ident.name)) |ex| {
        return if (ex.* == .identifier and std.mem.eql(u8, ex.identifier.name, ident.name)) ex else null;
    } else {
        return try self.lookupInParent(ident) orelse b: {
            const f = self.lookupFunction(ident) orelse break :b null;
            const out = try self.allocator.create(Expression);
            out.* = .{ .function = f };
            break :b out;
        };
    }
}

pub fn lookupFunction(self: Scope, ident: expr.Identifier) ?expr.Function {
    if (self.functions.get(ident.name)) |func| {
        return func;
    } else {
        return self.lookupFunctionInParent(ident);
    }
}

fn lookupInParent(self: Scope, ident: expr.Identifier) Error!?*Expression {
    if (self.parent) |p| {
        return try p.lookup(ident);
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

pub fn update(self: *Scope, ident: expr.Identifier, value: *Expression) Error!void {
    if (value.* == .function) {
        if (self.functions.get(ident.name)) |func| {
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
