const std = @import("std");
const stmt = @import("statement.zig");

pub const ArithmeticOps = enum { Add, Sub, Mul, Div, Expo };
pub const BooleanOps = enum { And, Or, Not };
pub const CompareOps = enum { Less, LessEqual, Greater, GreaterEqual, Equal, NotEqual };

pub const Operator = union(enum) {
    arithmetic: ArithmeticOps,
    boolean: BooleanOps,
    compare: CompareOps,
};

pub const BinaryOp = struct {
    left: *Expression,
    right: *Expression,
    op: Operator,
};

pub const UnaryOp = struct {
    operant: *Expression,
    op: Operator,
};

pub const Identifier = struct { name: []const u8 };

pub const Number = union(enum) {
    integer: i64,
    float: f64,

    pub fn eql(self: Number, other: Number) bool {
        if (self == .float and other == .float) {
            return self.float == other.float;
        } else if (self == .integer and other == .integer) {
            return self.integer == other.integer;
        } else if (self == .float) {
            return self.float == @as(f64, @floatFromInt(other.integer));
        } else {
            return other.float == @as(f64, @floatFromInt(self.integer));
        }
    }
};

pub const String = struct { value: []const u8 };
pub const Boolean = struct { value: bool };

pub const FunctionCall = struct {
    func: *const Expression,
    args: []Expression,
};

pub const Function = struct {
    args: []const Identifier,
    body: []const stmt.Statement,
};

pub const StructureAccess = struct {
    strct: *Expression,
    key: *Expression,
};

pub const Array = struct {
    elements: []const Expression,
};

pub const DictionaryEntry = struct {
    key: *Expression,
    value: *Expression,
};
pub const Dictionary = struct {
    entries: []const DictionaryEntry,
};

pub const Expression = union(enum) {
    boolean: Boolean,
    binary_op: BinaryOp,
    unary_op: UnaryOp,
    identifier: Identifier,
    number: Number,
    string: String,
    wrapped: *Expression,
    struct_access: StructureAccess,
    functioncall: FunctionCall,
    function: Function,
    array: Array,
    dictionary: Dictionary,

    pub fn eql(self: Expression, other: Expression) bool {
        _ = other;
        return switch (self) {
            else => unreachable,
        };
    }
};

pub fn identifier(chars: []const u8) Identifier {
    return .{ .name = chars };
}

pub fn mapOp(chars: []const u8) Operator {
    if (std.mem.eql(u8, chars, "+")) return .{ .arithmetic = .Add };
    if (std.mem.eql(u8, chars, "-")) return .{ .arithmetic = .Sub };
    if (std.mem.eql(u8, chars, "*")) return .{ .arithmetic = .Mul };
    if (std.mem.eql(u8, chars, "/")) return .{ .arithmetic = .Div };
    if (std.mem.eql(u8, chars, "^")) return .{ .arithmetic = .Expo };
    if (std.mem.eql(u8, chars, "and")) return .{ .boolean = .And };
    if (std.mem.eql(u8, chars, "or")) return .{ .boolean = .Or };
    if (std.mem.eql(u8, chars, "not")) return .{ .boolean = .Not };
    if (std.mem.eql(u8, chars, "==")) return .{ .compare = .Equal };
    if (std.mem.eql(u8, chars, "!=")) return .{ .compare = .NotEqual };
    if (std.mem.eql(u8, chars, "<=")) return .{ .compare = .LessEqual };
    if (std.mem.eql(u8, chars, ">=")) return .{ .compare = .GreaterEqual };
    if (std.mem.eql(u8, chars, "<")) return .{ .compare = .Less };
    if (std.mem.eql(u8, chars, ">")) return .{ .compare = .Greater };

    unreachable;
}

fn printIdent(out: std.io.AnyWriter, level: u8) !void {
    var l = level;
    while (l > 0) : (l -= 1) {
        try out.print("  ", .{});
    }
}

pub fn printExpression(out: std.io.AnyWriter, expr: Expression, indent: u8) !void {
    switch (expr) {
        .identifier => |id| {
            try printIdent(out, indent);
            try out.print("{s}\n", .{id.name});
        },
        .number => |n| {
            try printIdent(out, indent);
            switch (n) {
                .float => |v| try out.print("{}\n", .{v}),
                .integer => |v| try out.print("{}\n", .{v}),
            }
        },
        .boolean => |b| {
            try printIdent(out, indent);
            try out.print("{}\n", .{b.value});
        },
        .binary_op => |bin| {
            try printIdent(out, indent);
            switch (bin.op) {
                .arithmetic => |op| try out.print("{}\n", .{op}),
                .compare => |op| try out.print("{}\n", .{op}),
                .boolean => |op| try out.print("{}\n", .{op}),
            }
            try printExpression(out, bin.left.*, indent + 1);
            try printExpression(out, bin.right.*, indent + 1);
        },
        else => |ex| try out.print("TODO: {}\n", .{ex}),
    }
}

pub fn free(allocator: std.mem.Allocator, expr: *const Expression) void {
    switch (expr.*) {
        .wrapped => |e| {
            free(allocator, e);
        },
        .unary_op => |u| {
            free(allocator, u.operant);
        },
        .binary_op => |b| {
            free(allocator, b.left);
            free(allocator, b.right);
        },
        .struct_access => |sta| {
            free(allocator, sta.key);
            free(allocator, sta.strct);
        },
        .functioncall => |fc| {
            free(allocator, fc.func);
            allocator.free(fc.args);
        },
        .function => |f| {
            for (f.body) |st| {
                stmt.free(allocator, st);
            }
            allocator.free(f.body);
            allocator.free(f.args);
        },
        .array => |a| {
            allocator.free(a.elements);
        },
        .dictionary => |d| {
            for (d.entries) |*e| {
                free(allocator, e.key);
                free(allocator, e.value);
            }
            allocator.free(d.entries);
        },
        else => {},
    }
    allocator.destroy(expr);
}
