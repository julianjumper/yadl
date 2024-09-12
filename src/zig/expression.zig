const std = @import("std");
const stmt = @import("statement.zig");

pub const ArithmeticOps = enum { Add, Sub, Mul, Div, Expo, Mod };
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

    pub fn init(alloc: std.mem.Allocator, op: Operator, l: *Expression, r: *Expression) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .binary_op = BinaryOp{
            .op = op,
            .left = l,
            .right = r,
        } };
        return out;
    }
};

pub const UnaryOp = struct {
    operant: *Expression,
    op: Operator,

    pub fn init(alloc: std.mem.Allocator, op: Operator, operant: *Expression) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .unary_op = UnaryOp{
            .op = op,
            .operant = operant,
        } };
        return out;
    }
};

pub const Identifier = struct {
    name: []const u8,

    pub fn init(
        alloc: std.mem.Allocator,
        name: []const u8,
    ) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .identifier = Identifier{
            .name = name,
        } };
        return out;
    }
};

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

    pub fn add(self: Number, other: Number) Number {
        if (self == .float and other == .float) {
            return Number{ .float = self.float + other.float };
        } else if (self == .integer and other == .integer) {
            return Number{ .integer = self.integer + other.integer };
        } else if (self == .float) {
            return Number{ .float = self.float + @as(f64, @floatFromInt(other.integer)) };
        } else return Number{ .float = @as(f64, @floatFromInt(self.integer)) + other.float };
    }

    pub fn sub(self: Number, other: Number) Number {
        if (self == .float and other == .float) {
            return Number{ .float = self.float - other.float };
        } else if (self == .integer and other == .integer) {
            return Number{ .integer = self.integer - other.integer };
        } else if (self == .float) {
            return Number{ .float = self.float - @as(f64, @floatFromInt(other.integer)) };
        } else return Number{ .float = @as(f64, @floatFromInt(self.integer)) - other.float };
    }

    pub fn mul(self: Number, other: Number) Number {
        if (self == .float and other == .float) {
            return Number{ .float = self.float * other.float };
        } else if (self == .integer and other == .integer) {
            return Number{ .integer = self.integer * other.integer };
        } else if (self == .float) {
            return Number{ .float = self.float * @as(f64, @floatFromInt(other.integer)) };
        } else return Number{ .float = @as(f64, @floatFromInt(self.integer)) * other.float };
    }

    pub fn expo(self: Number, other: Number) Number {
        if (self == .float and other == .float) {
            const tmp = std.math.pow(f64, self.float, other.float);
            return Number{ .float = tmp };
        } else if (self == .integer and other == .integer) {
            const tmp = std.math.pow(i64, self.integer, other.integer);
            return Number{ .integer = tmp };
        } else if (self == .float) {
            const tmp = std.math.pow(f64, self.float, @as(f64, @floatFromInt(other.integer)));
            return Number{ .float = tmp };
        } else {
            const tmp = std.math.pow(f64, @floatFromInt(self.integer), other.float);
            return Number{ .float = tmp };
        }
    }

    pub fn init(alloc: std.mem.Allocator, comptime T: type, value: T) !*Expression {
        std.debug.assert(T == f64 or T == i64);
        const out = try alloc.create(Expression);
        if (T == f64) {
            out.* = .{ .number = Number{
                .float = value,
            } };
        } else if (T == i64) {
            out.* = .{ .number = Number{
                .integer = value,
            } };
        } else unreachable;
        return out;
    }
};

pub const String = struct {
    value: []const u8,

    pub fn init(alloc: std.mem.Allocator, value: []const u8) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .string = String{
            .value = value,
        } };
        return out;
    }
};
pub const Boolean = struct {
    value: bool,

    pub fn init(alloc: std.mem.Allocator, value: bool) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .boolean = Boolean{
            .value = value,
        } };
        return out;
    }
};

pub const FunctionCall = struct {
    func: *const Expression,
    args: []Expression,

    pub fn init(
        alloc: std.mem.Allocator,
        func: *const Expression,
        args: []Expression,
    ) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .functioncall = FunctionCall{
            .args = args,
            .func = func,
        } };
        return out;
    }
};

pub const Function = struct {
    args: []const Identifier,
    body: []const stmt.Statement,

    pub fn init(
        alloc: std.mem.Allocator,
        args: []const Identifier,
        body: []const stmt.Statement,
    ) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .function = Function{
            .args = args,
            .body = body,
        } };
        return out;
    }
};

pub const StructureAccess = struct {
    strct: *Expression,
    key: *Expression,

    pub fn init(
        alloc: std.mem.Allocator,
        strct: *Expression,
        key: *Expression,
    ) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .struct_access = StructureAccess{
            .key = key,
            .strct = strct,
        } };
        return out;
    }
};

pub const Array = struct {
    elements: []const Expression,

    pub fn init(
        alloc: std.mem.Allocator,
        elements: []const Expression,
    ) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .array = Array{
            .elements = elements,
        } };
        return out;
    }
};

pub const DictionaryEntry = struct {
    key: *Expression,
    value: *Expression,
};
pub const Dictionary = struct {
    entries: []const DictionaryEntry,

    pub fn init(
        alloc: std.mem.Allocator,
        entries: []const DictionaryEntry,
    ) !*Expression {
        const out = try alloc.create(Expression);
        out.* = .{ .dictionary = Dictionary{
            .entries = entries,
        } };
        return out;
    }
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
        return switch (self) {
            .number => |n| if (other == .number) n.eql(other.number) else false,
            else => unreachable,
        };
    }

    pub fn clone(self: Expression, alloc: std.mem.Allocator) !*Expression {
        return switch (self) {
            .number => |n| if (n == .integer) Number.init(alloc, i64, n.integer) else Number.init(alloc, f64, n.float),
            else => std.mem.Allocator.Error.OutOfMemory,
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
    if (std.mem.eql(u8, chars, "%")) return .{ .arithmetic = .Mod };
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
