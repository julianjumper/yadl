const std = @import("std");

pub const ArithmeticOps = enum { Add, Sub, Mul, Div, Expo };
pub const BooleanOps = enum { And, Or, Not };
pub const CompareOps = enum { Less, LessEqual, Greater, GreaterEqual, Equal, NotEqual };

pub const Operator = union(enum) {
    arithmetic: ArithmeticOps,
    boolean: BooleanOps,
    compare: CompareOps,
};

pub const BinaryOp = struct {
    left: *const Expression,
    right: *const Expression,
    op: Operator,
};

pub const UnaryOp = struct {
    operant: *const Expression,
    op: Operator,
};

pub const Identifier = struct { name: []const u8 };

pub const Number = union(enum) {
    integer: i64,
    float: f64,
};

pub const String = struct { value: []const u8 };
pub const Boolean = struct { value: bool };

pub const FunctionCall = struct {
    func: *const Expression,
    args: []const Expression,
};

pub const StructureAccess = struct {
    strct: *const Expression,
    key: *const Expression,
};

pub const Array = struct {
    elements: []const Expression,
};

const DictionaryEntry = struct {
    key: *const Expression,
    value: *const Expression,
};
pub const Dictionary = struct {
    entries: []const DictionaryEntry,
};

pub const Expression = union(enum) {
    binary_op: BinaryOp,
    unary_op: UnaryOp,
    boolean: Boolean,
    identifier: Identifier,
    number: Number,
    string: String,
    wrapped: *Expression,
    struct_access: StructureAccess,
    functioncall: FunctionCall,
    array: Array,
    dictionary: Dictionary,
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
