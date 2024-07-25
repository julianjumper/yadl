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
};

pub const String = struct { value: []const u8 };

pub const FunctionCall = struct {
    func: *Expression,
    args: []const Expression,
};

pub const StructureAccess = struct {
    strct: *Expression,
    key: *Expression,
};

pub const Array = struct {
    elements: []const Expression,
};

const DictionaryEntry = struct {
    key: *Expression,
    value: *Expression,
};
pub const Dictionary = struct {
    entries: []const DictionaryEntry,
};

pub const Expression = union(enum) {
    binary_op: BinaryOp,
    unary_op: UnaryOp,
    identifier: Identifier,
    number: Number,
    string: String,
    wrapped: *Expression,
    struct_access: StructureAccess,
    functioncall: FunctionCall,
    array: Array,
    dictionary: Dictionary,
};
