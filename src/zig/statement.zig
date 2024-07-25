const std = @import("std");
const expr = @import("expression.zig");

pub const Assignment = struct {
    varName: expr.Identifier,
    value: expr.Expression,
};
pub const StructuredAssignment = struct {
    access: expr.StructureAccess,
    value: expr.Expression,
};
pub const Branch = struct {
    condition: expr.Expression,
    body: []const Statement,
};
pub const If = struct {
    ifBranch: Branch,
    elseBranch: ?[]const Statement,
};
pub const WhileLoop = struct { loop: Branch };
pub const Return = struct { value: expr.Expression };

pub const Statement = union(enum) {
    assignment: Assignment,
    struct_assignment: StructuredAssignment,
    if_statement: If,
    whileloop: WhileLoop,
    functioncall: expr.FunctionCall,
    ret: Return,
};
