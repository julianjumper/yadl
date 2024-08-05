const std = @import("std");
const Lexer = @import("lexer.zig");
const stmt = @import("statement.zig");
const expr = @import("expression.zig");

const pError = error{
    UnexpectedToken,
    ArgumentParsingFailure,
    NumberParsingFailure,
};

pub const Error = Lexer.Error || pError;

current_position: u64,
tokens: []const Token,
lexer: Lexer,
allocator: std.mem.Allocator,
last_expected: ?Kind = null,
last_expected_chars: ?[]const u8 = null,

var parser_diagnostic: bool = false;

const Kind = Lexer.TokenKind;
const Token = Lexer.Token;
const Self = @This();

pub fn init(input: []const u8, allocator: std.mem.Allocator) Lexer.Error!Self {
    var tmp = Self{
        .current_position = 0,
        .tokens = undefined,
        .lexer = Lexer.init(input),
        .allocator = allocator,
    };
    // TODO: prefetching all tokens might turn into a memory problem later for huge files
    var ts = std.ArrayList(Token).init(allocator);
    try tmp.lexer.allTokens(&ts);
    tmp.tokens = ts.toOwnedSlice() catch return Lexer.Error.MemoryFailure;
    return tmp;
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.tokens);
}

pub fn freeStatements(self: *Self, stmts: []const stmt.Statement) void {
    for (stmts) |st| {
        self.freeStatement(st);
    }
    self.allocator.free(stmts);
}
pub fn freeStatement(self: *Self, st: stmt.Statement) void {
    switch (st) {
        .ret => |r| {
            self.freeExpression(r.value);
        },
        .whileloop => |w| {
            self.freeExpression(w.loop.condition);
            self.freeStatements(w.loop.body);
        },
        .assignment => |a| {
            self.freeExpression(a.value);
        },
        .if_statement => |i| {
            self.freeExpression(i.ifBranch.condition);
            self.freeStatements(i.ifBranch.body);
            if (i.elseBranch) |b| {
                self.freeStatements(b);
            }
        },
        .functioncall => |fc| {
            for (fc.args) |*arg| {
                self.freeExpression(arg);
            }
            self.freeExpression(fc.func);
        },
        .struct_assignment => |sa| {
            self.freeExpression(sa.value);
            const ex: expr.Expression = .{ .struct_access = sa.access.* };
            self.freeExpression(&ex);
        },
    }
}

pub fn freeExpression(self: *Self, ex: *const expr.Expression) void {
    switch (ex.*) {
        .wrapped => |e| {
            self.freeExpression(e);
        },
        .unary_op => |u| {
            self.freeExpression(u.operant);
        },
        .binary_op => |b| {
            self.freeExpression(b.left);
            self.freeExpression(b.right);
        },
        .struct_access => |sta| {
            self.freeExpression(sta.key);
            self.freeExpression(sta.strct);
        },
        .functioncall => |fc| {
            self.freeExpression(fc.func);
            for (fc.args) |*arg| {
                self.freeExpression(arg);
            }
        },
        .function => |f| {
            self.freeStatements(f.body);
            self.allocator.free(f.args);
        },
        .array => |a| {
            for (a.elements) |*e| {
                self.freeExpression(e);
            }
            self.allocator.free(a.elements);
        },
        .dictionary => |d| {
            for (d.entries) |*e| {
                self.freeExpression(e.key);
                self.freeExpression(e.value);
            }
            self.allocator.free(d.entries);
        },
        else => {},
    }
    self.allocator.destroy(ex);
}

pub fn reset(self: *Self) void {
    self.current_position = 0;
}

fn todo(comptime T: type, msg: []const u8) Error!T {
    std.debug.print("TODO: {s}\n", .{msg});
    return Error.NotImplemented;
}

fn expect(self: *Self, kind: Kind, expected_chars: ?[]const u8) Error!Token {
    if (self.current_position >= self.tokens.len) {
        return Error.EndOfFile;
    }

    const token = self.tokens[self.current_position];
    if (expected_chars) |chars| {
        if (token.kind == kind and std.mem.eql(u8, chars, token.chars)) {
            self.current_position += 1;
            return token;
        } else {
            self.last_expected = kind;
            self.last_expected_chars = chars;
            return Error.UnexpectedToken;
        }
    } else {
        if (token.kind == kind) {
            self.current_position += 1;
            return token;
        } else {
            self.last_expected = kind;
            self.last_expected_chars = null;
            return Error.UnexpectedToken;
        }
    }
}

fn currentToken(self: *Self) ?Token {
    return if (self.current_position < self.tokens.len) self.tokens[self.current_position] else null;
}

fn nextToken(self: *Self) ?Token {
    return if (self.current_position + 1 < self.tokens.len) self.tokens[self.current_position + 1] else null;
}

fn nextNextToken(self: *Self) ?Token {
    return if (self.current_position + 2 < self.tokens.len) self.tokens[self.current_position + 2] else null;
}

fn unexpectedToken(lexer: Lexer, actual: Token, expected: []const Kind, expected_chars: ?[]const u8) Error!void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const out = bw.writer();

    if (expected_chars) |chars| {
        out.print("ERROR: expected token of {any} with value '{s}'\n", .{ expected, chars }) catch return Error.UnknownError;
        out.print("       but got {} with '{s}' at {}:{}\n", .{
            actual.kind,
            actual.chars,
            actual.line,
            actual.column,
        }) catch return Error.UnknownError;
    } else {
        out.print("ERROR: expected token of the kinds {any}\n", .{expected}) catch return Error.UnknownError;
        out.print("       but got {} '{s}' at {}:{}\n", .{
            actual.kind,
            actual.chars,
            actual.line,
            actual.column,
        }) catch return Error.UnknownError;
    }
    try lexer.printContext(out.any(), actual);
    bw.flush() catch return Error.UnknownError;

    return Error.UnexpectedToken;
}

// Expression parsing
fn parseExpression(self: *Self) Error!*expr.Expression {
    const value = try self.parseValue();
    const op = self.expect(.Operator, null) catch |err| {
        if (err == Error.UnexpectedToken or err == Error.EndOfFile)
            return value;
        return err;
    };
    const ex = try self.parseExpression();
    const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
    out.* = .{ .binary_op = .{
        .left = value,
        .right = ex,
        .op = expr.mapOp(op.chars),
    } };
    return out;
}

fn parseBoolean(self: *Self) Error!*expr.Expression {
    const b = self.expect(.Boolean, null) catch unreachable;
    if (std.mem.eql(u8, b.chars, "true")) {
        const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
        out.* = .{ .boolean = .{ .value = true } };
        return out;
    } else if (std.mem.eql(u8, b.chars, "false")) {
        const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
        out.* = .{ .boolean = .{ .value = false } };
        return out;
    }
    unreachable;
}

fn parseValue(self: *Self) Error!*expr.Expression {
    if (self.currentToken()) |token| {
        return switch (token.kind) {
            .Number => self.parseNumber(),
            .Identifier => b: {
                if (self.nextToken()) |t| {
                    if (t.kind == .OpenParen and t.chars[0] == '(') {
                        break :b self.parseFunctionCallExpr();
                    } else if (t.kind == .OpenParen and t.chars[0] == '[') {
                        break :b self.parseStructAccess();
                    }
                    break :b self.parseIdentifier();
                }
                break :b self.parseIdentifier();
            },
            .Boolean => self.parseBoolean(),
            .OpenParen => b: {
                const pos = self.current_position;
                const val = self.parseFunction() catch {
                    self.current_position = pos;
                    _ = self.expect(.OpenParen, "(") catch unreachable;
                    const ex = try self.parseExpression();
                    _ = try self.expect(.CloseParen, ")");
                    const out = self.allocator.create(expr.Expression) catch break :b Error.MemoryFailure;
                    out.* = .{ .wrapped = ex };
                    break :b out;
                };
                break :b val;
            },
            else => |k| b: {
                if (k == .CloseParen) {
                    self.last_expected = .Number;
                    return Error.UnexpectedToken;
                }

                std.debug.print("token type: {}\n", .{k});
                std.debug.print("token value: '{s}'\n", .{token.chars});
                break :b todo(*expr.Expression, "parsing of expressions");
            },
        };
    } else return Error.EndOfFile;
}

fn parseIdentifier(self: *Self) Error!*expr.Expression {
    const id = self.expect(.Identifier, null) catch unreachable;
    const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
    out.* = .{ .identifier = expr.identifier(id.chars) };
    return out;
}

fn parseFunctionCallExpr(self: *Self) Error!*expr.Expression {
    _ = self;
    return todo(*expr.Expression, "parsing of expression function calls");
}

fn parseFunctionArguments(self: *Self) Error![]expr.Identifier {
    var args = std.ArrayList(expr.Identifier).init(self.allocator);
    var id = self.expect(.Identifier, null) catch {
        return Error.ArgumentParsingFailure;
    };
    args.append(.{ .name = id.chars }) catch return Error.MemoryFailure;
    while (self.expect(.ArgSep, null)) |_| {
        id = try self.expect(.Identifier, null);
        args.append(.{ .name = id.chars }) catch return Error.MemoryFailure;
    } else |err| {
        if (err == Error.UnexpectedToken)
            return args.toOwnedSlice() catch Error.MemoryFailure;

        return err;
    }
    return args.toOwnedSlice() catch Error.MemoryFailure;
}

fn parseFunction(self: *Self) Error!*expr.Expression {
    _ = self.expect(.OpenParen, "(") catch unreachable;
    const args: []expr.Identifier = self.parseFunctionArguments() catch &[_]expr.Identifier{};
    _ = try self.expect(.CloseParen, ")");
    _ = try self.expect(.LambdaArrow, null);
    if (self.parseExpression()) |ex| {
        const body = self.allocator.alloc(stmt.Statement, 1) catch return Error.MemoryFailure;
        body[0] = .{ .ret = .{ .value = ex } };
        const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
        out.* = .{ .function = .{ .args = args, .body = body } };
        return out;
    } else |err| {
        if (err != Error.UnexpectedToken) return err;
        const body = try self.parseCodeblock();
        const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
        out.* = .{ .function = .{ .args = args, .body = body } };
        return out;
    }
}

fn parseStructAccess(self: *Self) Error!*expr.Expression {
    _ = self;
    return todo(*expr.Expression, "parsing of structure access");
}

fn baseOf(digits: []const u8) u8 {
    if (digits.len < 3)
        return 10;

    return switch (digits[1]) {
        'x' => 16,
        'o' => 8,
        'b' => 2,
        else => 10,
    };
}

fn parseNumber(self: *Self) Error!*expr.Expression {
    const digits = self.expect(.Number, null) catch unreachable;
    const base = baseOf(digits.chars);

    if (std.mem.count(u8, digits.chars, ".") > 0) {
        var parts = std.mem.split(u8, digits.chars, ".");
        const tmp = parts.next() orelse unreachable;
        const int_part = if (base == 10) tmp else tmp[2..];
        const fraction_part = parts.next() orelse unreachable;

        const int = std.fmt.parseInt(i64, int_part, base) catch return Error.NumberParsingFailure;
        const fraction = std.fmt.parseInt(i64, fraction_part, base) catch return Error.NumberParsingFailure;
        const frac: f64 = @as(f64, @floatFromInt(fraction)) / std.math.pow(f64, @floatFromInt(base), @floatFromInt(fraction_part.len));
        const composite = @as(f64, @floatFromInt(int)) + frac;
        const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
        out.* = .{ .number = .{ .float = composite } };
        return out;
    } else {
        const int_part = if (base == 10) digits.chars else digits.chars[2..];
        const num = std.fmt.parseInt(i64, int_part, base) catch return Error.NumberParsingFailure;
        const out = self.allocator.create(expr.Expression) catch return Error.MemoryFailure;
        out.* = .{ .number = .{ .integer = num } };
        return out;
    }
}

// Statement parsing
fn parseCondition(self: *Self) Error!*expr.Expression {
    _ = try self.expect(.OpenParen, "(");
    const condition = try self.parseExpression();
    _ = try self.expect(.CloseParen, ")");
    return condition;
}

fn parseCodeblock(self: *Self) Error![]stmt.Statement {
    _ = try self.expect(.OpenParen, "{");
    _ = try self.expect(.Newline, null);
    const code = try self.parseStatements(true);
    _ = try self.expect(.CloseParen, "}");
    return code;
}

fn parseIfStatement(self: *Self) Error!stmt.Statement {
    _ = self.expect(.Keyword, "if") catch unreachable;
    const condition = try self.parseCondition();
    const code = try self.parseCodeblock();

    const branch: stmt.Branch = .{
        .condition = condition,
        .body = code,
    };

    _ = self.expect(.Keyword, "else") catch |err| {
        if (err == Error.UnexpectedToken) {
            return .{
                .if_statement = .{
                    .ifBranch = branch,
                    .elseBranch = null,
                },
            };
        }
        return err;
    };

    const elseCode = try self.parseCodeblock();

    return .{ .if_statement = .{
        .ifBranch = branch,
        .elseBranch = elseCode,
    } };
}

fn parseWhileloop(self: *Self) Error!stmt.Statement {
    _ = self.expect(.Keyword, "while") catch unreachable;
    const condition = try self.parseCondition();
    const code = try self.parseCodeblock();
    return stmt.whileloop(condition, code);
}

fn parseReturn(self: *Self) Error!stmt.Statement {
    _ = try self.expect(.Keyword, "return");
    const ex = try self.parseExpression();
    return .{ .ret = .{ .value = ex } };
}

fn parseFunctionCallArguments(self: *Self) Error![]expr.Expression {
    var args = std.ArrayList(expr.Expression).init(self.allocator);
    var ex = self.parseExpression() catch {
        return Error.ArgumentParsingFailure;
    };
    args.append(ex.*) catch return Error.MemoryFailure;
    self.allocator.destroy(ex);
    while (self.expect(.ArgSep, null)) |_| {
        ex = try self.parseExpression();
        args.append(ex.*) catch return Error.MemoryFailure;
        self.allocator.destroy(ex);
    } else |err| {
        if (err == Error.UnexpectedToken)
            return args.toOwnedSlice() catch Error.MemoryFailure;

        return err;
    }
    return args.toOwnedSlice() catch Error.MemoryFailure;
}

fn parseFunctionCall(self: *Self) Error!stmt.Statement {
    const func_name = try self.expect(.Identifier, null);
    _ = try self.expect(.OpenParen, "(");

    const args = self.parseFunctionCallArguments() catch |err| {
        if (err == Error.ArgumentParsingFailure)
            return .{ .functioncall = .{
                .func = &.{ .identifier = .{ .name = func_name.chars } },
                .args = &[_]expr.Expression{},
            } };
        return err;
    };

    _ = try self.expect(.CloseParen, ")");

    return .{ .functioncall = .{
        .func = &.{ .identifier = .{ .name = func_name.chars } },
        .args = args,
    } };
}

fn parseAssignment(self: *Self) Error!stmt.Statement {
    const id = self.expect(.Identifier, null) catch unreachable;
    _ = try self.expect(.Operator, "=");
    const expression = try self.parseExpression();
    return stmt.assignment(id.chars, expression);
}

fn parseStructAssignment(self: *Self) Error!stmt.Statement {
    _ = self;
    return Error.NotImplemented;
}

fn parseStatement(self: *Self) Error!stmt.Statement {
    self.last_expected = null;
    self.last_expected_chars = null;
    if (self.currentToken()) |token| {
        const st = try switch (token.kind) {
            .Keyword => if (token.chars[0] == 'r')
                self.parseReturn()
            else if (token.chars[0] == 'i')
                self.parseIfStatement()
            else
                self.parseWhileloop(),
            .Identifier => if (self.nextToken()) |t| (if (t.kind == .OpenParen and t.chars[0] == '(')
                self.parseFunctionCall()
            else if (t.kind == .OpenParen and t.chars[0] == '[')
                self.parseStructAssignment()
            else
                self.parseAssignment()) else Error.UnexpectedToken,
            .OpenParen => todo(stmt.Statement, "parse statement => case open paren"),
            .Newline => b: {
                _ = self.expect(.Newline, null) catch unreachable;
                break :b self.parseStatement();
            },
            else => Error.UnexpectedToken,
        };
        _ = self.expect(.Newline, null) catch |err| {
            if (err == Error.EndOfFile) {
                return st;
            } else return err;
        };
        return st;
    } else return Error.EndOfFile;

    unreachable;
}

fn parseStatements(self: *Self, should_ignore_paran: bool) Error![]stmt.Statement {
    var stmts = std.ArrayList(stmt.Statement).init(self.allocator);
    while (self.parseStatement()) |statement| {
        stmts.append(statement) catch return Error.MemoryFailure;
    } else |err| {
        if (Self.parser_diagnostic) {
            std.debug.print("INFO: error: {}\n", .{err});
            std.debug.print("INFO: current token index: {}\n", .{self.current_position});
            std.debug.print("INFO: total token conut: {}\n", .{self.tokens.len});
        }

        if (err != Error.EndOfFile and err != Error.UnexpectedToken)
            return err;

        if (err == Error.UnexpectedToken and !should_ignore_paran) {
            const token = self.currentToken() orelse unreachable;
            if (std.mem.eql(u8, token.chars, "}") and should_ignore_paran)
                return stmts.toOwnedSlice() catch Error.MemoryFailure;

            if (self.last_expected) |kind| {
                try unexpectedToken(
                    self.lexer,
                    token,
                    &[_]Lexer.TokenKind{kind},
                    self.last_expected_chars,
                );
            } else {
                try unexpectedToken(
                    self.lexer,
                    token,
                    &[_]Lexer.TokenKind{ .Identifier, .Keyword },
                    null,
                );
            }
        }
    }
    return stmts.toOwnedSlice() catch Error.MemoryFailure;
}

/// Returns an owned slice of Statements which must be
/// freed with `fn freeStatements(Self, []Statement)`
pub fn parse(self: *Self) Error![]stmt.Statement {
    return self.parseStatements(false);
}

test "simple assignment" {
    const input =
        \\aoeu = aoeu
        \\
    ;
    var ident = .{ .identifier = .{ .name = "aoeu" } };
    const expected: stmt.Statement = .{ .assignment = .{
        .varName = .{ .name = "aoeu" },
        .value = &ident,
    } };

    var parser = try Self.init(input, std.testing.allocator);
    const result = try parser.parse();
    defer parser.deinit();
    defer parser.freeStatements(result);

    try std.testing.expectEqual(1, result.len);
    const result_stmt = result[0];
    try std.testing.expect(result_stmt == .assignment);
    try std.testing.expectEqualStrings(expected.assignment.varName.name, result_stmt.assignment.varName.name);
    try std.testing.expect(result_stmt.assignment.value.* == .identifier);
    try std.testing.expectEqualStrings(expected.assignment.value.identifier.name, result_stmt.assignment.value.identifier.name);
}

test "assignment of function" {
    const input =
        \\aoeu = (x) => {
        \\    return x
        \\}
        \\
    ;
    var ident = .{ .identifier = .{ .name = "x" } };
    const ret_value = .{ .value = &ident };
    var fun = .{ .function = .{
        .args = &[_]expr.Identifier{.{ .name = "x" }},
        .body = &[_]stmt.Statement{
            .{ .ret = ret_value },
        },
    } };
    const expected: stmt.Statement = .{ .assignment = .{
        .varName = .{ .name = "aoeu" },
        .value = &fun,
    } };

    var parser = try Self.init(input, std.testing.allocator);
    const result = try parser.parse();
    defer parser.deinit();
    defer parser.freeStatements(result);

    try std.testing.expectEqual(1, result.len);
    const result_stmt = result[0];
    try std.testing.expect(result_stmt == .assignment);
    try std.testing.expectEqualStrings(expected.assignment.varName.name, result_stmt.assignment.varName.name);
    try std.testing.expect(result_stmt.assignment.value.* == .function);
    const function = result_stmt.assignment.value.function;
    try std.testing.expectEqual(expected.assignment.value.function.args.len, function.args.len);
    try std.testing.expectEqual(expected.assignment.value.function.body.len, function.body.len);
}

test "simple assignment - no newline" {
    const input = "aoeu = aoeu";
    var ident = .{ .identifier = .{ .name = "aoeu" } };
    const expected: stmt.Statement = .{ .assignment = .{
        .varName = .{ .name = "aoeu" },
        .value = &ident,
    } };

    var parser = try Self.init(input, std.testing.allocator);
    const result = try parser.parse();
    defer parser.deinit();
    defer parser.freeStatements(result);

    try std.testing.expectEqual(1, result.len);
    const result_stmt = result[0];
    try std.testing.expect(result_stmt == .assignment);
    try std.testing.expectEqualStrings(expected.assignment.varName.name, result_stmt.assignment.varName.name);
    try std.testing.expect(result_stmt.assignment.value.* == .identifier);
    try std.testing.expectEqualStrings(expected.assignment.value.identifier.name, result_stmt.assignment.value.identifier.name);
}

test "simple if statement" {
    const input =
        \\if (true) {
        \\    aoeu = aoeu
        \\}
        \\
    ;
    var cond = .{ .boolean = .{ .value = true } };
    var ident = .{ .identifier = .{ .name = "aoeu" } };
    const ass = .{ .assignment = .{
        .varName = .{ .name = "aoeu" },
        .value = &ident,
    } };
    const branch: stmt.Branch = .{
        .condition = &cond,
        .body = &[_]stmt.Statement{
            ass,
        },
    };
    const expected: stmt.Statement = .{ .if_statement = .{
        .ifBranch = branch,
        .elseBranch = null,
    } };

    var parser = try Self.init(input, std.testing.allocator);
    const result = try parser.parse();
    defer parser.deinit();
    defer parser.freeStatements(result);

    try std.testing.expectEqual(1, result.len);
    const result_stmt = result[0];
    try std.testing.expect(result_stmt == .if_statement);

    const expected_branch = expected.if_statement.ifBranch;
    const result_branch = result_stmt.if_statement.ifBranch;

    try std.testing.expect(result_branch.condition.* == .boolean);
    try std.testing.expectEqual(expected_branch.condition.boolean.value, result_branch.condition.boolean.value);

    const expected_body_statement = expected_branch.body[0];
    const result_body_statement = result_branch.body[0];

    try std.testing.expect(result_body_statement == .assignment);
    try std.testing.expectEqualStrings(expected_body_statement.assignment.value.identifier.name, result_body_statement.assignment.value.identifier.name);
    try std.testing.expectEqualStrings(expected_body_statement.assignment.varName.name, result_body_statement.assignment.varName.name);
}
