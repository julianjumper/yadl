const std = @import("std");
const Lexer = @import("lexer.zig");
const stmt = @import("statement.zig");
const expr = @import("expression.zig");

const pError = error{
    UnexpectedToken,
    ArgumentParsingFailure,
    NumberParsingFailure,
};

pub const ParserError = Lexer.LexerError || pError;

current_position: u64,
tokens: []const Lexer.Token,
lexer: Lexer,
allocator: std.mem.Allocator,
last_expected: ?Lexer.TokenKind = null,
last_expected_chars: ?[]const u8 = null,

var parser_diagnostic: bool = false;

const Kind = Lexer.TokenKind;
const Token = Lexer.Token;
const Self = @This();

pub fn init(input: []const u8, allocator: std.mem.Allocator) Lexer.LexerError!Self {
    var tmp = Self{
        .current_position = 0,
        .tokens = undefined,
        .lexer = Lexer.init(input),
        .allocator = allocator,
    };
    var ts = std.ArrayList(Lexer.Token).init(allocator);
    try tmp.lexer.allTokens(&ts);
    tmp.tokens = ts.toOwnedSlice() catch return Lexer.LexerError.MemoryFailure;
    return tmp;
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.tokens);
}

pub fn reset(self: *Self) void {
    self.current_position = 0;
}

fn todo(comptime T: type, msg: []const u8) ParserError!T {
    std.debug.print("TODO: {s}\n", .{msg});
    return ParserError.NotImplemented;
}

fn expect(self: *Self, kind: Kind, expected_chars: ?[]const u8) ParserError!Token {
    if (self.current_position >= self.tokens.len) {
        return ParserError.EndOfFile;
    }

    const token = self.tokens[self.current_position];
    if (expected_chars) |chars| {
        if (token.kind == kind and std.mem.eql(u8, chars, token.chars)) {
            self.current_position += 1;
            return token;
        } else {
            self.last_expected = kind;
            self.last_expected_chars = chars;
            return ParserError.UnexpectedToken;
        }
    } else {
        if (token.kind == kind) {
            self.current_position += 1;
            return token;
        } else {
            self.last_expected = kind;
            self.last_expected_chars = null;
            return ParserError.UnexpectedToken;
        }
    }
}

fn currentToken(self: *Self) ?Token {
    return if (self.current_position < self.tokens.len) self.tokens[self.current_position] else null;
}

fn nextToken(self: *Self) ?Token {
    return if (self.current_position + 1 < self.tokens.len) self.tokens[self.current_position + 1] else null;
}

fn unexpectedToken(lexer: Lexer, actual: Lexer.Token, expected: []const Lexer.TokenKind, expected_chars: ?[]const u8) ParserError!void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const out = bw.writer();

    if (expected_chars) |chars| {
        out.print("ERROR: expected token of {any} with value '{s}'\n", .{ expected, chars }) catch return ParserError.UnknownError;
        out.print("       but got {} with '{s}' at {}:{}\n", .{
            actual.kind,
            actual.chars,
            actual.line,
            actual.column,
        }) catch return ParserError.UnknownError;
    } else {
        out.print("ERROR: expected token of the kinds {any}\n", .{expected}) catch return ParserError.UnknownError;
        out.print("       but got {} '{s}' at {}:{}\n", .{
            actual.kind,
            actual.chars,
            actual.line,
            actual.column,
        }) catch return ParserError.UnknownError;
    }
    try lexer.printContext(out.any(), actual);
    bw.flush() catch return ParserError.UnknownError;

    return ParserError.UnexpectedToken;
}

// Expression parsing
fn parseExpression(self: *Self) ParserError!expr.Expression {
    const value = try self.parseValue();
    const op = self.expect(.Operator, null) catch |err| {
        if (err == ParserError.UnexpectedToken or err == ParserError.EndOfFile)
            return value;
        return err;
    };
    const ex = try self.parseExpression();
    return .{ .binary_op = .{
        .left = &value,
        .right = &ex,
        .op = expr.mapOp(op.chars),
    } };
}

fn parseBoolean(self: *Self) ParserError!expr.Expression {
    const b = self.expect(.Boolean, null) catch unreachable;
    if (std.mem.eql(u8, b.chars, "true")) {
        return .{ .boolean = .{ .value = true } };
    } else if (std.mem.eql(u8, b.chars, "false")) {
        return .{ .boolean = .{ .value = false } };
    }
    unreachable;
}

fn parseValue(self: *Self) ParserError!expr.Expression {
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
            else => |k| b: {
                if (k == .CloseParen) {
                    self.last_expected = .Number;
                    return ParserError.UnexpectedToken;
                }

                std.debug.print("token type: {}\n", .{k});
                std.debug.print("token value: '{s}'\n", .{token.chars});
                break :b todo(expr.Expression, "parsing of expressions");
            },
        };
    } else return ParserError.EndOfFile;
}

fn parseIdentifier(self: *Self) ParserError!expr.Expression {
    const id = self.expect(.Identifier, null) catch unreachable;
    return .{ .identifier = expr.identifier(id.chars) };
}

fn parseFunctionCallExpr(self: *Self) ParserError!expr.Expression {
    _ = self;
    return todo(expr.Expression, "parsing of expression function calls");
}

fn parseStructAccess(self: *Self) ParserError!expr.Expression {
    _ = self;
    return todo(expr.Expression, "parsing of structure access");
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

fn parseNumber(self: *Self) ParserError!expr.Expression {
    const digits = self.expect(.Number, null) catch unreachable;
    const base = baseOf(digits.chars);

    if (std.mem.count(u8, digits.chars, ".") > 0) {
        var parts = std.mem.split(u8, digits.chars, ".");
        const tmp = parts.next() orelse unreachable;
        const int_part = if (base == 10) tmp else tmp[2..];
        const fraction_part = parts.next() orelse unreachable;

        const int = std.fmt.parseInt(i64, int_part, base) catch return ParserError.NumberParsingFailure;
        const fraction = std.fmt.parseInt(i64, fraction_part, base) catch return ParserError.NumberParsingFailure;
        const frac: f64 = @as(f64, @floatFromInt(fraction)) / std.math.pow(f64, @floatFromInt(base), @floatFromInt(fraction_part.len));
        const composite = @as(f64, @floatFromInt(int)) + frac;
        return .{ .number = .{ .float = composite } };
    } else {
        const int_part = if (base == 10) digits.chars else digits.chars[2..];
        const num = std.fmt.parseInt(i64, int_part, base) catch return ParserError.NumberParsingFailure;
        return .{ .number = .{ .integer = num } };
    }
}

// Statement parsing
fn parseCondition(self: *Self) ParserError!expr.Expression {
    _ = try self.expect(.OpenParen, "(");
    const condition = try self.parseExpression();
    _ = try self.expect(.CloseParen, ")");
    return condition;
}

fn parseCodeblock(self: *Self) ParserError![]stmt.Statement {
    _ = try self.expect(.OpenParen, "{");
    _ = self.expect(.Newline, null) catch {};
    const code = try self.parseStatements(true);
    _ = self.expect(.Newline, null) catch {};
    _ = try self.expect(.CloseParen, "}");
    return code;
}

fn parseIfStatement(self: *Self) ParserError!stmt.Statement {
    _ = self.expect(.Keyword, "if") catch unreachable;
    const condition = try self.parseCondition();
    const code = try self.parseCodeblock();

    const branch: stmt.Branch = .{
        .condition = condition,
        .body = code,
    };

    _ = self.expect(.Keyword, "else") catch |err| {
        if (err == ParserError.UnexpectedToken) {
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

fn parseWhileloop(self: *Self) ParserError!stmt.Statement {
    _ = self.expect(.Keyword, "while") catch unreachable;
    const condition = try self.parseCondition();
    const code = try self.parseCodeblock();
    return stmt.whileloop(condition, code);
}

fn parseReturn(self: *Self) ParserError!stmt.Statement {
    _ = try self.expect(.Keyword, "return");
    const ex = try self.parseExpression();
    return .{ .ret = .{ .value = ex } };
}

fn parseFunctionArguments(self: *Self) ParserError![]expr.Expression {
    var args = std.ArrayList(expr.Expression).init(self.allocator);
    var ex = self.parseExpression() catch {
        return ParserError.ArgumentParsingFailure;
    };
    args.append(ex) catch return ParserError.MemoryFailure;
    while (self.expect(.ArgSep, null)) |_| {
        ex = try self.parseExpression();
        args.append(ex) catch return ParserError.MemoryFailure;
    } else |err| {
        if (err == ParserError.UnexpectedToken)
            return args.items;

        return err;
    }
    return args.items;
}

fn parseFunctionCall(self: *Self) ParserError!stmt.Statement {
    const func_name = try self.expect(.Identifier, null);
    _ = try self.expect(.OpenParen, "(");

    const args = self.parseFunctionArguments() catch |err| {
        if (err == ParserError.ArgumentParsingFailure)
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

fn parseAssignment(self: *Self) ParserError!stmt.Statement {
    const id = self.expect(.Identifier, null) catch unreachable;
    _ = try self.expect(.Operator, "=");
    const expression = try self.parseExpression();
    return stmt.assignment(id.chars, expression);
}

fn parseStructAssignment(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseStatement(self: *Self) ParserError!stmt.Statement {
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
                self.parseAssignment()) else ParserError.UnexpectedToken,
            .OpenParen => todo(stmt.Statement, "parse statement => case open paren"),
            .Newline => b: {
                _ = self.expect(.Newline, null) catch unreachable;
                break :b self.parseStatement();
            },
            else => ParserError.UnexpectedToken,
        };
        _ = self.expect(.Newline, null) catch |err| {
            if (err == ParserError.EndOfFile) {
                return st;
            } else return err;
        };
        return st;
    } else return ParserError.EndOfFile;

    unreachable;
}

fn parseStatements(self: *Self, should_ignore_paran: bool) ParserError![]stmt.Statement {
    var stmts = std.ArrayList(stmt.Statement).init(self.allocator);
    while (self.parseStatement()) |statement| {
        stmts.append(statement) catch return ParserError.MemoryFailure;
    } else |err| {
        if (Self.parser_diagnostic) {
            std.debug.print("INFO: error: {}\n", .{err});
            std.debug.print("INFO: current token index: {}\n", .{self.current_position});
            std.debug.print("INFO: total token conut: {}\n", .{self.tokens.len});
        }

        if (err != ParserError.EndOfFile and err != ParserError.UnexpectedToken)
            return err;

        if (err == ParserError.UnexpectedToken and !should_ignore_paran) {
            const token = self.currentToken() orelse unreachable;
            if (std.mem.eql(u8, token.chars, "}") and should_ignore_paran)
                return stmts.toOwnedSlice() catch ParserError.MemoryFailure;

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
    return stmts.toOwnedSlice() catch ParserError.MemoryFailure;
}

pub fn parse(self: *Self) ParserError![]stmt.Statement {
    return self.parseStatements(false);
}

test "simple assignment" {
    const input =
        \\aoeu = aoeu
        \\
    ;
    const expected: stmt.Statement = .{ .assignment = .{
        .varName = .{ .name = "aoeu" },
        .value = .{ .identifier = .{ .name = "aoeu" } },
    } };

    var parser = try Self.init(input, std.testing.allocator);
    const result = try parser.parse();
    defer parser.deinit();
    defer parser.allocator.free(result);

    try std.testing.expectEqual(1, result.len);
    const result_stmt = result[0];
    try std.testing.expect(result_stmt == .assignment);
    try std.testing.expectEqualStrings(expected.assignment.varName.name, result_stmt.assignment.varName.name);
    try std.testing.expect(result_stmt.assignment.value == .identifier);
    try std.testing.expectEqualStrings(expected.assignment.value.identifier.name, result_stmt.assignment.value.identifier.name);
}

test "simple assignment - no newline" {
    const input = "aoeu = aoeu";
    const expected: stmt.Statement = .{ .assignment = .{
        .varName = .{ .name = "aoeu" },
        .value = .{ .identifier = .{ .name = "aoeu" } },
    } };

    var parser = try Self.init(input, std.testing.allocator);
    const result = try parser.parse();
    defer parser.deinit();
    defer parser.allocator.free(result);

    try std.testing.expectEqual(1, result.len);
    const result_stmt = result[0];
    try std.testing.expect(result_stmt == .assignment);
    try std.testing.expectEqualStrings(expected.assignment.varName.name, result_stmt.assignment.varName.name);
    try std.testing.expect(result_stmt.assignment.value == .identifier);
    try std.testing.expectEqualStrings(expected.assignment.value.identifier.name, result_stmt.assignment.value.identifier.name);
}

test "simple if statement" {
    const input =
        \\if (true) {
        \\    aoeu = aoeu
        \\}
        \\
    ;
    const expected: stmt.Statement = .{ .if_statement = .{
        .ifBranch = stmt.Branch{
            .condition = .{ .boolean = .{ .value = true } },
            .body = &[_]stmt.Statement{
                .{ .assignment = .{
                    .varName = .{ .name = "aoeu" },
                    .value = .{ .identifier = .{ .name = "aoeu" } },
                } },
            },
        },
        .elseBranch = null,
    } };

    var parser = try Self.init(input, std.testing.allocator);
    const result = try parser.parse();
    defer parser.deinit();
    defer parser.allocator.free(result);

    try std.testing.expectEqual(1, result.len);
    const result_stmt = result[0];
    try std.testing.expect(result_stmt == .if_statement);

    const expected_branch = expected.if_statement.ifBranch;
    const result_branch = result_stmt.if_statement.ifBranch;
    defer parser.allocator.free(result_branch.body);

    try std.testing.expect(result_branch.condition == .boolean);
    try std.testing.expectEqual(expected_branch.condition.boolean.value, result_branch.condition.boolean.value);

    const expected_body_statement = expected_branch.body[0];
    const result_body_statement = result_branch.body[0];

    try std.testing.expect(result_body_statement == .assignment);
    try std.testing.expectEqualStrings(expected_body_statement.assignment.value.identifier.name, result_body_statement.assignment.value.identifier.name);
    try std.testing.expectEqualStrings(expected_body_statement.assignment.varName.name, result_body_statement.assignment.varName.name);
}
