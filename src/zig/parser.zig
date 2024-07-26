const std = @import("std");
const Lexer = @import("lexer.zig");
const stmt = @import("statement.zig");
const expr = @import("expression.zig");

const pError = error{
    UnexpectedToken,
};

pub const ParserError = Lexer.LexerError || pError;

current_position: u64,
tokens: []const Lexer.Token,
lexer: *Lexer,
allocator: std.mem.Allocator,

const Kind = Lexer.TokenKind;
const Token = Lexer.Token;
const Self = @This();

pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Lexer.LexerError!Self {
    var ts = std.ArrayList(Lexer.Token).init(allocator);
    try lexer.allTokens(&ts);
    return Self{
        .current_position = 0,
        .tokens = ts.items,
        .lexer = lexer,
        .allocator = allocator,
    };
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
            try unexpectedToken(self.lexer, token, &[_]Lexer.TokenKind{kind}, chars);
            return ParserError.UnexpectedToken;
        }
    } else {
        if (token.kind == kind) {
            self.current_position += 1;
            return token;
        } else {
            try unexpectedToken(self.lexer, token, &[_]Lexer.TokenKind{kind}, null);
            return ParserError.UnexpectedToken;
        }
    }
}

pub fn currentToken(self: *Self) ?Token {
    return if (self.current_position < self.tokens.len) self.tokens[self.current_position] else null;
}

fn unexpectedToken(lexer: *Lexer, actual: Lexer.Token, expected: []const Lexer.TokenKind, expected_chars: ?[]const u8) ParserError!void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const out = bw.writer();

    out.print("ERROR: {}\n", .{ParserError.UnexpectedToken}) catch return ParserError.UnknownError;
    if (expected_chars) |chars| {
        out.print("    expected token of {any} with value '{s}'\n", .{ expected, chars }) catch return ParserError.UnknownError;
        out.print("    but got {} with '{s}' at {}:{}\n", .{
            actual.kind,
            actual.chars,
            actual.line,
            actual.column,
        }) catch return ParserError.UnknownError;
    } else {
        out.print("    expected token of the kinds {any}\n", .{expected}) catch return ParserError.UnknownError;
        out.print("    but got {} ('{s}') at {}:{}\n", .{
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
    const pos = self.current_position;
    const value = self.parseValue() catch |err| {
        self.current_position = pos;
        return err;
    };
    const op = self.expect(.Operator, null) catch |err| {
        if (err == ParserError.UnexpectedToken)
            return value;
        self.current_position = pos;
        return err;
    };

    const ex = self.parseExpression() catch |err| {
        self.current_position = pos;
        return err;
    };
    return .{ .binary_op = .{
        .left = &value,
        .right = &ex,
        .op = expr.mapOp(op.chars),
    } };
}

fn parseValue(self: *Self) ParserError!expr.Expression {
    if (self.currentToken()) |token| {
        return switch (token.kind) {
            .Number => self.parseNumber(),
            .Identifier => self.parseIdentifier(),
            else => |k| b: {
                std.debug.print("expression type: {}\n", .{k});
                break :b todo(expr.Expression, "parsing of expressions");
            },
        };
    } else return ParserError.EndOfFile;
}

fn parseIdentifier(self: *Self) ParserError!expr.Expression {
    const id = try self.expect(.Identifier, null);
    return .{ .identifier = expr.identifier(id.chars) };
}

fn parseFunctionCallExpr(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return todo(stmt.Statement, "parsing of expression function calls");
}

fn baseOf(digits: []const u8) u8 {
    if (digits.len < 2)
        return 10;

    return switch (digits[1]) {
        'x' => 16,
        'o' => 8,
        'b' => 2,
        else => 10,
    };
}

fn parseNumber(self: *Self) ParserError!expr.Expression {
    const digits = try self.expect(.Number, null);
    const base = baseOf(digits.chars);

    if (std.mem.count(u8, digits.chars, ".") > 0) {
        var parts = std.mem.split(u8, digits.chars, ".");
        const int_part = parts.next() orelse unreachable;
        const fraction_part = parts.next() orelse unreachable;

        const int = std.fmt.parseInt(i64, int_part, base) catch return ParserError.UnknownError;
        const fraction = std.fmt.parseInt(i64, int_part, base) catch return ParserError.UnknownError;
        const frac: f64 = @as(f64, @floatFromInt(fraction)) / std.math.pow(f64, @floatFromInt(base), @floatFromInt(fraction_part.len));
        const composite = @as(f64, @floatFromInt(int)) + frac;
        return .{ .number = .{ .float = composite } };
    } else {
        const num = std.fmt.parseInt(i64, digits.chars, base) catch return ParserError.UnknownError;
        return .{ .number = .{ .integer = num } };
    }
}

// Statement parsing
fn parseCondition(self: *Self) ParserError!expr.Expression {
    const pos = self.current_position;
    _ = self.expect(.OpenParen, "(") catch |err| {
        self.current_position = pos;
        return err;
    };

    const condition = self.parseExpression() catch |err| {
        self.current_position = pos;
        return err;
    };

    _ = self.expect(.CloseParen, ")") catch |err| {
        self.current_position = pos;
        return err;
    };
    return condition;
}

fn parseCodeblock(self: *Self) ParserError![]stmt.Statement {
    const pos = self.current_position;
    _ = self.expect(.OpenParen, "{") catch |err| {
        self.current_position = pos;
        return err;
    };

    _ = self.expect(.Newline, null) catch {};

    const code = self.parse() catch |err| {
        self.current_position = pos;
        return err;
    };

    _ = self.expect(.Newline, null) catch {};

    _ = self.expect(.CloseParen, "}") catch |err| {
        self.current_position = pos;
        return err;
    };
    return code;
}

fn parseIfStatement(self: *Self) ParserError!stmt.Statement {
    const pos = self.current_position;
    _ = self.expect(.Keyword, "if") catch |err| {
        self.current_position = pos;
        return err;
    };

    const condition = self.parseCondition() catch |err| {
        self.current_position = pos;
        return err;
    };

    const code = self.parseCodeblock() catch |err| {
        self.current_position = pos;
        return err;
    };
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
        self.current_position = pos;
        return err;
    };

    const elseCode = self.parseCodeblock() catch |err| {
        self.current_position = pos;
        return err;
    };

    return .{ .if_statement = .{
        .ifBranch = branch,
        .elseBranch = elseCode,
    } };
}

fn parseWhileloop(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return todo(stmt.Statement, "parsing of while loop");
}

fn parseReturn(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return todo(stmt.Statement, "parsing of return statement");
}

fn parseFunctionCall(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return todo(stmt.Statement, "parsing of statement function calls");
}

fn parseAssignment(self: *Self) ParserError!stmt.Statement {
    const pos = self.current_position;
    const id = self.expect(.Identifier, null) catch unreachable;
    _ = self.expect(.Operator, "=") catch |err| {
        self.current_position = pos;
        return err;
    };
    const expression = try self.parseExpression();
    return stmt.assignment(id.chars, expression);
}

fn parseStructAssignment(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseStatement(self: *Self) ParserError!stmt.Statement {
    if (self.currentToken()) |token| {
        return switch (token.kind) {
            .Keyword => self.parseReturn() catch self.parseIfStatement() catch self.parseWhileloop(),
            .Identifier => self.parseFunctionCall() catch self.parseStructAssignment() catch self.parseAssignment(),
            .Newline => b: {
                _ = self.expect(.Newline, null) catch unreachable;
                break :b self.parseStatement();
            },
            else => b: {
                try unexpectedToken(self.lexer, token, &[_]Lexer.TokenKind{ .Identifier, .Keyword }, null);
                break :b ParserError.UnexpectedToken;
            },
        };
    } else return ParserError.EndOfFile;

    unreachable;
}

pub fn parse(self: *Self) ParserError![]stmt.Statement {
    var stmts = std.ArrayList(stmt.Statement).init(self.allocator);
    while (self.parseStatement()) |statement| {
        stmts.append(statement) catch {
            return ParserError.MemoryFailure;
        };
        _ = try self.expect(.Newline, null);
    } else |err| {
        if (err != ParserError.EndOfFile)
            return err;
    }
    return stmts.items;
}
