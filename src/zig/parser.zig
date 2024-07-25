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

fn expect(self: *Self, kind: Kind) ParserError!Token {
    if (self.current_position >= self.tokens.len) {
        return ParserError.EndOfFile;
    }

    if (self.tokens[self.current_position].kind == kind) {
        self.current_position += 1;
        return self.tokens[self.current_position - 1];
    } else {
        try unexpectedToken(self.lexer, self.tokens[self.current_position], .{kind});
    }
}

pub fn currentToken(self: *Self) ?Token {
    return if (self.current_position < self.tokens.len) self.tokens[self.current_position] else null;
}

fn unexpectedToken(lexer: *Lexer, actual: Lexer.Token, expected: []const Lexer.TokenKind) ParserError!void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const out = bw.writer();

    out.print("ERROR: {}\n", .{ParserError.UnexpectedToken}) catch return ParserError.UnknownError;
    out.print("    expected token of the kinds {any}\n", .{expected}) catch return ParserError.UnknownError;
    out.print("    but got {} ('{s}') at {}:{}\n", .{
        actual.kind,
        actual.chars,
        actual.line,
        actual.column,
    }) catch return ParserError.UnknownError;
    try lexer.printContext(out.any(), actual);
    bw.flush() catch return ParserError.UnknownError;

    return ParserError.UnexpectedToken;
}

fn parseIfStatement(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseWhileloop(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseReturn(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseFunctionCall(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseAssignment(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseStructAssignment(self: *Self) ParserError!stmt.Statement {
    _ = self;
    return ParserError.NotImplemented;
}

fn parseStatement(self: *Self) ParserError!stmt.Statement {
    if (self.currentToken()) |token| {
        std.debug.print("{}\n", .{token.kind});
        switch (token.kind) {
            .Keyword => return self.parseReturn() catch self.parseIfStatement() catch self.parseWhileloop(),
            .Identifier => return self.parseFunctionCall() catch self.parseStructAssignment() catch self.parseAssignment(),
            else => try unexpectedToken(self.lexer, token, &[_]Lexer.TokenKind{ .Identifier, .Keyword }),
        }
    } else return ParserError.EndOfFile;

    unreachable;
}

pub fn parse(self: *Self, allocator: std.mem.Allocator) ParserError![]stmt.Statement {
    var stmts = std.ArrayList(stmt.Statement).init(allocator);
    while (self.parseStatement()) |statement| {
        stmts.append(statement) catch {
            return ParserError.MemoryFailure;
        };
    } else |err| {
        if (err != ParserError.EndOfFile)
            return err;
    }
    return stmts.items;
}
