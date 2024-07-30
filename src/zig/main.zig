const std = @import("std");
const Parser = @import("parser.zig");
const stmt = @import("statement.zig");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allocator = arena.allocator();

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const input =
        \\aoeusnthaoeu = 1234123
        \\if (aoeuaoeu == 1234) {
        \\    aoeuaoeu = 0x1234567890AbCdEf
        \\}
        \\    while (true and false) {
        \\  aoeu = 12341234
        \\}
        \\ aoeu = () => {
        \\    return 42
        \\}
    ;

    var parser = try Parser.init(input, allocator);
    const exprs = parser.parse() catch |err| {
        if (err != Parser.ParserError.EndOfFile and err != Parser.ParserError.UnexpectedToken)
            return err;
        std.process.exit(1);
    };
    defer parser.freeStatements(exprs);

    for (exprs) |expr| {
        try stmt.printStatement(stdout.any(), expr, 1);
    }

    try bw.flush(); // don't forget to flush!
}
