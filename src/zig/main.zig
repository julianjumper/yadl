const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allocator = arena.allocator();

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const input =
        \\aoeusnthaoeu = 1234123
        \\if (aoeuaoeu == 1234) {
        \\    aoeuaoeu = 1234
        \\}
        \\ aoeusnhaoeusnthaoeu) => aoeu 'saoehusn thoeu'
    ;
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    const exprs = parser.parse() catch |err| {
        if (err != Parser.ParserError.EndOfFile and err != Parser.ParserError.UnexpectedToken)
            return err;
        return;
    };

    try stdout.print("{any}\n", .{exprs});

    try bw.flush(); // don't forget to flush!
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
