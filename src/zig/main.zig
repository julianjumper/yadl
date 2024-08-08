const std = @import("std");
const Parser = @import("parser.zig");
const stmt = @import("statement.zig");

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
const allocator = arena.allocator();

fn readFile(alloc: std.mem.Allocator, filepath: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(filepath, .{});
    defer file.close();
    const stat = try file.stat();
    const contents = try file.readToEndAlloc(alloc, stat.size);
    return contents;
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var args = try std.process.argsWithAllocator(allocator);
    _ = args.next() orelse unreachable; // program name
    while (args.next()) |filepath| {
        const input = try readFile(allocator, filepath);

        var parser = try Parser.init(input, allocator);

        const exprs = parser.parse() catch |err| {
            if (err != Parser.Error.EndOfFile and err != Parser.Error.UnexpectedToken)
                return err;
            std.process.exit(1);
        };

        for (exprs) |expr| {
            try stmt.printStatement(stdout.any(), expr, 1);
        }
        try bw.flush();
        parser.freeStatements(exprs);
    }
}
