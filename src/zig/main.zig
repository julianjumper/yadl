const std = @import("std");
const Parser = @import("parser.zig");
const stmt = @import("statement.zig");
const interpreter = @import("interpreter.zig");

const Scope = @import("scope.zig");

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
    defer arena.deinit();

    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    var args = try std.process.argsWithAllocator(allocator);
    _ = args.next() orelse unreachable; // program name
    while (args.next()) |filepath| {
        const input = try readFile(allocator, filepath);

        var parser = Parser.init(input, allocator);

        const stmts = parser.parse() catch |err| {
            if (err != Parser.Error.EndOfFile and err != Parser.Error.UnexpectedToken)
                return err;
            std.process.exit(1);
        };
        var scope = Scope.empty(allocator);

        for (stmts) |st| {
            try interpreter.evalStatement(st, &scope);
        }
        // try bw.flush();
        parser.freeStatements(stmts);
    }
}
