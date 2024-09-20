const std = @import("std");

pub fn load_lines(path: []const u8, allocator: std.mem.Allocator) ![][]const u8 {
    const cwd = std.fs.cwd();
    const file = try cwd.openFile(path, .{ .mode = .read_only });
    const stat = try file.stat();
    const content = try file.readToEndAlloc(allocator, stat.size);
    // defer allocator.free(content);

    var splitter = std.mem.split(u8, content, "\n");
    var out = std.ArrayList([]const u8).init(allocator);
    while (splitter.next()) |line| {
        try out.append(line);
    }
    return try out.toOwnedSlice();
}
