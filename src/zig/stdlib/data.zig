const std = @import("std");

const expression = @import("../expression.zig");

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

pub fn load_json(path: []const u8, allocator: std.mem.Allocator) !*expression.Expression {
    const cwd = std.fs.cwd();
    const file = try cwd.openFile(path, .{ .mode = .read_only });
    const stat = try file.stat();
    const content = try file.readToEndAlloc(allocator, stat.size);
    var tmp = std.json.Scanner.initCompleteInput(allocator, content);
    const value = try std.json.Value.jsonParse(
        allocator,
        &tmp,
        // TODO: hard coded value here
        .{ .duplicate_field_behavior = .use_first, .max_value_len = 1024 },
    );
    return map_to_expression(allocator, value);
}

fn map_to_expression(allocator: std.mem.Allocator, value: std.json.Value) std.mem.Allocator.Error!*expression.Expression {
    return switch (value) {
        .null => b: {
            const out = try allocator.create(expression.Expression);
            out.* = .{ .none = null };
            break :b out;
        },
        .bool => |b| expression.Boolean.init(allocator, b),
        .float => |f| expression.Number.init(allocator, f64, f),
        .integer => |n| expression.Number.init(allocator, i64, n),
        .string => |s| expression.String.init(allocator, s),
        .array => |a| b: {
            const tmp = try allocator.alloc(expression.Expression, a.items.len);
            for (a.items, tmp) |item, *elem| {
                const t = try map_to_expression(allocator, item);
                elem.* = t.*;
                allocator.destroy(t);
            }
            break :b expression.Array.init(allocator, tmp);
        },
        .object => |o| b: {
            const tmp = try allocator.alloc(expression.DictionaryEntry, o.keys().len);
            var iter = o.iterator();
            var index: usize = 0;
            while (iter.next()) |entry| {
                const s: []u8 = try allocator.alloc(u8, entry.key_ptr.len);
                std.mem.copyForwards(u8, s, entry.key_ptr.*);
                const val = try map_to_expression(allocator, entry.value_ptr.*);
                const str = try expression.String.init(allocator, s);
                tmp[index] = .{ .key = str, .value = val };
                index += 1;
            }
            break :b expression.Dictionary.init(allocator, tmp);
        },
        else => unreachable,
    };
}
