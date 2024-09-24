const std = @import("std");

const interpreter = @import("../interpreter.zig");

pub const Error = error{
    NotImplemented,
} || std.mem.Allocator.Error || interpreter.Error;
