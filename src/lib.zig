//! This module provides functions for serializing and deserializing
//! data structures with the SSZ method.

const std = @import("std");
const ArrayList = std.ArrayList;
const builtin = std.builtin;
const Allocator = std.mem.Allocator;

/// Number of bytes per chunk.
const BYTES_PER_CHUNK = 32;

/// Number of bytes per serialized length offset.
const BYTES_PER_LENGTH_OFFSET = 4;

pub extern "forward" fn read_args(dest: *u8) void;
pub extern "forward" fn return_data(data: *const u8, len: usize) void;

pub fn args(len: usize) ![]u8 {
    var input = try std.heap.page_allocator.alloc(u8, len);
    read_args(@ptrCast(input));
    return input;
}

pub fn output(data: []u8) void {
    return_data(@ptrCast(data), data.len);
}

export fn arbitrum_main(len: usize) i32 {
    var input = args(len) catch return 1;
    output(input);
    return 0; // OK.
}

// Determine the serialized size of an object so that
// the code serializing of variable-size objects can
// determine the offset to the next object.
fn serializedSize(comptime T: type, data: T) !usize {
    const info = @typeInfo(T);
    return switch (info) {
        .Array => data.len,
        .Pointer => switch (info.Pointer.size) {
            .Slice => data.len,
            else => serializedSize(info.Pointer.child, data.*),
        },
        .Optional => if (data == null)
            @as(usize, 1)
        else
            1 + try serializedSize(info.Optional.child, data.?),
        .Null => @as(usize, 0),
        else => error.NoSerializedSizeAvailable,
    };
}

/// Returns true if an object is of fixed size
fn isFixedSizeObject(comptime T: type) !bool {
    const info = @typeInfo(T);
    switch (info) {
        .Bool, .Int, .Null => return true,
        .Array => return false,
        .Struct => inline for (info.Struct.fields) |field| {
            if (!try isFixedSizeObject(field.type)) {
                return false;
            }
        },
        .Pointer => switch (info.Pointer.size) {
            .Many, .Slice, .C => return false,
            .One => return isFixedSizeObject(info.Pointer.child),
        },
        else => return error.UnknownType,
    }
    return true;
}

/// Provides the generic serialization of any `data` var to SSZ. The
/// serialization is written to the `ArrayList` `l`.
pub fn serialize(comptime T: type, data: T, l: *ArrayList(u8)) !void {
    const info = @typeInfo(T);
    switch (info) {
        .Array => {
            // Bitvector[N] or vector?
            if (info.Array.child == bool) {
                var byte: u8 = 0;
                for (data, 0..) |bit, index| {
                    if (bit) {
                        byte |= @as(u8, 1) << @as(u3, @truncate(index));
                    }

                    if (index % 8 == 7) {
                        try l.append(byte);
                        byte = 0;
                    }
                }

                // Write the last byte if the length
                // is not byte-aligned
                if (data.len % 8 != 0) {
                    try l.append(byte);
                }
            } else {
                // If the item type is fixed-size, serialize inline,
                // otherwise, create an array of offsets and then
                // serialize each object afterwards.
                if (try isFixedSizeObject(info.Array.child)) {
                    for (data) |item| {
                        try serialize(info.Array.child, item, l);
                    }
                } else {
                    // Size of the buffer before anything is
                    // written to it.
                    var start = l.items.len;

                    // Reserve the space for the offset
                    for (data) |_| {
                        _ = try l.writer().writeIntLittle(u32, 0);
                    }

                    // Now serialize one item after the other
                    // and update the offset list with its location.
                    for (data) |item| {
                        std.mem.writeIntLittle(u32, l.items[start .. start + 4][0..4], @as(u32, @truncate(l.items.len)));
                        _ = try serialize(info.Array.child, item, l);
                        start += 4;
                    }
                }
            }
        },
        .Bool => {
            if (data) {
                try l.append(1);
            } else {
                try l.append(0);
            }
        },
        .Int => |int| {
            switch (int.bits) {
                8, 16, 32, 64, 128, 256 => {},
                else => return error.InvalidSerializedIntLengthType,
            }
            _ = try l.writer().writeIntLittle(T, data);
        },
        .Pointer => {
            // Bitlist[N] or list?
            switch (info.Pointer.size) {
                .Slice, .One => {
                    if (@sizeOf(info.Pointer.child) == 1) {
                        _ = try l.writer().write(data);
                    } else {
                        for (data) |item| {
                            try serialize(@TypeOf(item), item, l);
                        }
                    }
                },
                else => return error.UnSupportedPointerType,
            }
        },
        .Struct => {
            // First pass, accumulate the fixed sizes
            comptime var var_start = 0;
            inline for (info.Struct.fields) |field| {
                if (@typeInfo(field.type) == .Int or @typeInfo(field.type) == .Bool) {
                    var_start += @sizeOf(field.type);
                } else {
                    var_start += 4;
                }
            }

            // Second pass: intertwine fixed fields and variables offsets
            var var_acc = @as(usize, var_start); // variable part size accumulator
            inline for (info.Struct.fields) |field| {
                switch (@typeInfo(field.type)) {
                    .Int, .Bool => {
                        try serialize(field.type, @field(data, field.name), l);
                    },
                    else => {
                        try serialize(u32, @as(u32, @truncate(var_acc)), l);
                        var_acc += try serializedSize(field.type, @field(data, field.name));
                    },
                }
            }

            // Third pass: add variable fields at the end
            if (var_acc > var_start) {
                inline for (info.Struct.fields) |field| {
                    switch (@typeInfo(field.type)) {
                        .Int, .Bool => {
                            // skip fixed-size fields
                        },
                        else => {
                            try serialize(field.type, @field(data, field.name), l);
                        },
                    }
                }
            }
        },
        // Nothing to be added to the payload
        .Null => {},
        // Optionals are like unions, but their 0 value has to be 0.
        .Optional => {
            if (data != null) {
                _ = try l.writer().writeIntLittle(u8, 1);
                try serialize(info.Optional.child, data.?, l);
            } else {
                _ = try l.writer().writeIntLittle(u8, 0);
            }
        },
        .Union => {
            if (info.Union.tag_type == null) {
                return error.UnionIsNotTagged;
            }
            inline for (info.Union.fields, 0..) |f, index| {
                if (@intFromEnum(data) == index) {
                    _ = try l.writer().writeIntLittle(u8, index);
                    try serialize(f.type, @field(data, f.name), l);
                    return;
                }
            }
        },
        else => {
            return error.UnknownType;
        },
    }
}

/// Takes a byte array containing the serialized payload of type `T` (with
/// possible trailing data) and deserializes it into the `T` object pointed
/// at by `out`.
pub fn deserialize(comptime T: type, serialized: []const u8, out: *T) !void {
    const info = @typeInfo(T);
    switch (info) {
        .Array => {
            // Bitvector[N] or regular vector?
            if (info.Array.child == bool) {
                for (serialized, 0..) |byte, bindex| {
                    var i = @as(u8, 0);
                    var b = byte;
                    while (bindex * 8 + i < out.len and i < 8) : (i += 1) {
                        out[bindex * 8 + i] = b & 1 == 1;
                        b >>= 1;
                    }
                }
            } else {
                const U = info.Array.child;
                if (try isFixedSizeObject(U)) {
                    comptime var i = 0;
                    const pitch = @sizeOf(U);
                    inline while (i < out.len) : (i += pitch) {
                        try deserialize(U, serialized[i * pitch .. (i + 1) * pitch], &out[i]);
                    }
                } else {
                    // first variable index is also the size of the list
                    // of indices. Recast that list as a []const u32.
                    const size = std.mem.readIntLittle(u32, serialized[0..4]) / @sizeOf(u32);
                    const indices = std.mem.bytesAsSlice(u32, serialized[0 .. size * 4]);
                    var i = @as(usize, 0);
                    while (i < size) : (i += 1) {
                        const end = if (i < size - 1) indices[i + 1] else serialized.len;
                        const start = indices[i];
                        if (start >= serialized.len or end > serialized.len) {
                            return error.IndexOutOfBounds;
                        }
                        try deserialize(U, serialized[start..end], &out[i]);
                    }
                }
            }
        },
        .Bool => out.* = (serialized[0] == 1),
        .Int => {
            const N = @sizeOf(T);
            out.* = std.mem.readIntLittle(T, serialized[0..N]);
        },
        .Optional => {
            const index: u8 = serialized[0];
            if (index != 0) {
                var x: info.Optional.child = undefined;
                try deserialize(info.Optional.child, serialized[1..], &x);
                out.* = x;
            } else {
                out.* = null;
            }
        },
        // Data is not copied in this function, copy is therefore
        // the responsibility of the caller.
        .Pointer => out.* = serialized[0..],
        .Struct => {
            // Calculate the number of variable fields in the
            // struct.
            comptime var n_var_fields = 0;
            comptime {
                for (info.Struct.fields) |field| {
                    switch (@typeInfo(field.type)) {
                        .Int, .Bool => {},
                        else => n_var_fields += 1,
                    }
                }
            }

            var indices: [n_var_fields]u32 = undefined;

            // First pass, read the value of each fixed-size field,
            // and write down the start offset of each variable-sized
            // field.
            comptime var i = 0;
            comptime var variable_field_index = 0;
            inline for (info.Struct.fields) |field| {
                switch (@typeInfo(field.type)) {
                    .Bool, .Int => {
                        // Direct deserialize
                        try deserialize(field.type, serialized[i .. i + @sizeOf(field.type)], &@field(out.*, field.name));
                        i += @sizeOf(field.type);
                    },
                    else => {
                        try deserialize(u32, serialized[i .. i + 4], &indices[variable_field_index]);
                        i += 4;
                        variable_field_index += 1;
                    },
                }
            }

            // Second pass, deserialize each variable-sized value
            // now that their offset is known.
            comptime var last_index = 0;
            inline for (info.Struct.fields) |field| {
                switch (@typeInfo(field.type)) {
                    .Bool, .Int => {}, // covered by the previous pass
                    else => {
                        const end = if (last_index == indices.len - 1) serialized.len else indices[last_index + 1];
                        try deserialize(field.type, serialized[indices[last_index]..end], &@field(out.*, field.name));
                        last_index += 1;
                    },
                }
            }
        },
        .Union => {
            // Read the type index
            var union_index: u8 = undefined;
            try deserialize(u8, serialized, &union_index);

            // Use the index to figure out which type must
            // be deserialized.
            inline for (info.Union.fields, 0..) |field, index| {
                if (index == union_index) {
                    // &@field(out.*, field.name) can not be used directly,
                    // because this field type hasn't been activated at this
                    // stage.
                    var data: field.type = undefined;
                    try deserialize(field.type, serialized[1..], &data);
                    out.* = @unionInit(T, field.name, data);
                }
            }
        },
        else => return error.NotImplemented,
    }
}
