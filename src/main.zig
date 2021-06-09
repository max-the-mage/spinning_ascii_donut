const std = @import("std");
const zzz = @import("zzz");
pub extern "c" fn putchar(char: c_int) c_int;
const Timer = std.time.Timer;

// range iterator
fn range(len: usize) []const u0 {
    return @as([*]u0, undefined)[0..len];
}

const lumi = ".'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B$@";
const lumi_mult: f32 = @divFloor(@intToFloat(f32, lumi.len), @sqrt(2.0));

const pi2 = std.math.pi*2.0;

var fps_timer: Timer = undefined;
var delta_timer: Timer = undefined;

var output: []u8 = undefined;
var z_buffer: []f32 = undefined;

var owidth: u64 = undefined;
var out_buf: []u8 = undefined;

var frame_time: u64 = 750_000;

var delta_multiplier: f32 = 1.0;

const RenderData = struct{
    width: u64,
    height: u64,
    k1: f32,
    k2: f32,

    points_per_revolution: f32,
    rotation_speed: f32,
    r1: f32,
    r2: f32,

    theta_spacing: f32,
    phi_spacing: f32,

    frame_cycles: usize,
};

const ThreadData = struct {
    allocator: *std.mem.Allocator,
    render_data: *RenderData,
    config_file: *std.fs.File,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = &gpa.allocator;

    var a: f32 = 0.0;
    var b: f32 = 0.0;
    
    var config_file = try std.fs.cwd().openFile("res/config.zzz", .{.read = true});
    var data = getRenderData(try getConfigNode(alloc, &config_file));

    var cu_thread = try std.Thread.spawn(updateDataThread, .{
        .allocator = alloc,
        .render_data = &data,
        .config_file = &config_file,
    });

    owidth = data.width+1;

    output = try alloc.alloc(u8, (data.width*data.height));
    z_buffer = try alloc.alloc(f32, (data.width*data.height));
    out_buf = try alloc.alloc(u8, (owidth*data.height));

    std.mem.set(u8, output, ' ');
    std.mem.set(f32, z_buffer, 0.0);

    var frames: usize = 0;

    fps_timer = try Timer.start();
    delta_timer = try Timer.start();
    while (true) : (frames += 1) {
        
        try renderFrame(&data, a, b, frames);

        a += data.rotation_speed*delta_multiplier*0.0010;
        b += data.rotation_speed*delta_multiplier*0.0006;
    }
}

/// loads config data from file and returns the parsed zzz root node
fn getConfigNode(alloc: *std.mem.Allocator, config_file: *std.fs.File) !*zzz.ZNode {
    const config_text = try config_file.readToEndAlloc(alloc, 4096);
    defer alloc.free(config_text);
    try config_file.seekTo(0); // reset back to start of file

    var config_tree = zzz.ZTree(1, 30){};
    const root = try config_tree.appendText(config_text);
    root.convertStrings();

    return root;
}

fn getRenderData(root: *zzz.ZNode) RenderData {

    const render = root.getChild(0).?;
    const torus = root.getChild(1).?;

    const k2 = render.getChildValue(2).?.Float;
    const r1 = torus.getChildValue(2).?.Float;
    const r2 = torus.getChildValue(3).?.Float;

    const width = @intCast(u64, render.getChildValue(0).?.Int);

    const ppr = torus.getChildValue(0).?.Float;

    return RenderData{
        .width = width,
        .height = @intCast(u64, render.getChildValue(1).?.Int),
        .k2 = k2,

        .points_per_revolution = ppr,
        .rotation_speed = torus.getChildValue(1).?.Float,
        .r1 = r1,
        .r2 = r2,

        .k1 = @intToFloat(f32, width)*k2*1.5/(8.0*(r1+r2)),

        .theta_spacing = pi2 / ppr,
        .phi_spacing = pi2 / (ppr*1.25),

        .frame_cycles = @floatToInt(usize, ppr*(ppr*1.25)),
    };
}

/// updates the render data from the config file
fn updateRenderData(root: *zzz.ZNode, data: *RenderData) void {

    const render = root.getChild(0).?;
    const torus = root.getChild(1).?;

    data.k2 = render.getChildValue(2).?.Float;
    data.r1 = torus.getChildValue(2).?.Float;
    data.r2 = torus.getChildValue(3).?.Float;

    data.points_per_revolution = torus.getChildValue(0).?.Float;
    data.rotation_speed = torus.getChildValue(1).?.Float;

    data.k1 = @intToFloat(f32, data.width)*data.k2*1.5/(8.0*(data.r1+data.r2));

    data.theta_spacing = pi2 / data.points_per_revolution;
    data.phi_spacing = pi2 / (data.points_per_revolution*1.25);

    data.frame_cycles = @floatToInt(usize, data.points_per_revolution*(data.points_per_revolution*1.25));
}

/// passes data needed to thread to update the render data
fn updateDataThread(data: ThreadData) !void {
    while (true) {
        updateRenderData(try getConfigNode(data.allocator, data.config_file), data.render_data);
        std.time.sleep(std.time.ns_per_s); // wait 1 second between updating the file
    }
}

fn renderFrame(data: *RenderData, a: f32, b: f32, frames: usize) !void {

    // precompute sins & cosines
    const cos_a = @cos(a);
    const cos_b = @cos(b);

    const sin_a = @sin(a);
    const sin_b = @sin(b);

    std.mem.set(u8, output, ' ');
    std.mem.set(f32, z_buffer, 0.0);
    // theta goes around cross sectional of torus
    var theta: f32 = 0.0;
    while (theta < pi2) : (theta += data.theta_spacing) {
        const cos_theta = @cos(theta);
        const sin_theta = @sin(theta);

        // phi goes around center of revolution of torus
        var phi: f32 = 0.0;
        while (phi < pi2) : (phi += data.phi_spacing) {
            const cos_phi = @cos(phi);
            const sin_phi = @sin(phi);

            // circle points
            const circle_x = data.r2 + data.r1*cos_theta;
            const circle_y = data.r1*sin_theta;

            // one over z
            const ooz = 1.0/(sin_phi*circle_x*sin_a + sin_theta*cos_a + data.k2);
            // some fancy factoring
            const t = sin_phi * circle_x * cos_a - sin_theta * sin_a;

            // x and y projection
            const xp = @floatToInt(i32, @intToFloat(f32, data.width)/2.0 + (data.k1)*ooz*(cos_phi * circle_x * cos_b - t * sin_b));
            const yp = @floatToInt(i32, @intToFloat(f32, data.height)/2.0 + (data.k1/2.0)*ooz*(cos_phi * circle_x * sin_b + t * cos_b));

            // calculate luminance from surface normal
            const luminance = ((sin_theta*sin_a - sin_phi*cos_theta*cos_a)*cos_b -
            sin_phi*cos_theta*sin_a - sin_theta*cos_a - cos_phi*cos_theta*sin_b);

            const luminance_index = @floatToInt(usize, lumi_mult*(if(luminance > 0.0) luminance else 0.0));

            const o: i64 = xp + try std.math.cast(i64, data.width) * yp;

            if (
                data.height > yp and yp > 0 and
                data.width > xp and xp > 0 and
                ooz > z_buffer[@intCast(usize, o)])
            {
                z_buffer[@intCast(usize, o)] = ooz;
                output[@intCast(usize, o)] = lumi[luminance_index];
            }

            //std.log.info("{d}", .{luminance_index});
        }
    }

    for (range(data.height)) |_,i| {
        _=try std.fmt.bufPrint(
            out_buf[(i*owidth)..(owidth+i*owidth)],
            "{s}\n", .{output[(i*data.width)..(data.width+(i*data.width))]}
        );
    }
    std.debug.print("\x1b[H{s}", .{out_buf});
    
    if (@mod(frames, 30) == 0) {
        const thirty_frame_time = fps_timer.lap();
        frame_time = @divTrunc(thirty_frame_time, 30);
    }

    const delta_time = delta_timer.lap();
    delta_multiplier = 120.0/@floatCast(f32, (1.0/(@intToFloat(f64, delta_time) * 0.000000001)));

    const fps = 1.0/(@intToFloat(f64, frame_time) * 0.000000001);

    std.debug.print(
        "K1({d:.3}) K2({d}) R1({d}) R2({d}) dimensions({d}x{d}) frame_cycles({d}) illumination_detail({}) rotation_speed({d:.1})\ndelta_multiplier({d:.3}) fps({d:.2})\nb",
    .{
        data.k1, data.k2, data.r1, data.r2, data.width, data.height, 
        data.frame_cycles, lumi.len, data.rotation_speed, delta_multiplier, fps
    });
}