//============================================================================
//  Vectrex for MiSTer
//
//  Original MiSTer port:
//  Copyright (C) 2017-2019 Sorgelig
//
//  Enhanced core rebuild and subsequent modifications:
//  Copyright (C) 2026 Videodr0me
//  SPDX-License-Identifier: GPL-2.0-or-later
//
//  Based on Dar's original Vectrex FPGA implementation.
//  Incorporates earlier Vectrex development by Alan Steremberg (alanswx).
//  Extensively modified in 2026; see CHANGELOG.md for details.
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

// [MiSTer-DB9 BEGIN] - joy_type/joy_2p relocated: upstream owns status[127:124]
// [MiSTer-DB9 RESERVED status bits: 62:61 65]
// [MiSTer-DB9 END]
module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [45:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	//if VIDEO_ARX[12] or VIDEO_ARY[12] is set then [11:0] contains scaled size instead of aspect ratio.
	output [12:0] VIDEO_ARX,
	output [12:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output [1:0]  VGA_SL,
	output        VGA_SCALER, // Force VGA scaler
	output        VGA_DISABLE, // analog out is off

	input  [11:0] HDMI_WIDTH,
	input  [11:0] HDMI_HEIGHT,
	output        HDMI_FREEZE,
	output        HDMI_BLACKOUT,
	output        HDMI_BOB_DEINT,

`ifdef MISTER_FB
	// Use framebuffer in DDRAM
	// FB_FORMAT:
	//    [2:0] : 011=8bpp(palette) 100=16bpp 101=24bpp 110=32bpp
	//    [3]   : 0=16bits 565 1=16bits 1555
	//    [4]   : 0=RGB  1=BGR (for 16/24/32 modes)
	//
	// FB_STRIDE either 0 (rounded to 256 bytes) or multiple of pixel size (in bytes)
	output        FB_EN,
	output  [4:0] FB_FORMAT,
	output [11:0] FB_WIDTH,
	output [11:0] FB_HEIGHT,
	output [31:0] FB_BASE,
	output [13:0] FB_STRIDE,
	input         FB_VBL,
	input         FB_LL,
	output        FB_FORCE_BLANK,

`ifdef MISTER_FB_PALETTE
	// Palette control for 8bit modes.
	// Ignored for other video modes.
	output        FB_PAL_CLK,
	output  [7:0] FB_PAL_ADDR,
	output [23:0] FB_PAL_DOUT,
	input  [23:0] FB_PAL_DIN,
	output        FB_PAL_WR,
`endif
`endif

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	// I/O board button press simulation (active high)
	// b[1]: user button
	// b[0]: osd button
	output  [1:0] BUTTONS,

	input         CLK_AUDIO, // 24.576 MHz
	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)

	//ADC
	inout   [3:0] ADC_BUS,

	//SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

`ifdef MISTER_DUAL_SDRAM
	//Secondary SDRAM
	//Set all output SDRAM_* signals to Z ASAP if SDRAM2_EN is 0
	input         SDRAM2_EN,
	output        SDRAM2_CLK,
	output [12:0] SDRAM2_A,
	output  [1:0] SDRAM2_BA,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_nCS,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nWE,
`endif

	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..6 - USR2..USR6
	// Set USER_OUT to 1 to read from USER_IN.
	output	USER_OSD,
	// [MiSTer-DB9 BEGIN] - DB9/SNAC8 support: per-pin push-pull mask
	output	[7:0] USER_PP,
	// [MiSTer-DB9 END]
	input	[7:0] USER_IN,
	output	[7:0] USER_OUT,

	input         OSD_STATUS
);

// [MiSTer-DB9 BEGIN] - DB9/SNAC8 support: USER_PP default (port_batch replaces with USER_PP_DRIVE)
assign USER_PP = USER_PP_DRIVE;
// [MiSTer-DB9 END]

wire       ioctl_download;
wire       rom_download;
wire       direct_video;
wire       vfb_ioctl_wait;
wire       vfb_hblank, vfb_vblank, vfb_field;
wire       video_is_15khz, video_supports_120hz;
wire       vfb_video_mode_toggle, vfb_video_freeze;
wire [1:0] vfb_profile_res;
wire       fifo_full_led;
wire       save_hold_reset, save_pending, save_available, save_error;
logic [22:0] reset_timeout = 23'd0;

// Default values for unused framework ports.
assign ADC_BUS  = 'Z;

// [MiSTer-DB9 BEGIN] - DB9/SNAC8 support: joydb wrapper
wire         CLK_JOY = CLK_50M;                 // Assign clock between 40-50Mhz
wire   [1:0] joy_type        = status[62:61];   // 0=Off, 1=Saturn, 2=DB9MD, 3=DB15 (relocated from [127:126])
wire         joy_2p          = status[65];      // relocated from [125]
wire         joy_db9md_en    = (joy_type == 2'd2);
wire         joy_db15_en     = (joy_type == 2'd3);
wire         joy_any_en      = |joy_type;
// Legacy 3-bit alias for fork-specific MT32 / SNAC fallback code. Non-canonical
// RHS variants (ext_iec_en, mt32_disable) need a hand-port — alias is raw.
wire   [2:0] JOY_FLAG        = {joy_db9md_en, joy_db15_en, joy_2p};
// [MiSTer-DB9 END]

// [MiSTer-DB9-Pro BEGIN] - Saturn key gate
wire         saturn_unlocked;                   // driven by hps_io UIO_DB9_KEY (0xFE)
// [MiSTer-DB9-Pro END]

// [MiSTer-DB9 BEGIN] - DB9/SNAC8 support: joydb wrapper wires + instance
wire   [7:0] USER_OUT_DRIVE;
wire   [7:0] USER_PP_DRIVE;
wire  [15:0] joydb_1, joydb_2;
wire         joydb_1ena, joydb_2ena;
wire  [15:0] joy_raw_payload;

// [MiSTer-DB9 BEGIN] - DB9/SNAC8 support: probe-gating wires
// SNAC cores: replace 1'b0 with the core's SNAC enable expression so SNAC
// preempts the joydb wrapper on shared USER_IO pins. Default 1'b0 is no-op.
wire         snac_active     = 1'b0;
// MT32-pi probe-suppression gate. Auto-detected from MT32 signals declared
// elsewhere in this file (mt32_disable / mt32_use / mt32_on_primary). Hand-edit
// if the heuristic missed your core's gate expression. Suppresses the OSD-open
// autodetect probe so it doesn't read the RPi's I2C master traffic as a ghost
// Saturn signature. See the fork hazard notes.
wire         mt32_primary_active = 1'b0;
// [MiSTer-DB9 END]
// [MiSTer-DB9 BEGIN] - DB9 programmable-remap matrix wires
// joydb_*_mapped = MiSTer-standard joystick words (consumed in Layer B);
// db9_remap_* = 0xFD selector stream driven by the hps_io instance.
wire  [15:0] joydb_1_mapped, joydb_2_mapped;
wire         db9_remap_cmd;
wire   [5:0] db9_remap_byte_cnt;
wire  [15:0] db9_remap_din;
// [MiSTer-DB9 END]
// [MiSTer-DB9 BEGIN] - DB9 remap factory default (used until Main_MiSTer streams UIO 0xFD)
// Derived from CONF_STR J1, same rule as db9_map.cpp; lets the core work on a stock MiSTer binary.
// DB15:  Button 1=A, Button 2=B, Button 3=C, Button 4=D, Pause=F, View=E
// DB9MD: Button 1=A, Button 2=B, Button 3=C, Button 4=X, Pause=START, View=Y
wire  [35:0] db9_remap_default_db15  = 36'hFFF897654;
wire  [35:0] db9_remap_default_db9md = 36'hFFF8A7654;
// [MiSTer-DB9 END]
joydb joydb (
  .clk             ( CLK_JOY         ),
  .clk_sys         ( clk_sys            ),
  .USER_IN         ( USER_IN         ),
  .OSD_STATUS          ( OSD_STATUS          ),
  .snac_active         ( snac_active         ),
  .mt32_primary_active ( mt32_primary_active ),
  .joy_type        ( joy_type        ),
  .joy_2p          ( joy_2p          ),
  .saturn_unlocked ( saturn_unlocked ),
  .USER_OUT_DRIVE  ( USER_OUT_DRIVE  ),
  .USER_PP_DRIVE   ( USER_PP_DRIVE   ),
  .USER_OSD        ( USER_OSD        ),
  .joydb_1         ( joydb_1         ),
  .joydb_2         ( joydb_2         ),
  .joydb_1ena      ( joydb_1ena      ),
  .joydb_2ena      ( joydb_2ena      ),
  .remap_cmd       ( db9_remap_cmd      ),
  .remap_byte_cnt  ( db9_remap_byte_cnt ),
  .remap_din       ( db9_remap_din      ),
  .remap_default_db15  ( db9_remap_default_db15  ),
  .remap_default_db9md ( db9_remap_default_db9md ),
  .joydb_1_mapped  ( joydb_1_mapped     ),
  .joydb_2_mapped  ( joydb_2_mapped     ),
  .joy_raw         ( joy_raw_payload )
);

assign USER_OUT = USER_OUT_DRIVE;
// [MiSTer-DB9 END]
assign {UART_RTS, UART_TXD, UART_DTR} = 0;
assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;


assign LED_USER       = ioctl_download | fifo_full_led | save_pending | save_error;
assign LED_DISK       = 0;
assign LED_POWER      = 0;
assign BUTTONS        = 0;
assign VGA_SCALER     = 0;
assign VGA_DISABLE    = 0;
assign HDMI_BLACKOUT  = 0;
assign HDMI_BOB_DEINT = 0;

`include "build_id.v"
localparam CONF_STR = {
	"VECTREX;;",
	"-;",
	"f1,ART;",
	"FS1,VECBINROM;",
	"F2,ART,Load Overlay;",
	"-;",
	// Fixed profiles and two independently saved custom profiles.
	"H7P1,Video Profiles & Effects;",
	"P1O[68:66],Profile,80s Cruise Control,80s Overdrive,Neon Blue,Red Alert,Ultraviolet,Custom 1,Custom 2,A Touch of CRT;",
	"H9HAHBHCHDHEP1-;",
	"H9HAHBHCHDHEP1-,Crisp vectors. Gentle glow.;",
	"H9HAHBHCHDHEP1-,A conservatively adjusted;",
	"H9HAHBHCHDHEP1-,Vectrex for those who keep;",
	"H9HAHBHCHDHEP1-,the box and want the tube;",
	"H9HAHBHCHDHEP1-,to last forever.;",
	"H9HAHBHCHDHEP1-;",
	"H9HAHBHCHDHEP1-,For advanced settings;",
	"H9HAHBHCHDHEP1-,select Custom Profiles 1/2;",
	"h9HAP1-;",
	"h9HAP1-,The familiar Vectrex glow.;",
	"h9HAP1-,Luminous lines and bright;",
	"h9HAP1-,sparks against the dark.;",
	"h9HAP1-,Full of character with;",
	"h9HAP1-,the glow kept in balance.;",
	"h9HAP1-;",
	"h9HAP1-,For advanced settings;",
	"h9HAP1-,select Custom Profiles 1/2;",
	"hAH9P1-;",
	"hAH9P1-,Hot vectors and fuller glow;",
	"hAH9P1-,with trails that linger.;",
	"hAH9P1-,Vectrex turned up a notch.;",
	"hAH9P1-,A gentle phosphor shimmer;",
	"hAH9P1-,gives it a livelier pulse.;",
	"hAH9P1-;",
	"hAH9P1-,For advanced settings;",
	"hAH9P1-,select Custom Profiles 1/2;",
	"h9hAP1-;",
	"h9hAP1-,Electric blue neon blazes;",
	"h9hAP1-,white at its brightest.;",
	"h9hAP1-,A subdued blue halo hangs;",
	"h9hAP1-,in the dark behind it.;",
	"h9hAP1-;",
	"h9hAP1-,Best without an overlay.;",
	"h9hAP1-,Inspired by Neon Hawk.;",
	"h9hAP1-;",
	"h9hAP1-,For advanced settings;",
	"h9hAP1-,select Custom Profiles 1/2;",
	"hBP1-;",
	"hBP1-,Red warning lights ablaze.;",
	"hBP1-,Wild flicker. Fierce glow.;",
	"hBP1-,Long restless trails.;",
	"hBP1-,The CRT pushed past its;",
	"hBP1-,limits. An arcade meltdown.;",
	"hBP1-;",
	"hBP1-,Best without an overlay.;",
	"hBP1-;",
	"hBP1-,     Epilepsy warning:;",
	"hBP1-,    excessive flashing;",
	"hCP1-;",
	"hCP1-,Violet arcade fever dream.;",
	"hCP1-,Boosted until the phosphor;",
	"hCP1-,almost burns through.;",
	"hCP1-,Neon haze and long trails.;",
	"hCP1-,Spectacular explosions;",
	"hCP1-,flood the screen.;",
	"hCP1-;",
	"hCP1-,Best without an overlay.;",
	"hCP1-;",
	"hCP1-,     Epilepsy warning:;",
	"hCP1-,    excessive flashing;",
	"hDP1O[54:52],> Dwell Low Size,1,2,3,4,5,6,7,8;",
	"hDP1O[71:69],> Dwell Mid Growth,Same,+1,+2,+3,+4,+5,+6,+7;",
	"hDP1O[6:4],> Dwell High Growth,Same,+1,+2,+3,+4,+5,+6,+7;",
	"hDP1O[73:72],> Brightness,Reduced,Standard,Bright,Boost;",
	"hDP1O[76:74],> Bloom Width,Off,Thin,Tight,Soft,Normal,Broad,Wide-,Wide;",
	"hDH5P1O[79:77],> Bloom Curve,Minimal,Min+,Mild,Mild+,Moderate,Mod+,Strong-,Strong;",
	"hDP1O[82:80],> Halo,Off,0.25x,0.33x,0.5x,0.75x,1.0x,1.25x,1.5x;",
	"hDH6P1O[43:41],> Halo Curve,Minimal,Min+,Mild,Mild+,Moderate,Mod+,Strong-,Strong;",
	"hDH6P1O[84:83],> Halo Spread,Original,Wide 1,Wide 2,Focus;",
	"hDH6P1O[57:55],> Halo Compression,Off,2,4,6,8,10,16,24;",
	"hDH6P1O[38],> Halo Grid,16x16,8x8;",
	"hDP1O[86:85],> Inter-Frame Decay,Off,Short,Medium,Long;",
	"hDP1O[88:87],> Intra-Frame Decay,Off,LUT A,LUT B,LUT C;",
	"hDP1O[91:89],> Vector Color,Vectrex P4,Deluxe Blue,White,Red,Purple,Cyan,Blue;",
	"hEP1O[126:124],> Dwell Low Size,1,2,3,4,5,6,7,8;",
	"hEP1O[94:92],> Dwell Mid Growth,Same,+1,+2,+3,+4,+5,+6,+7;",
	"hEP1O[15:13],> Dwell High Growth,Same,+1,+2,+3,+4,+5,+6,+7;",
	"hEP1O[96:95],> Brightness,Reduced,Standard,Bright,Boost;",
	"hEP1O[99:97],> Bloom Width,Off,Thin,Tight,Soft,Normal,Broad,Wide-,Wide;",
	"hEH5P1O[102:100],> Bloom Curve,Minimal,Min+,Mild,Mild+,Moderate,Mod+,Strong-,Strong;",
	"hEP1O[105:103],> Halo,Off,0.25x,0.33x,0.5x,0.75x,1.0x,1.25x,1.5x;",
	"hEH6P1O[46:44],> Halo Curve,Minimal,Min+,Mild,Mild+,Moderate,Mod+,Strong-,Strong;",
	"hEH6P1O[107:106],> Halo Spread,Original,Wide 1,Wide 2,Focus;",
	"hEH6P1O[60:58],> Halo Compression,Off,2,4,6,8,10,16,24;",
	"hEH6P1O[28],> Halo Grid,16x16,8x8;",
	"hEP1O[109:108],> Inter-Frame Decay,Off,Short,Medium,Long;",
	"hEP1O[111:110],> Intra-Frame Decay,Off,LUT A,LUT B,LUT C;",
	"hEP1O[114:112],> Vector Color,Vectrex P4,Deluxe Blue,White,Red,Purple,Cyan,Blue;",
	"P2,Video Overlay & Geometry;",
	"P2-;",
	"P2O[24],Overlay,On,Off;",
	"h3P2O[10:8],Tint Strength,0,+1,+2,+3,+4,+5,+6,-1;",
	"h3P2O[27:25],Translucent Blend,50%,60%,70%,80%,10%,20%,30%,40%;",
	"h3P2O[34:32],Opaque Brightness,50%,60%,70%,80%,10%,20%,30%,40%;",
	"P2-;",
	"P2O[21:20],Orientation,Normal,90 CCW,90 CW;",
	"P2O[30],Framing,Overlay,Screen;",
	"P2O[31],View Area,Standard,Open Matte;",
	"P2O[29],Offset,Compensated,Zero;",
	"P2-;",
	"d0D1P2O[122],120Hz (720p only),Off,On;",
	"h0h1P2O[121],Direct Video Scan Rate,15 kHz,31 kHz;",
	"h2P2O[120],15 kHz Format,480i,240p;",
	"h1P2O[3:1],CRT Vertical Offset,0,+1,+2,+3,-1,-2,-3;",
	"-;",
	// [MiSTer-DB9-Pro BEGIN] - Saturn-first joy_type (relocated, upstream uses status[127:124])
	"O[62:61],UserIO Joystick,Off,Saturn,DB9MD,DB15;",
	"O[65],UserIO Players, 1 Player,2 Players;",
	// [MiSTer-DB9-Pro END]
	"-;",
	"P3,System;",
	"P3-;",
	"P3O[35],BIOS,Rev 3,Rev 1;",
	"P3O[11],Quick Boot,Yes,No;",
	"P3O[23:22],Beam Threshold,Z 20,Z 24,Z 28,Off;",
	"P3O[12],Port 2,Joystick,VecVoice (SP0256);",
	"d8P3O[19:18],Speech Level,Normal,Low,High;",
	"hFP3O[118],Cartridge Autosave,On,Off;",
	"hFP3R[119],Save Cartridge;",
	"h4P3-,Cartridge save error;",
	"P3-;",
	"P3O[116],Pause when OSD open,Off,On;",
	"P3O[117],Dim video after 60s,On,Off;",
	"P4,Diagnostic;",
	"P4-;",
	"P4O[123],Effects Bypass,Off,On;",
	"P4O[48:47],Reference Dwell,46,72,Standard,104;",
	"P4O[50:49],Velocity Comp.,0.3,0.4,0.5,Off;",
	"P4O[64:63],Halo Correction,None,50%,75%,100%;",
	"P4-;",
	"P4O[40:39],Buffer Mode,EOF + VBL,VBL,EOF;",
	"P4O[17:16],Aspect Ratio,Optimized,Stretched,Pixel Perfect;",
	"-;",
	"P5,Core Info;",
	"P5-;",
	"P5-,Enhanced Vectrex core;",
	"P5-,     by Videodr0me 2026;",
	"P5-;",
	"P5-,Based on Sorgelig's MiSTer;",
	"P5-,port of Dar's original;",
	"P5-,DE10-Lite core, with;",
	"P5-,additional work by alanswx.;",
	"P5-;",
	"P5-,If you enjoy reliving the;",
	"P5-,golden age of vector games,;",
	"P5-,please support my work and;",
	"P5-,future updates:;",
	"P5-;",
	"P5-,buymeacoffee.com/videodr0me;",
	"-;",
	"R7,Reset;",
	"J1,Button 1,Button 2,Button 3,Button 4,Pause,View;",
	"jn,A,B,X,Y,Select,R;",
	"I,",
	"Overlay On / Framing: Overlay,",
	"Overlay Off / Framing: Overlay,",
	"Overlay Off / Framing: Screen,",
	"Overlay Off / Open Matte,",
	"Profile: 80s Cruise Control,",
	"Profile: 80s Overdrive,",
	"Profile: Neon Blue,",
	"Profile: Red Alert,",
	"Profile: Ultraviolet,",
	"Profile: Custom 1,",
	"Profile: Custom 2,",
	"Profile: A Touch of CRT,",
	"Copied to Custom 1,",
	"Copied to Custom 2;",
	// Keep version metadata after selectable entries; MiSTer counts it during action lookup.
	"v,1;",
	"V,v1.0.",`BUILD_DATE
};

////////////////////   CLOCKS   ///////////////////

wire        clk_sys;
wire        machine_pll_locked_raw;
logic [1:0] machine_pll_lock_sync = 2'b00;

pll pll
(
	.refclk  (CLK_50M),
	.rst     (1'b0),
	.outclk_0(clk_sys),
	.locked  (machine_pll_locked_raw)
);

always_ff @(posedge clk_sys or negedge machine_pll_locked_raw) begin
	if (!machine_pll_locked_raw) machine_pll_lock_sync <= 2'b00;
	else machine_pll_lock_sync <= {machine_pll_lock_sync[0], 1'b1};
end

// videodr0me_fb runs at 125 MHz; see rtl/pll_vfb for why it needs its own PLL.
wire clk_125;
wire pll_vfb_locked;
pll_vfb pll_vfb
(
	.refclk  (CLK_50M),
	.rst     (1'b0),
	.outclk_0(clk_125),
	.locked  (pll_vfb_locked)
);

///////////////////////////////////////////////////

wire [127:0] status;
vfb_control_pkg::profile_settings_t profile_settings;
vfb_control_pkg::diagnostic_settings_t diagnostic_settings;
wire         profile_bypass;
wire [15:0]  profile_menu_mask;
wire [127:0] profile_status_in;
wire         profile_status_set;
wire         menu_info_req;
wire [7:0]   menu_info;
wire [10:0]  ps2_key;
// [MiSTer-DB9 BEGIN] - DB9/SNAC8 support: USB joysticks renamed, DB9 merged in
wire [15:0]  joystick_0_USB, joystick_1_USB;
wire [15:0]  joystick_0 = joydb_1ena ? (OSD_STATUS ? 16'd0 : joydb_1_mapped[9:0]) : joystick_0_USB;
wire [15:0]  joystick_1 = joydb_2ena ? (OSD_STATUS ? 16'd0 : joydb_2_mapped[9:0]) : joydb_1ena ? joystick_0_USB : joystick_1_USB;
// [MiSTer-DB9 END]
wire         artwork_valid_125;
(* altera_attribute = {"-name SYNCHRONIZER_IDENTIFICATION FORCED_IF_ASYNCHRONOUS"} *)
logic artwork_valid_meta = 1'b0, artwork_valid_sync = 1'b0;
logic artwork_valid_seen = 1'b0;
always_ff @(posedge clk_sys) begin
	artwork_valid_meta <= artwork_valid_125;
	artwork_valid_sync <= artwork_valid_meta;
	if (!artwork_valid_sync)
		artwork_valid_seen <= 1'b0;
	else if (!RESET)
		artwork_valid_seen <= 1'b1;
end
wire artwork_loaded = artwork_valid_sync && !artwork_valid_seen;
// Touch is the only profile for which mask bits 14:9 are all clear.
wire [15:0] status_menu_mask = {save_available, profile_menu_mask[14:9],
	status[12], profile_menu_mask[7:5], save_error, profile_menu_mask[3:0]};

vfb_profile_resolver profile_resolver (
	.clk_sys             (clk_sys),
	.reset               (RESET),
	.status              (status),
	.profile_resolution  (vfb_profile_res),
	.video_freeze        (vfb_video_freeze),
	.video_is_15khz      (video_is_15khz),
	.video_supports_120hz(video_supports_120hz),
	.direct_video        (direct_video),
	.artwork_loaded      (artwork_loaded),
	.artwork_present     (artwork_valid_sync),
	.view_button         (joystick_0[9] | joystick_1[9]),
	.osd_open            (OSD_STATUS),
	.ps2_key             (ps2_key),

	.profile_settings    (profile_settings),
	.diagnostic_settings (diagnostic_settings),
	.effects_bypass      (profile_bypass),
	.menu_mask           (profile_menu_mask),
	.status_in           (profile_status_in),
	.status_set          (profile_status_set),
	.info_req            (menu_info_req),
	.info                (menu_info)
);
wire  [1:0] buttons;

wire [15:0] joya_0, joya_1;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire [7:0]  ioctl_dout;
wire [15:0] ioctl_index;
wire [32:0] timestamp;
wire [17:0] backup_addr;
wire [7:0]  backup_data, backup_q;
wire        backup_wr, flash_enabled, flash_busy, flash_changed;
wire [5:0]  flash_sector;
wire [31:0] sd_lba;
wire        sd_rd, sd_wr, sd_ack, sd_buff_wr;
wire [13:0] sd_buff_addr;
wire [7:0]  sd_buff_dout, sd_buff_din;
wire        img_mounted, img_readonly;
wire [63:0] img_size;

hps_io #(.CONF_STR(CONF_STR)) hps_io
(
	.clk_sys            (clk_sys),
	.HPS_BUS            (HPS_BUS),

	.buttons            (buttons),
	.status             (status),
	.status_in          (profile_status_in),
	.status_set         (profile_status_set),
	.info_req           (menu_info_req),
	.info               (menu_info),
	.ps2_key            (ps2_key),
	.status_menumask    (status_menu_mask),
	.direct_video       (direct_video),
	.new_vmode          (vfb_video_mode_toggle),
	.TIMESTAMP          (timestamp),
	.img_mounted        (img_mounted),
	.img_readonly       (img_readonly),
	.img_size           (img_size),
	.sd_lba             ('{sd_lba}),
	.sd_blk_cnt         ('{6'd0}),
	.sd_rd              (sd_rd),
	.sd_wr              (sd_wr),
	.sd_ack             (sd_ack),
	.sd_buff_addr       (sd_buff_addr),
	.sd_buff_dout       (sd_buff_dout),
	.sd_buff_din        ('{sd_buff_din}),
	.sd_buff_wr         (sd_buff_wr),

	.ioctl_download     (ioctl_download),
	.ioctl_wr           (ioctl_wr),
	.ioctl_addr         (ioctl_addr),
	.ioctl_dout         (ioctl_dout),
	.ioctl_index        (ioctl_index),
	.ioctl_wait         (vfb_ioctl_wait),

	.joystick_l_analog_0(joya_0),
	.joystick_l_analog_1(joya_1),
	.joystick_0         (joystick_0_USB),
	.joystick_1         (joystick_1_USB),
	// [MiSTer-DB9 BEGIN] - DB9/SNAC8 support: joy_raw
	.joy_raw(joy_raw_payload),
	// programmable remap matrix selector load (UIO_DB9_MAP 0xFD)
	.db9_remap_cmd(db9_remap_cmd),
	.db9_remap_byte_cnt(db9_remap_byte_cnt),
	.db9_remap_din(db9_remap_din),
	// [MiSTer-DB9 END]
	// [MiSTer-DB9-Pro BEGIN] - Saturn key gate
	.saturn_unlocked(saturn_unlocked),
	// [MiSTer-DB9-Pro END]
);

vectrex_flash_save flash_save (
	.clk          (clk_sys),
	.download     (rom_download),
	.download_wr  (ioctl_wr && rom_download),
	.download_data(ioctl_dout),
	.flash_enabled(flash_enabled),
	.flash_busy   (flash_busy),
	.flash_changed(flash_changed),
	.flash_sector (flash_sector),
	.cart_addr    (backup_addr),
	.cart_data    (backup_data),
	.cart_wr      (backup_wr),
	.cart_q       (backup_q),
	.autosave     (!status[118]),
	.save_request (status[119]),
	.hold_reset   (save_hold_reset),
	.pending      (save_pending),
	.available    (save_available),
	.error        (save_error),
	.img_mounted  (img_mounted),
	.img_readonly (img_readonly),
	.img_size     (img_size),
	.sd_lba       (sd_lba),
	.sd_rd        (sd_rd),
	.sd_wr        (sd_wr),
	.sd_ack       (sd_ack),
	.sd_buff_addr (sd_buff_addr),
	.sd_buff_dout (sd_buff_dout),
	.sd_buff_wr   (sd_buff_wr),
	.sd_buff_din  (sd_buff_din)
);

wire       pause_cpu;
wire [9:0] audio;
assign AUDIO_L   = pause_cpu ? 16'd0 : {audio, 6'd0};
assign AUDIO_R   = pause_cpu ? 16'd0 : {audio, 6'd0};
assign AUDIO_S   = 1;
assign AUDIO_MIX = 0;

// Span two MC6809 fall-E reset samples without disturbing video state.
logic       bios_factory_q = 1'b0;
logic [5:0] bios_reset_cnt = 6'd0;
always_ff @(posedge clk_sys) begin
	if (bios_factory_q != status[35]) begin
		bios_factory_q <= status[35];
		bios_reset_cnt <= 6'd32;
	end
	else if (bios_reset_cnt != 0)
		bios_reset_cnt <= bios_reset_cnt - 1'b1;
end
wire bios_reset = bios_reset_cnt != 0;

// Cartridge loading and menu reset stop machine/source timing while the
// current framebuffer continues presenting. Framework resets restart both.
wire video_reset = !machine_pll_lock_sync[1] | RESET | status[0] | buttons[1];
wire delayed_reset = (reset_timeout != 23'd0) &&
	(reset_timeout <= 23'd999);
wire machine_reset = video_reset | status[7] | rom_download | save_hold_reset | delayed_reset;
wire core_reset = machine_reset | bios_reset;

// A delayed warm reset preserves RAM state and skips the GCE startup animation.
always_ff @(posedge clk_sys) begin
	if ((rom_download || save_hold_reset) && !status[11])
		reset_timeout <= 23'd5_000_001;
	else if (reset_timeout != 23'd0)
		reset_timeout <= reset_timeout - 1'b1;
end


assign VGA_SL      = 0;
assign VGA_F1      = vfb_field;
assign HDMI_FREEZE = vfb_video_freeze;
assign VGA_DE      = ~(vfb_hblank | vfb_vblank);

wire        vfb_clk_video, vfb_ce_pixel;
wire [7:0]  vfb_r, vfb_g, vfb_b;
wire        vfb_hs, vfb_vs;
wire [23:0] paused_rgb;

pause #(.RW(8), .GW(8), .BW(8), .CLK_HZ(24_000_000)) pause_inst
(
	.clk_sys      (clk_sys),
	.reset        (machine_reset),
	.user_button  (joystick_0[8] | joystick_1[8]),
	.pause_request(1'b0),
	.options      ({~status[117], status[116]}),
	.OSD_STATUS   (OSD_STATUS),
	.r            (vfb_r),
	.g            (vfb_g),
	.b            (vfb_b),
	.pause_cpu    (pause_cpu),
	.rgb_out      (paused_rgb)
);

assign CLK_VIDEO = vfb_clk_video;
assign CE_PIXEL  = vfb_ce_pixel;
assign VGA_R     = paused_rgb[23:16];
assign VGA_G     = paused_rgb[15:8];
assign VGA_B     = paused_rgb[7:0];
assign VGA_HS    = vfb_hs;
assign VGA_VS    = vfb_vs;

// Beam state exported by the machine core to the external renderer.
wire signed [19:0] beam_x, beam_y;
wire [7:0]         beam_z;
wire               beam_on, beam_ce, beam_zero_n;
wire               t2_reload;
wire [15:0]        t2_period;


// boot2.rom contains artwork; boot.rom remains the startup cartridge.
assign rom_download = ioctl_download && (ioctl_index[4:0] <= 1) &&
	(ioctl_index[9:8] == 0) && (ioctl_index != 16'h0080);

vectrex vectrex
(
	.reset        (core_reset),
	.pause        (pause_cpu && !bios_reset),
	.clock        (clk_sys),
	.bios_factory (bios_factory_q),
	.boot_reset   (status[0]),
	.timestamp    (timestamp),

	.cart_download(rom_download),
	.cart_data    (ioctl_dout),
	.cart_addr    (ioctl_addr),
	.cart_wr      (ioctl_wr && rom_download),
	.backup_addr  (backup_addr),
	.backup_data  (backup_data),
	.backup_wr    (backup_wr),
	.backup_q     (backup_q),
	.flash_enabled(flash_enabled),
	.flash_busy   (flash_busy),
	.flash_changed(flash_changed),
	.flash_sector (flash_sector),

	.speech_mode  (status[12]),
	.speech_level (status[19:18]),
	.audio_out    (audio),

	.up_1         (joystick_0[4]),
	.dn_1         (joystick_0[5]),
	.lf_1         (joystick_0[6]),
	.rt_1         (joystick_0[7]),
	.pot_x_1      (joya_0[7:0]  ? joya_0[7:0]   : {joystick_0[1], {7{joystick_0[0]}}}),
	.pot_y_1      (joya_0[15:8] ? ~joya_0[15:8] : {joystick_0[2], {7{joystick_0[3]}}}),

	.up_2         (joystick_1[4]),
	.dn_2         (joystick_1[5]),
	.lf_2         (joystick_1[6]),
	.rt_2         (joystick_1[7]),
	.pot_x_2      (joya_1[7:0]  ? joya_1[7:0]   : {joystick_1[1], {7{joystick_1[0]}}}),
	.pot_y_2      (joya_1[15:8] ? ~joya_1[15:8] : {joystick_1[2], {7{joystick_1[3]}}}),

	.beam_x       (beam_x),
	.beam_y       (beam_y),
	.beam_on      (beam_on),
	.beam_z       (beam_z),
	.beam_ce      (beam_ce),
	.beam_zero_n  (beam_zero_n),
	.t2_reload    (t2_reload),
	.t2_period    (t2_period)
);


// ---------------------------------------------------------------------------
// vectrex_video maps the emulated beam into the selected raster and feeds the
// external framebuffer and CRT-effects pipeline.
// ---------------------------------------------------------------------------
vectrex_video vectrex_video
(
	.clk_sys            (clk_sys),
	.clk_50             (CLK_50M),
	.clk_125            (clk_125),
	.reset              (video_reset),
	.vfb_clock_reset    (!pll_vfb_locked),
	.reset_source       (machine_reset),

	.beam_x             (beam_x),
	.beam_y             (beam_y),
	.beam_z             (beam_z),
	.beam_on            (beam_on),
	.beam_tick          (beam_ce),
	.beam_zero_n        (beam_zero_n),
	.t2_reload          (t2_reload),
	.t2_period          (t2_period),

	.hdmi_height        (HDMI_HEIGHT),
	.direct_video       (direct_video),
	.direct_video_31khz (status[121]),
	.crt_15khz_480i     (!status[120]),
	.crt_vertical_position (status[3:1]),
	.mode_120hz         (status[122]),
	.aspect_ratio       (status[17:16]),

	.v_orient           (status[21:20]),
	.open_matte         (status[31]),
	.zoom_normal        (status[30]),
	.vertical_offset    (!status[29]),
	.beam_threshold     (status[23:22]),

	.overlay_off        (status[24]),
	.ovl_tint           (status[10:8]),
	.ovl_bright         (status[27:25]),
	.ovl_opaque_bright  (status[34:32]),
	.profile_settings   (profile_settings),
	.diagnostic_settings(diagnostic_settings),
	.effects_bypass     (profile_bypass),
	.ioctl_download     (ioctl_download),
	.ioctl_wr           (ioctl_wr),
	.ioctl_index        (ioctl_index),
	.ioctl_addr         (ioctl_addr),
	.ioctl_data         (ioctl_dout),
	.ioctl_wait         (vfb_ioctl_wait),
	.artwork_valid      (artwork_valid_125),
	// Mode 0 promotes completed redraws at VBLANK. Accepted Timer 2 reloads
	// define normal boundaries; the armed fallback uses ZERO or timeout.
	.buffer_mode        (status[40:39]),
	.clk_video          (vfb_clk_video),
	.ce_pixel           (vfb_ce_pixel),
	.vga_r              (vfb_r),
	.vga_g              (vfb_g),
	.vga_b              (vfb_b),
	.vga_hs             (vfb_hs),
	.vga_vs             (vfb_vs),
	.vga_hblank         (vfb_hblank),
	.vga_vblank         (vfb_vblank),
	.field              (vfb_field),
	.video_arx          (VIDEO_ARX),
	.video_ary          (VIDEO_ARY),
	.mode_is_15khz      (video_is_15khz),
	.mode_supports_120hz(video_supports_120hz),
	.profile_resolution (vfb_profile_res),
	.video_mode_toggle  (vfb_video_mode_toggle),
	.video_freeze       (vfb_video_freeze),

	.ddram_clk          (DDRAM_CLK),
	.ddram_busy         (DDRAM_BUSY),
	.ddram_burstcnt     (DDRAM_BURSTCNT),
	.ddram_addr         (DDRAM_ADDR),
	.ddram_dout         (DDRAM_DOUT),
	.ddram_dout_ready   (DDRAM_DOUT_READY),
	.ddram_rd           (DDRAM_RD),
	.ddram_din          (DDRAM_DIN),
	.ddram_be           (DDRAM_BE),
	.ddram_we           (DDRAM_WE),

	.sdram_dq           (SDRAM_DQ),
	.sdram_clk          (SDRAM_CLK),
	.sdram_cke          (SDRAM_CKE),
	.sdram_ncs          (SDRAM_nCS),
	.sdram_nras         (SDRAM_nRAS),
	.sdram_ncas         (SDRAM_nCAS),
	.sdram_nwe          (SDRAM_nWE),
	.sdram_dqml         (SDRAM_DQML),
	.sdram_dqmh         (SDRAM_DQMH),
	.sdram_a            (SDRAM_A),
	.sdram_ba           (SDRAM_BA),

	.fifo_full_led      (fifo_full_led)
);

endmodule
