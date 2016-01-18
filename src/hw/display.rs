extern crate sdl2;

use std::path::Path;
use super::super::mem::Memory;
use super::super::bios::BdaEntry;

// 60 FPS
const SYNC_PERIOD_NS: u64 = 16666667;

/* Emulates a CGA graphics adapter */
const VRAM_ADDR: u32 = 0xb8000;

use super::sdl2::pixels::Color;
const CGA_PALETTE: [Color;16] =
[
	Color::RGB(0,0,0), // black
	Color::RGB(0,0,0xaa), // blue
	Color::RGB(0,0xaa,0), // green
	Color::RGB(0,0xaa,0xaa), // cyan
	Color::RGB(0xaa,0,0), // red
	Color::RGB(0xaa,0,0xaa), // magenta
	Color::RGB(0xaa,0x55,0), // brown
	Color::RGB(0xaa,0xaa,0xaa), // grey
	Color::RGB(0x55,0x55,0x55), // light grey
	Color::RGB(0x55,0x55,0xff), // light blue
	Color::RGB(0x55,0xff,0x55), // light green
	Color::RGB(0x55,0xff,0xff), // light cyan
	Color::RGB(0xff,0x55,0x55), // light red
	Color::RGB(0xff,0x55,0xff), // light magenta
	Color::RGB(0xff,0xff,0x55), // yellow
	Color::RGB(0xff,0xff,0xff), // white
];

#[derive(Clone,Copy)]
pub enum GraphicMode // TODO: video mode?
{
	//T4025,
	T8025,
	//G320200,
	G640200
}

impl GraphicMode
{
	pub fn cols(&self) -> u32
	{
		match *self
		{
			GraphicMode::T8025 => 80,
			GraphicMode::G640200 => 80
		}
	}
	pub fn rows(&self) -> u32
	{
		match *self
		{
			GraphicMode::T8025 => 25,
			GraphicMode::G640200 => 25
		}
	}
	pub fn page_size(&self) -> u32
	{
		match *self
		{
			GraphicMode::T8025 => 4096,
			GraphicMode::G640200 => 16384
		}
	}
	pub fn page_count(&self) -> u8
	{
		match *self
		{
			GraphicMode::T8025 => 4,
			GraphicMode::G640200 => 1
		}
	}

	pub fn from_bios(mode: u8) -> GraphicMode
	{
		match mode
		{
			0x3 => GraphicMode::T8025,
			0x6 => GraphicMode::G640200,
			_ => panic!("Unsupported video mode: {}, mode")
		}
	}

	pub fn to_bios(&self) -> u8
	{
		match *self
		{
			GraphicMode::T8025 => 0x3,
			GraphicMode::G640200 => 0x6
		}
	}
}

pub struct Cursor
{
	x: BdaEntry,
	y: BdaEntry
}

impl Cursor
{
	fn new(bda_idx: u8) -> Cursor
	{
		Cursor
		{
			x: BdaEntry::new(bda_idx),
			y: BdaEntry::new(bda_idx + 1)
		}
	}
}

// Tell one in VRAM_USED_INTERVAL in the status register that VRAM is in use
const VRAM_USED_INTERVAL: u32 = 1;

pub struct Display
{
	mode: GraphicMode,
	window: sdl2::video::Window,

	font: sdl2::surface::Surface<'static>,
	horrible_bw_surface_table: sdl2::surface::Surface<'static>,
	cursors: [Cursor; 8],
	cur_page: u8,
	last_sync_ns: u64,

	count_until_vram_used: u32
}

struct BlittableWindow<'a>
{
	window: &'a mut sdl2::video::Window,
	event_pump: &'a sdl2::EventPump
}

impl<'a> AsMut<sdl2::surface::SurfaceRef> for BlittableWindow<'a>
{
	fn as_mut(&mut self) -> &mut sdl2::surface::SurfaceRef
	{
		self.window.surface_mut(self.event_pump).unwrap()
	}
}

impl Display
{
	pub fn new(sdl: &sdl2::Sdl) -> Display
	{
		let window = Display::create_window(&sdl.video().unwrap(), 640, 400);
		let mut font = sdl2::surface::Surface::load_bmp(Path::new("cga_font.bmp")).unwrap();
		let horrible_bw_surface_table = sdl2::surface::Surface::load_bmp(Path::new("horrible_bw_surface_table.bmp")).unwrap();
		let cur = |x: u8| -> Cursor { Cursor::new(x) };

		font.set_color_key(true, sdl2::pixels::Color::RGB(0,0,0)).unwrap();

		Display
		{
			mode: GraphicMode::T8025,
			window: window,
			font: font,
			horrible_bw_surface_table: horrible_bw_surface_table,
			cursors: [cur(0x50), cur(0x52), cur(0x54), cur(0x56), cur(0x58), cur(0x5a), cur(0x5c), cur(0x5e)],
			cur_page: 0, // TODO: should be BdaEntry
			count_until_vram_used: VRAM_USED_INTERVAL,
			last_sync_ns: 0
		}
	}

	fn page_addr(&self, page: u8) -> u32
	{
		VRAM_ADDR + self.mode.page_size() * (page as u32)
	}

	fn chr_addr(&self, page: u8, x: u8, y: u8) -> u32
	{
		self.page_addr(page) + ((y as u32) * self.mode.cols() + (x as u32)) * 2
	}

	fn cur_addr(&self, mem: &Memory, page: u8) -> u32
	{
		self.chr_addr(page, self.cursors[page as usize].x.get(mem), self.cursors[page as usize].y.get(mem))
	}

	fn create_window(video: &sdl2::VideoSubsystem, width: u32, height: u32) -> sdl2::video::Window
	{
		let builder = sdl2::video::WindowBuilder::new(&video, "Riapyx", width, height);
		builder.build().unwrap()
	}

	pub fn render(&mut self, event_pump: &sdl2::EventPump, mem: &mut Memory, clock: u32, time_ns: u64)
	{
		// Only update once every 10k clock cycles to avoid artifacts when removing a character...
		// TODO: have tty output set the dirty flag to avoid this
		if time_ns < self.last_sync_ns + SYNC_PERIOD_NS || (cfg!(feature="vram-dirty") && !(mem.is_vram_dirty() && clock % 10000 == 0))
		{
			return
		}

		self.last_sync_ns = time_ns;
		mem.clear_vram_dirty();
		// display_print!("Sync");

		match self.mode
		{
			GraphicMode::T8025 => 
			{
				/* Display screen content */
				for y in 0 .. 25
				{
					for x in 0 .. 80
					{
						let addr = self.chr_addr(self.cur_page, x, y);
						let chr = mem.read_u8(addr);
						let attr = mem.read_u8(addr + 1);

						let src_rect = sdl2::rect::Rect::new(((chr % 32) as i32) * 8, ((chr / 32) as i32) * 8, 8, 8).unwrap();
						let dst_rect = sdl2::rect::Rect::new(x as i32 * 8, y as i32 * 16, 8, 16).unwrap();

						let text_color = CGA_PALETTE[(attr & 0x0f) as usize];
						let back_color = CGA_PALETTE[((attr >> 4) & 0x7) as usize]; // Last bit is blink/not blink

						self.window.surface_mut(event_pump).unwrap().fill_rect(dst_rect, back_color).unwrap();
						self.font.set_color_mod(text_color);

						let target = BlittableWindow
						{
							window: &mut self.window,
							event_pump: &event_pump
						};

						self.font.blit_scaled(src_rect, target, dst_rect).unwrap();
					}
				}

				/* Display cursor */
				let (x, y) = self.tty_coords(mem, self.cur_page);
				let cur_rect = sdl2::rect::Rect::new(x as i32 * 8, y as i32 * 16 + 14, 8, 2).unwrap();
				self.window.surface_mut(event_pump).unwrap().fill_rect(cur_rect, CGA_PALETTE[7]).unwrap();
			}
			GraphicMode::G640200 =>
			{
				for y in 0 .. 200
				{
					for x in 0 .. (640 / 8)
					{
						let addr = self.page_addr(self.cur_page) + (y%2) * 8192 + (y / 2) * 80 + x;
						let block = mem.read_u8(addr);
						
						let src_rect = sdl2::rect::Rect::new(0, block as i32, 8, 1).unwrap();
						let dst_rect = sdl2::rect::Rect::new(x as i32 * 8, (y as i32) * 2, 8, 2).unwrap();

						let target = BlittableWindow
						{
							window: &mut self.window,
							event_pump: &event_pump
						};

						self.horrible_bw_surface_table.blit_scaled(src_rect, target, dst_rect).unwrap();
					}
				}
			}
		}

		self.window.update_surface().unwrap();
	}

	pub fn set_mode(&mut self, mode: GraphicMode)
	{
		self.mode = mode;
	}

	pub fn tty_scroll(&mut self, mem: &mut Memory, page: u8, x1: u8, y1: u8, x2_: u8, y2_: u8, attr: u8)
	{
		let x2 = if x2_ >= self.mode.cols() as u8 { self.mode.cols() as u8 - 1 } else { x2_ };
		let y2 = if y2_ >= self.mode.rows() as u8 { self.mode.rows() as u8 - 1 } else { y2_ };

		let topleft = self.page_addr(page);
		for y in y1 .. y2
		{
			for x in x1 .. (x2 + 1)
			{
				let new = mem.read_u16(topleft + ((y as u32 + 1) * self.mode.cols() + x as u32) * 2);
				mem.write_u16(topleft + (y as u32 * self.mode.cols() + x as u32) * 2, new)
			}
		}

		for x in x1 .. (x2 + 1)
		{
			mem.write_u16(topleft + (y2 as u32 * self.mode.cols() + x as u32) * 2, (attr as u16) << 8);
		}
	}

	pub fn tty_coords(&self, mem: &Memory, page: u8) -> (u8, u8)
	{
		(self.cursors[page as usize].x.get(mem), self.cursors[page as usize].y.get(mem))
	}

	pub fn tty_setcoords(&mut self, mem: &mut Memory, page: u8, x: u8, y: u8)
	{
		self.cursors[page as usize].x.set(mem, x);
		self.cursors[page as usize].y.set(mem, y);
	}

	pub fn tty_read_at_cur(&self, mem: &Memory, page: u8) -> (u8, u8)
	{
		let addr = self.cur_addr(mem, page);
		(mem.read_u8(addr), mem.read_u8(addr+1))
	}

	pub fn tty_output(&mut self, mem: &mut Memory, page: u8, chr: u8, text_color: u8)
	{
		const CR: u8 = 0xd;
		const LF: u8 = 0xa;
		const BACKSPACE: u8 = 0x8;

		/* Print + cursor update */
		let target_addr = self.cur_addr(mem, page);
		let (x, y) = self.tty_coords(mem, page);

		let (new_x, new_y) = 
			match chr
			{
				CR => (0, y),
				LF => (x, y+1),
				BACKSPACE =>
				{
					(if x > 0 { x-1 } else {x}, y)
				}
				_ =>
				{
					assert!(target_addr < VRAM_ADDR + 16 * 1024);
					mem.write_u8(target_addr, chr);
					mem.write_u8(target_addr + 1, text_color);
					if x + 1 >= 80
					{ (0, y+1) }
					else
					{ (x+1, y) }
				}
			};
		self.tty_setcoords(mem, page, new_x, new_y);

		/* Scroll if necessary */
		if new_y >= 25
		{
			let cols = self.mode.cols() as u8;
			let rows = self.mode.rows() as u8;
			self.tty_scroll(mem, page, 0, 0, cols, rows, 0);

			self.cursors[page as usize].y.set(mem, new_y - 1);
		}
	}

	pub fn write_char_at_cur(&self, mem: &mut Memory, page: u8, chr: u8, attr: u8, cnt: u16)
	{
		let addr = self.cur_addr(mem, page);

		let (_x, _y) = self.tty_coords(mem, self.cur_page);
		display_print!("Write char {:x} at {}/{} {} times", chr, _x, _y, cnt);

		for idx in 0 .. cnt as u32
		{
			mem.write_u8(addr + idx * 2, chr);
			mem.write_u8(addr + idx * 2 + 1, attr);
		}
	}

	pub fn set_page(&mut self, page: u8)
	{
		assert!(page < self.mode.page_count());
		self.cur_page = page;
	}

	pub fn cur_page(&self) -> u8
	{
		self.cur_page
	}

	pub fn get_mode(&self) -> GraphicMode
	{
		self.mode
	}

	pub fn get_status_reg(&mut self) -> u8
	{
		/* For some reason, EDIT seems to want VRAM to be used some times */
		let status_reg = 
			if self.count_until_vram_used > 0
			{
				self.count_until_vram_used -= 1;
				9
			}
			else
			{
				self.count_until_vram_used = VRAM_USED_INTERVAL;
				0
			};

		display_print!("Get status reg = {}", status_reg);
		status_reg
	}
}
