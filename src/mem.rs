use std::boxed::Box;

pub struct Memory
{
	ram: Box<[u8]>,
	dirty: bool
}

fn bound_checks(what: &str, addr: u32)
{
	/*if (addr > 0xf0000)
	{
		debug_print!("{} ROM @ address {:06x}", what, addr);
	}
	//debug_print!("{} stuff @ address {:06x}", what, addr);
	if (addr < 0x400)
	{
		debug_print!("{} IVT @ address {:06x} (irq={:x})", what, addr, addr/4);
	}*/
	if cfg!(feature="bda-tracking") && addr >= 0x400 && addr < 0x500 && addr != 0x46c && addr != 0x46e && addr != 0x417 && addr != 0x418 && addr != 0x450 && addr != 0x451 && addr != 0x470 && addr != 0x410
	{
		debug_print!("{} BDA @ address {:06x}", what, addr);
	}
	/*if addr >= 0xb8000 && addr < (0xb8000 + 16 * 1024)
	{
		debug_print!("{} VRAM @ address {:06x}", what, addr);
	} */
	if cfg!(feature="vram-check") && (addr >= 0xa0000 && addr < 0xb8000) || (addr >= (0xb8000 + 16 * 1024) && addr < (0xb8000 + 64 * 1024))
	{
		debug_print!("Warning: {} VRAM @ address {:06x}", what, addr);
	}
}

fn read_bound_checks(addr: u32) {bound_checks("Reading from",addr)}
fn write_bound_checks(addr: u32) {bound_checks("Writing to",addr)}

const ADDR_MAX: u32 = 0x00100000;
const VRAM_MASK: u32 = 0xf8000;
const VRAM_VAL: u32 = 0xb8000;

impl Memory
{
	// TODO: provide ROM and prevent write access to it
	pub fn new(size: u32) -> Memory
	{
		Memory
		{
			ram: vec![0; size as usize].into_boxed_slice(),
			dirty: true
			//rom: rom_vec.into_boxed_slice()
		}
	}

	pub fn read_u8(&self, addr: u32) -> u8
	{
		read_bound_checks(addr);
		self.ram[(addr % ADDR_MAX) as usize]
	}

	pub fn read_u16(&self, addr: u32) -> u16
	{
		read_bound_checks(addr);
		(self.ram[(addr % ADDR_MAX) as usize] as u16) + ((self.ram[((addr + 1) % ADDR_MAX) as usize] as u16) << 8)
	}

	pub fn write_u8(&mut self, addr: u32, data: u8)
	{
		write_bound_checks(addr);
		if cfg!(feature="vram-dirty") && addr & VRAM_MASK == VRAM_VAL
		{
			self.dirty = true;
		}
		self.ram[(addr % ADDR_MAX) as usize] = data
	}

	pub fn write_u16(&mut self, addr: u32, data: u16)
	{
		write_bound_checks(addr);
		if cfg!(feature="vram-dirty") && addr & VRAM_MASK == VRAM_VAL
		{
			self.dirty = true;
		}
		self.ram[(addr % ADDR_MAX) as usize] = (data & 0xFF) as u8;
		self.ram[((addr + 1) % ADDR_MAX) as usize] = (data>>8) as u8
	}

	pub fn slice_from(&self, addr: u32) -> &[u8]
	{
		let len = self.ram.len();
		&self.ram[(addr as usize) .. len]
	}

	pub fn slice(&self, addr: u32, len: u32) -> &[u8]
	{
		&self.ram[(addr as usize) .. ((addr + len) as usize)]
	}

	pub fn clear_vram_dirty(&mut self)
	{
		self.dirty = false;
	}

	pub fn is_vram_dirty(&self) -> bool
	{
		self.dirty
	}
}