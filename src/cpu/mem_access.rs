use mem::Memory;
use super::base::*;
use super::reg_access::*;

impl CPU
{
	pub fn apply_segment_override(&self, seg: u16) -> u16
	{
		// TODO: WARNING: if BP was used, then seg should be overriden with BP (see page 28 of user manual)
		// TODO2: no override if we are actually using ES (destination of string operations)
		// TODO3: check that segment override wins over SS for BP & SS
		match self.segment_override_prefix
		{
			None => seg,
			Some(sr) => self.get_reg(sr)
		}
	}

	// TODO/ read/write or load/store, but avoid both
	pub fn store_memory_u16(&self, mem: &mut Memory, seg: u16, addr: u16, val: u16)
	{
		mem.write_u16((self.apply_segment_override(seg) as u32) * 0x10 + addr as u32, val)
	}

	pub fn load_memory_u16(&self, mem: &Memory, seg: u16, addr: u16) -> u16
	{
		mem.read_u16((self.apply_segment_override(seg) as u32) * 0x10 + addr as u32)
	}

	pub fn store_memory_u8(&self, mem: &mut Memory, seg: u16, addr: u16, val: u8)
	{
		mem.write_u8((self.apply_segment_override(seg) as u32) * 0x10 + addr as u32, val)
	}

	pub fn load_memory_u8(&self, mem: &Memory, seg: u16, addr: u16) -> u8
	{
		mem.read_u8((self.apply_segment_override(seg) as u32) * 0x10 + addr as u32)
	}

	/* No override variants (for DI with string instructions with invariably use ES) */
	pub fn store_memory_u16_noov(&self, mem: &mut Memory, seg: u16, addr: u16, val: u16)
	{
		mem.write_u16((seg as u32) * 0x10 + addr as u32, val)
	}

	pub fn load_memory_u16_noov(&self, mem: &Memory, seg: u16, addr: u16) -> u16
	{
		mem.read_u16((seg as u32) * 0x10 + addr as u32)
	}

	pub fn store_memory_u8_noov(&self, mem: &mut Memory, seg: u16, addr: u16, val: u8)
	{
		mem.write_u8((seg as u32) * 0x10 + addr as u32, val)
	}

	pub fn load_memory_u8_noov(&self, mem: &Memory, seg: u16, addr: u16) -> u8
	{
		mem.read_u8((seg as u32) * 0x10 + addr as u32)
	}
}