use super::base::*;
use super::instruction::*;
use super::super::mem::Memory;

impl CPU
{
	pub fn get_ireg_addr(&self, ir: &IndReg) -> (u16, u16) // Seg, Addr
	{
		match *ir
		{
			IndReg::BX => (self.ds, self.bx),
			IndReg::BP => (self.ss, self.bp),
			IndReg::SI => (self.ds, self.si),
			IndReg::DI => (self.ds, self.di),
			IndReg::BXSI => (self.ds, self.bx + self.si),
			IndReg::BXDI => (self.ds, self.bx + self.di),
			IndReg::BPSI => (self.ss, self.bp + self.si),
			IndReg::BPDI => (self.ss, self.bp + self.di)
		}
	}

	pub fn get_ireg_u16_value(&self, mem: &Memory, ir: &IndReg) -> u16
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.load_memory_u16(mem, seg, addr)
	}

	// TODO: i8d?
	pub fn get_ireg_u8d_u16_value(&self, mem: &Memory, ir: &IndReg, dis: i8) -> u16
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.load_memory_u16(mem, seg, addr + (dis as i16) as u16)
	}

	pub fn get_ireg_u16d_u16_value(&self, mem: &Memory, ir: &IndReg, dis: u16) -> u16
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.load_memory_u16(mem, seg, addr + dis)
	}

	pub fn get_ireg_u8_value(&self, mem: &Memory, ir: &IndReg) -> u8
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.load_memory_u8(mem, seg, addr)
	}

	pub fn get_ireg_u8d_u8_value(&self, mem: &Memory, ir: &IndReg, dis: i8) -> u8
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.load_memory_u8(mem, seg, addr + (dis as i16) as u16)
	}

	pub fn get_ireg_u16d_u8_value(&self, mem: &Memory, ir: &IndReg, dis: u16) -> u8
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.load_memory_u8(mem, seg, addr + dis)
	}

	pub fn set_ireg_u8_value(&self, mem: &mut Memory, ir: &IndReg, val: u8)
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.store_memory_u8(mem, seg, addr, val)
	}

	pub fn set_ireg_u8d_u8_value(&self, mem: &mut Memory, ir: &IndReg, dis: i8, val: u8)
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.store_memory_u8(mem, seg, addr + (dis as i16) as u16, val)
	}

	pub fn set_ireg_u16d_u8_value(&self, mem: &mut Memory, ir: &IndReg, dis: u16, val: u8)
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.store_memory_u8(mem, seg, addr + dis, val)
	}

	pub fn set_ireg_u16_value(&self, mem: &mut Memory, ir: &IndReg, val: u16)
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.store_memory_u16(mem, seg, addr, val)
	}

	pub fn set_ireg_u8d_u16_value(&self, mem: &mut Memory, ir: &IndReg, dis: i8, val: u16)
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.store_memory_u16(mem, seg, addr + (dis as i16) as u16, val)
	}

	pub fn set_ireg_u16d_u16_value(&self, mem: &mut Memory, ir: &IndReg, dis: u16, val: u16)
	{
		let (seg, addr) = self.get_ireg_addr(ir);
		self.store_memory_u16(mem, seg, addr + dis, val)
	}
}