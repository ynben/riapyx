use super::base::*;
use super::instruction::*;

use std::io::Write;

pub trait RegisterAccess<RegisterType: Clone + Copy>
{
	type ValueType;

	fn get_reg(&self, r: RegisterType) -> Self::ValueType;
	fn set_reg(&mut self, r: RegisterType, val: Self::ValueType);
}

impl RegisterAccess<WReg> for CPU
{
	type ValueType = u16;
	
	fn get_reg(&self, wr: WReg) -> u16
	{
		match wr
		{
			WReg::AX => self.ax,
			WReg::BX => self.bx,
			WReg::CX => self.cx,
			WReg::DX => self.dx,
			WReg::SP => self.sp,
			WReg::BP => self.bp,
			WReg::SI => self.si,
			WReg::DI => self.di,
			WReg::IP => self.ip
		}
	}

	fn set_reg(&mut self, wr: WReg, val: u16)
	{
		match wr
		{
			WReg::AX => self.ax = val,
			WReg::BX => self.bx = val,
			WReg::CX => self.cx = val,
			WReg::DX => self.dx = val,
			WReg::SP => self.sp = val,
			WReg::BP => self.bp = val,
			WReg::SI => self.si = val,
			WReg::DI => self.di = val,
			WReg::IP => self.ip = val
		}
	}
}

impl RegisterAccess<BReg> for CPU
{
	type ValueType = u8;

	fn get_reg(&self, wr: BReg) -> u8
	{
		(match wr
		{
			BReg::AL => self.ax & 0xff,
			BReg::AH => self.ax >> 8,
			BReg::BL => self.bx & 0xff,
			BReg::BH => self.bx >> 8,
			BReg::CL => self.cx & 0xff,
			BReg::CH => self.cx >> 8,
			BReg::DL => self.dx & 0xff,
			BReg::DH => self.dx >> 8
		} as u8)
	}

	fn set_reg(&mut self, wr: BReg, val: u8)
	{
		let set_low = |orig: u16|
		{
			(orig & 0xff00) | (val as u16)
		};

		let set_high = |orig: u16|
		{
			(orig & 0x00ff) | ((val as u16) << 8)
		};

		match wr
		{
			BReg::AL => self.ax = set_low(self.ax),
			BReg::AH => self.ax = set_high(self.ax),
			BReg::BL => self.bx = set_low(self.bx),
			BReg::BH => self.bx = set_high(self.bx),
			BReg::CL => self.cx = set_low(self.cx),
			BReg::CH => self.cx = set_high(self.cx),
			BReg::DL => self.dx = set_low(self.dx),
			BReg::DH => self.dx = set_high(self.dx),
		}
	}
}

impl RegisterAccess<SegReg> for CPU
{
	type ValueType = u16;

	fn get_reg(&self, sr: SegReg) -> u16
	{
		match sr
		{
			SegReg::CS => self.cs,
			SegReg::DS => self.ds,
			SegReg::SS => self.ss,
			SegReg::ES => self.es,
		}
	}

	fn set_reg(&mut self, sr: SegReg, val: u16)
	{
		match sr
		{
			SegReg::CS => 
			{
				if cfg!(feature="cpu-trace")
				{
					if let &mut Some(ref mut log) = &mut self.log
					{
						write!(log, "{:04x} -> {:04x}: CS changed {:04x}\n", self.cs, val, self.ip).unwrap();
					}
				}

				self.cs = val;
			}
			SegReg::DS => self.ds = val,
			SegReg::SS => self.ss = val,
			SegReg::ES => self.es = val
		}
	}
}

impl CPU
{
	pub fn set_flag_value(&mut self, flag_mask: u16, set: bool)
	{
		if set
		{
			self.set_flag(flag_mask);
		}
		else
		{
			self.clear_flag(flag_mask);
		}
	}

	pub fn set_flag(&mut self, flag_mask: u16)
	{
		self.flags = self.flags | flag_mask;
	}

	pub fn clear_flag(&mut self, flag_mask: u16)
	{
		self.flags = self.flags & not(flag_mask);
	}
}