use std::ops::BitXor;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::Shl;
use std::ops::Shr;
use std::ops::Add;
use std::ops::Sub;
use std::cmp::Eq;
use std::fmt::Display;
use std::fmt::LowerHex;
use super::super::mem::Memory;
use super::instruction::*;
use super::reg_access::*;
use super::base::*;

pub trait Operand<ValueType>
{
	fn zero_flag(&self) -> bool;
	fn sign_flag(&self) -> bool;
	fn parity_flag(&self) -> bool;

	fn sar(&self, cnt: u8) -> ValueType;
	fn rol(&self, cnt: u8) -> ValueType;
	fn rcl(&self, has_carry: bool, cnt: u8) -> ValueType;
	fn ror(&self, cnt: u8) -> ValueType;
	fn rcr(&self, has_carry: bool, cnt: u8) -> ValueType;

	fn inc(&mut self);
	fn dec(&mut self);
	fn neg(&mut self);
	fn not(&mut self);

	// TODO: regroup all these flags into a single add_flags()?
	// TODO: avoid borrows and use copies?
	fn add_oca_flags(&self, a: ValueType, b: ValueType) -> (bool, bool, bool);
	fn sub_oca_flags(&self, a: ValueType, b: ValueType) -> (bool, bool, bool);
	fn dec_oa_flags(&self) -> (bool, bool);
	fn inc_oa_flags(&self) -> (bool, bool);
	fn neg_oca_flags(&self) -> (bool, bool, bool);
	/* _fo => _from_orig */
	fn shl1_oc_flags_fo(&self) -> (bool, bool);
	fn shlcl_c_flag_fo(&self, cnt: u8) -> bool;
	fn shr1_oc_flags_fo(&self) -> (bool, bool);
	fn shrcl_c_flag_fo(&self, cnt: u8) -> bool;
	fn sar1_oc_flags_fo(&self) -> (bool, bool);
	fn sarcl_c_flag_fo(&self, cnt: u8) -> bool;
	fn rolcl_c_flag_fo(&self, cnt: u8) -> bool;
	fn rclcl_c_flag_fo(&self, has_carry: bool, cnt: u8) -> bool;
	fn rorcl_c_flag_fo(&self, cnt: u8) -> bool;
	fn ror1_oc_flags_fo(&self) -> (bool, bool);
	fn rcr1_oc_flags_fo(&self, has_carry: bool) -> (bool, bool);
	fn rcrcl_c_flag_fo(&self, has_carry: bool, cnt: u8) -> bool;

	/* (Hi, Lo, OC) */
	fn mul(&self, x: ValueType) -> (ValueType, ValueType, bool);
	fn imul(&self, x: ValueType) -> (ValueType, ValueType, bool);
	/* Some<(Quot, Rem)> */
	fn div(&self, dividend_hi: ValueType, dividend_lo: ValueType) -> Option<(ValueType, ValueType)>;
	fn idiv(&self, dividend_hi: ValueType, dividend_lo: ValueType) -> Option<(ValueType, ValueType)>;
	// TODO: merge flags & computation functions?

	fn bit_count() -> u8;
	fn zero() -> ValueType;
	fn one() -> ValueType;
}

fn compute_parity(val: u8) -> bool
{
	let mut ret = val;
	ret ^= ret >> 4;
	ret ^= ret >> 2;
	ret ^= ret >> 1;
	(ret & 1) == 0
}

impl Operand<u8> for u8
{
	fn zero_flag(&self) -> bool
	{
		*self == 0
	}
	fn sign_flag(&self) -> bool
	{
		(*self & 0x80) > 0
	}
	fn parity_flag(&self) -> bool
	{
		compute_parity(*self)
	}
	
	fn add_oca_flags(&self, a: u8, b: u8) -> (bool, bool, bool)
	{
		let same_sign = ((a ^ b) & 0x80) != 0x80;
		let overflow = same_sign && ((((*self) ^ a) & 0x80) == 0x80);
	
		let carry = *self < a || *self < b;
	
		let (ms, ma, mb) = (*self & 0xf, a & 0xf, b & 0xf);
		let half_carry = ms < ma || ms < mb;

		(overflow, carry, half_carry)
	}

	fn sub_oca_flags(&self, a: u8, b: u8) -> (bool, bool, bool)
	{
		// Overflow if a and b of different sign and result of same sign as b
		let diff_sign = ((a ^ b) & 0x80) == 0x80;
		let overflow = diff_sign && ((((*self) ^ b) & 0x80) != 0x80);
	
		let borrow = *self > a;
	
		let (ms, ma) = (*self & 0xf, a & 0xf);
		let half_borrow = ms > ma;

		(overflow, borrow, half_borrow)
	}
	fn dec_oa_flags(&self) -> (bool, bool)
	{
		let overflow = *self == 0x7f;

		let half_borrow = *self & 0xf == 0xf;

		(overflow, half_borrow)
	}
	fn inc_oa_flags(&self) -> (bool, bool)
	{
		let overflow = *self == 0x80;

		let half_borrow = *self & 0xf == 0x0;

		(overflow, half_borrow)
	}
	fn neg_oca_flags(&self) -> (bool, bool, bool)
	{
		let overflow = *self == 0x80;
		let carry = *self != 0;
		let half_carry = *self & 0xf != 0;
		(overflow, carry, half_carry)
	}
	fn neg(&mut self)
	{
		*self = (-(*self as i8)) as u8;
	}
	fn not(&mut self)
	{
		*self = *self ^ 0xff;
	}
	fn inc(&mut self)
	{
		*self += 1
	}
	fn dec(&mut self)
	{
		*self -= 1
	}

	fn shl1_oc_flags_fo(&self) -> (bool, bool)
	{
		let hi_bits = *self & 0xc0;
		/* Overflow if leading bit different from second leading bit */
		(hi_bits == 0x80 || hi_bits == 0x40, *self & 0x80 != 0)
	}

	fn shlcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		match cnt
		{
			0 ... 7 => *self & (0x1 << (8 - cnt)) != 0,
			_ => false,
		}
	}

	fn shr1_oc_flags_fo(&self) -> (bool, bool)
	{
		let hi_bit = *self & 0x80;
		(hi_bit == 0x80, *self & 0x1 == 0x1)
	}

	fn shrcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		match cnt
		{
			// 0 should never happen; handled beforehand by instruction_exec
			0 ... 7 => *self & (0x1 << (cnt - 1)) != 0,
			_ => false,
		}
	}

	fn sar1_oc_flags_fo(&self) -> (bool, bool)
	{
		(false, *self & 0x1 == 0x1)
	}

	fn sarcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		match cnt
		{
			0 => false,
			1 ... 7 => *self & (0x1 << (cnt - 1)) != 0,
			_ => *self & 0x80 != 0,
		}
	}

	fn sar(&self, cnt: u8) -> u8
	{
		match cnt
		{
			0 ... 7 => ((*self as i8) >> cnt) as u8,
			_ => 0xff * ((*self & 0x80) >> 7) // TODO: is masking useful?
		}
	}

	fn rolcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		let cntm = cnt % 8;
		*self & (0x1 << (8 - cntm)) != 0
	}

	fn rol(&self, cnt: u8) -> u8
	{
		let cntm = cnt % 8;
		match cntm
		{
			0 => *self, // Needed as x>>8=x in Rust...
			_ => (*self << cntm) + (*self >> (8-cntm))
		}
	}

	fn rcl(&self, has_carry: bool, cnt: u8) -> u8
	{
		let carry: u8 = if has_carry {1} else {0};
		let cntm = cnt % 9;
		match cntm
		{
			0 => *self,
			1 => (*self << 1) | carry,
			8 => (*self >> 1) | (carry << 7),
			_ => (*self << cntm) | (*self >> (9-cntm)) | (carry << (cntm - 1))
		}
	}

	fn rclcl_c_flag_fo(&self, has_carry: bool, cnt: u8) -> bool
	{
		let cntm = cnt % 9;
		match cntm
		{
			0 => has_carry,
			8 => *self & 0x1 != 0,
			_ => *self & (0x1 << (8 - cntm)) != 0
		}
	}

	fn rorcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		let cntm = cnt % 8;
		/* if cntm==0, cntm-1 ~ 7 in this case, which is what
		 * we expect */
		*self & (0x1 << (cntm-1)) != 0
	}

	fn ror(&self, cnt: u8) -> u8
	{
		let cntm = cnt % 8;
		match cntm
		{
			0 => *self,
			_ => (*self >> cntm) + (*self << (8-cntm))
		}
	}

	fn rcr(&self, has_carry: bool, cnt: u8) -> u8
	{
		let carry: u8 = if has_carry {1} else {0};
		let cntm = cnt % 9;
		match cntm
		{
			0 => *self,
			1 => (*self >> 1) | (carry << 7),
			8 => (*self << 1) | carry,
			_ => (*self >> cntm) | (*self << (9-cntm)) | (carry << (8 - cntm))
		}
	}

	fn rcrcl_c_flag_fo(&self, has_carry: bool, cnt: u8) -> bool
	{
		let cntm = cnt % 9;
		match cntm
		{
			0 => has_carry,
			8 => *self & 0x80 != 0,
			_ => *self & (0x1 << (cntm-1)) != 0
		}
	}

	fn rcr1_oc_flags_fo(&self, has_carry: bool) -> (bool, bool)
	{
		(
			(has_carry && (*self & 0x80) == 0) ||
			((!has_carry) && (*self & 0x80) != 0)
		, *self & 1 != 0)
	}

	fn ror1_oc_flags_fo(&self) -> (bool, bool)
	{
		let hibits = *self & 0x81;
		(hibits == 0x80 || hibits == 0x01, *self & 1 != 0)
	}

	fn mul(&self, x: u8) -> (u8, u8, bool)
	{
		let result = (x as u16) * (*self as u16);
		let lo = result & 0xff;
		let hi = result >> 8;
		(hi as u8, lo as u8, hi != 0)
	}

	fn imul(&self, x: u8) -> (u8, u8, bool)
	{
		let result = (((x as i8) as i16) * ((*self as i8) as i16)) as u16;
		let lo = (result & 0xff) as u8;
		let hi = (result >> 8) as u8;
		/* OC set if result cannot be truncated to AX
		 * Three cases: DX == 0 and AX negative, or DX == -1 and AX positive, or DX = any other value */
		let overflow = (hi != 0 || lo & 0x80 == 0x80) && (hi != 0xff || lo & 0x80 == 0x0);
		(hi as u8, lo as u8, overflow)
	}
	fn div(&self, dividend_hi: u8, dividend_lo: u8) -> Option<(u8, u8)>
	{
		let dividend = ((dividend_hi as u16) << 8) + (dividend_lo as u16);
		let divisor = *self as u16;

		if divisor == 0
		{
			return None;
		}

		let quotient = dividend / divisor;
		let remainder = dividend % divisor;
		if quotient >= 1<<8
		{
			return None;
		}
		Some((quotient as u8, remainder as u8))
	}
	fn idiv(&self, dividend_hi: u8, dividend_lo: u8) -> Option<(u8, u8)>
	{
		let dividend = (((dividend_hi as u16) << 8) + (dividend_lo as u16)) as i16;
		let divisor = (*self as i8) as i16;

		if divisor == 0
		{
			return None;
		}

		let quotient = dividend / divisor;
		let remainder = dividend % divisor;
		if quotient > 0x7f || quotient < -0x7f
		{
			return None;
		}
		Some(((quotient as u16) as u8, (remainder as u16) as u8))
	}

	fn bit_count() -> u8
	{
		8
	}

	fn zero() -> u8
	{
		0
	}

	fn one() -> u8
	{
		1
	}
}

impl Operand<u16> for u16
{
	fn zero_flag(&self) -> bool
	{
		*self == 0
	}
	fn sign_flag(&self) -> bool
	{
		(*self & 0b1000000000000000) > 0
	}
	fn parity_flag(&self) -> bool
	{
		compute_parity(*self as u8)
	}

	fn add_oca_flags(&self, a: u16, b: u16) -> (bool, bool, bool)
	{
		let same_sign = ((a ^ b) & 0x8000) != 0x8000;
		let overflow = same_sign && ((((*self) ^ a) & 0x8000) == 0x8000);
	
		let carry = *self < a || *self < b;
	
		let (ms, ma, mb) = (*self & 0x000f, a & 0x000f, b & 0x000f);
		let half_carry = ms < ma || ms < mb;

		(overflow, carry, half_carry)
	}

	fn sub_oca_flags(&self, a: u16, b: u16) -> (bool, bool, bool)
	{
		// Overflow if a and b of different sign and result of same sign as b
		let diff_sign = ((a ^ b) & 0x8000) == 0x8000;
		let overflow = diff_sign && ((((*self) ^ b) & 0x8000) != 0x8000);
	
		let borrow = *self > a;
	
		let (ms, ma) = (*self & 0x000f, a & 0x000f);
		let half_borrow = ms > ma;

		(overflow, borrow, half_borrow)
	}
	fn dec_oa_flags(&self) -> (bool, bool)
	{
		let overflow = *self == 0x7fff;

		let half_borrow = *self & 0x000f == 0xf;

		(overflow, half_borrow)
	}
	fn inc_oa_flags(&self) -> (bool, bool)
	{
		let overflow = *self == 0x8000;

		let half_borrow = *self & 0x000f == 0x0;

		(overflow, half_borrow)
	}
	fn neg_oca_flags(&self) -> (bool, bool, bool)
	{
		let overflow = *self == 0x8000;
		let carry = *self != 0;
		let half_carry = *self & 0x000f != 0;
		(overflow, carry, half_carry)
	}
	fn neg(&mut self)
	{
		*self = (-(*self as i16)) as u16;
	}
	fn not(&mut self)
	{
		*self = *self ^ 0xffff;
	}
	fn inc(&mut self)
	{
		*self += 1
	}
	fn dec(&mut self)
	{
		*self -= 1
	}

	fn shl1_oc_flags_fo(&self) -> (bool, bool)
	{
		let hi_bits = *self & 0xc000;
		/* Overflow if leading bit different from second leading bit */
		(hi_bits == 0x8000 || hi_bits == 0x4000, *self & 0x8000 != 0)
	}

	fn shlcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		match cnt
		{
			0 ... 15 => *self & (0x1 << (16 - cnt)) != 0,
			_ => false,
		}
	}

	fn shr1_oc_flags_fo(&self) -> (bool, bool)
	{
		let hi_bit = *self & 0x8000;
		(hi_bit == 0x8000, *self & 0x1 == 0x1)
	}

	fn shrcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		match cnt
		{
			// 0 should never happen; handled beforehand by instruction_exec
			0 ... 15 => *self & (0x1 << (cnt - 1)) != 0,
			_ => false,
		}
	}

	fn sar1_oc_flags_fo(&self) -> (bool, bool)
	{
		(false, *self & 0x1 == 0x1)
	}

	fn sarcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		match cnt
		{
			0 => false,
			1 ... 15 => *self & (0x1 << (cnt - 1)) != 0,
			_ => *self & 0x8000 != 0,
		}
	}

	fn sar(&self, cnt: u8) -> u16
	{
		match cnt
		{
			0 ... 15 => ((*self as i16) >> cnt) as u16,
			_ => 0xffff * ((*self & 0x8000) >> 15)
		}
	}

	fn rolcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		let cntm = cnt % 16;
		*self & (0x1 << (16 - cntm)) != 0
	}

	fn rol(&self, cnt: u8) -> u16
	{
		let cntm = cnt % 16;
		match cntm
		{
			0 => *self, // Needed as x>>16=x in Rust...
			_ => (*self << cntm) + (*self >> (16-cntm))
		}
	}

	fn rcl(&self, has_carry: bool, cnt: u8) -> u16
	{
		let carry: u16 = if has_carry {1} else {0};
		let cntm = cnt % 17;
		match cntm
		{
			0 => *self,
			1 => (*self << 1) | carry,
			16 => (*self >> 1) | (carry << 15),
			_ => (*self << cntm) | (*self >> (17-cntm)) | (carry << (cntm - 1))
		}
	}

	fn rclcl_c_flag_fo(&self, has_carry: bool, cnt: u8) -> bool
	{
		let cntm = cnt % 17;
		match cntm
		{
			0 => has_carry,
			16 => *self & 0x1 != 0,
			_ => *self & (0x1 << (16 - cntm)) != 0
		}
	}

	fn rorcl_c_flag_fo(&self, cnt: u8) -> bool
	{
		let cntm = cnt % 16;
		/* if cntm==0, cntm-1 ~ 15 in this case, which is what
		 * we expect */
		*self & (0x1 << (cntm-1)) != 0
	}

	fn ror(&self, cnt: u8) -> u16
	{
		let cntm = cnt % 16;
		match cntm
		{
			0 => *self,
			_ => (*self >> cntm) + (*self << (16-cntm))
		}
	}

	fn rcr(&self, has_carry: bool, cnt: u8) -> u16
	{
		let carry: u16 = if has_carry {1} else {0};
		let cntm = cnt % 17;
		match cntm
		{
			0 => *self,
			1 => (*self >> 1) | (carry << 15),
			16 => (*self << 1) | carry,
			_ => (*self >> cntm) | (*self << (17-cntm)) | (carry << (16 - cntm))
		}
	}

	fn rcrcl_c_flag_fo(&self, has_carry: bool, cnt: u8) -> bool
	{
		let cntm = cnt % 17;
		match cntm
		{
			0 => has_carry,
			16 => *self & 0x8000 != 0,
			_ => *self & (0x1 << (cntm-1)) != 0
		}
	}

	fn rcr1_oc_flags_fo(&self, has_carry: bool) -> (bool, bool)
	{
		(
			(has_carry && (*self & 0x8000) == 0) ||
			((!has_carry) && (*self & 0x8000) != 0)
		, *self & 1 != 0)
	}

	fn ror1_oc_flags_fo(&self) -> (bool, bool)
	{
		let hibits = *self & 0x8001;
		(hibits == 0x8000 || hibits == 0x0001, *self & 1 != 0)
	}

	fn mul(&self, x: u16) -> (u16, u16, bool)
	{
		let result = (x as u32) * (*self as u32);
		let lo = result & 0xffff;
		let hi = result >> 16;
		(hi as u16, lo as u16, hi != 0)
	}

	fn imul(&self, x: u16) -> (u16, u16, bool)
	{
		let result = (((x as i16) as i32) * ((*self as i16) as i32)) as u32;
		let lo = (result & 0xffff) as u16;
		let hi = (result >> 16) as u16;
		/* OC set if result cannot be truncated to AX
		 * Three cases: DX == 0 and AX negative, or DX == -1 and AX positive, or DX = any other value */
		let overflow = (hi != 0 || lo & 0x8000 == 0x8000) && (hi != 0xffff || lo & 0x8000 == 0x0);
		(hi as u16, lo as u16, overflow)
	}
	fn div(&self, dividend_hi: u16, dividend_lo: u16) -> Option<(u16, u16)>
	{
		let dividend = ((dividend_hi as u32) << 16) + (dividend_lo as u32);
		let divisor = *self as u32;

		if divisor == 0
		{
			return None;
		}

		let quotient = dividend / divisor;
		let remainder = dividend % divisor;
		if quotient >= 1<<16
		{
			return None;
		}
		Some((quotient as u16, remainder as u16))
	}
	fn idiv(&self, dividend_hi: u16, dividend_lo: u16) -> Option<(u16, u16)>
	{
		let dividend = (((dividend_hi as u32) << 16) + (dividend_lo as u32)) as i32;
		let divisor = (*self as i16) as i32;

		if divisor == 0
		{
			return None;
		}

		let quotient = dividend / divisor;
		let remainder = dividend % divisor;
		if quotient > 0x7fff || quotient < -0x7fff
		{
			return None;
		}
		Some(((quotient as u32) as u16, (remainder as u32) as u16))
	}

	fn bit_count() -> u8
	{
		16
	}

	fn zero() -> u16
	{
		0
	}

	fn one() -> u16
	{
		1
	}
}

pub trait OperandAccess<OperandType>
{
	type ValueType: Operand<Self::ValueType> +
		BitXor<Self::ValueType, Output=Self::ValueType> +
		BitOr<Self::ValueType, Output=Self::ValueType> +
		BitAnd<Self::ValueType, Output=Self::ValueType> +
		Add<Self::ValueType, Output=Self::ValueType> +
		Sub<Self::ValueType, Output=Self::ValueType> +
		Shl<u8, Output=Self::ValueType> +
		Shr<u8, Output=Self::ValueType> +
		Eq +
		Copy +
		Display + 
		LowerHex;

	fn load_operand(&self, &Memory, &OperandType) -> Self::ValueType;
	fn store_operand(&mut self, &mut Memory, &OperandType, Self::ValueType);

	/* TODO: factor among OperandAccess of same operand size */
	fn get_acc(&mut self) -> Self::ValueType;
	fn set_acc(&mut self, Self::ValueType);
	fn get_aux(&mut self) -> Self::ValueType;
	fn set_aux(&mut self, Self::ValueType);
}

impl OperandAccess<BOperand> for CPU
{
	type ValueType = u8;

	fn load_operand(&self, mem: &Memory, src: &BOperand) -> u8
	{
		match *src
		{
			BOperand::Reg(wr) => self.get_reg(wr),
			BOperand::Immediate(ref imm) => *imm,
			BOperand::Indirect(ref ir) => self.get_ireg_u8_value(mem, ir),
			BOperand::Indirect8iDis(ref ir, dis) => self.get_ireg_u8d_u8_value(mem, ir, dis),
			BOperand::Indirect16uDis(ref ir, dis) => self.get_ireg_u16d_u8_value(mem, ir, dis),
			BOperand::Direct(addr) => self.load_memory_u8(mem, self.ds, addr),
		}
	}

	fn store_operand(&mut self, mem: &mut Memory, dst: &BOperand, val: u8)
	{
		match *dst
		{
			BOperand::Reg(br) => self.set_reg(br, val),
			BOperand::Indirect(ref ir) => self.set_ireg_u8_value(mem, ir, val),
			BOperand::Indirect8iDis(ref ir, dis) => self.set_ireg_u8d_u8_value(mem, ir, dis, val),
			BOperand::Indirect16uDis(ref ir, dis) => self.set_ireg_u16d_u8_value(mem, ir, dis, val),
			BOperand::Direct(addr) => self.store_memory_u8(mem, self.ds, addr, val),
			_ => panic!("Unhandled byte operand write: {:?}", dst)
		}
	}

	fn get_acc(&mut self) -> u8 { self.get_reg(BReg::AL) }
	fn set_acc(&mut self, val: u8) { self.set_reg(BReg::AL, val) }
	fn get_aux(&mut self) -> u8 { self.get_reg(BReg::AH) }
	fn set_aux(&mut self, val: u8) { self.set_reg(BReg::AH, val) }
}

impl OperandAccess<WOperand> for CPU
{
	type ValueType = u16;

	fn load_operand(&self, mem: &Memory, src: &WOperand) -> u16
	{
		match *src
		{
			WOperand::Reg(wr) => self.get_reg(wr),
			WOperand::SegReg(sr) => self.get_reg(sr),
			WOperand::Immediate(ref imm) => *imm,
			WOperand::Direct(addr) => self.load_memory_u16(mem, self.ds, addr),
			WOperand::Indirect(ref ir) => self.get_ireg_u16_value(mem, ir),
			WOperand::Indirect8iDis(ref ir, dis) => self.get_ireg_u8d_u16_value(mem, ir, dis),
			WOperand::Indirect16uDis(ref ir, dis) => self.get_ireg_u16d_u16_value(mem, ir, dis),
		}
	}

	fn store_operand(&mut self, mem: &mut Memory, dst: &WOperand, val: u16)
	{
		match *dst
		{
			WOperand::Reg(wr) => self.set_reg(wr, val),
			WOperand::SegReg(sr) => self.set_reg(sr, val),
			WOperand::Direct(addr) => self.store_memory_u16(mem, self.ds, addr, val),
			WOperand::Indirect(ref ir) => self.set_ireg_u16_value(mem, ir, val),
			WOperand::Indirect8iDis(ref ir, dis) => self.set_ireg_u8d_u16_value(mem, ir, dis, val),
			WOperand::Indirect16uDis(ref ir, dis) => self.set_ireg_u16d_u16_value(mem, ir, dis, val),
			_ => panic!("Unhandled wide operand write: {:?}", dst)
		}
	}

	fn get_acc(&mut self) -> u16 { self.get_reg(WReg::AX) }
	fn set_acc(&mut self, val: u16) { self.set_reg(WReg::AX, val) }
	fn get_aux(&mut self) -> u16 { self.get_reg(WReg::DX) }
	fn set_aux(&mut self, val: u16) { self.set_reg(WReg::DX, val) }
}

impl OperandAccess<ImplicitBOperand> for CPU
{
	type ValueType = u8;

	fn load_operand(&self, mem: &Memory, src: &ImplicitBOperand) -> u8
	{
		match *src
		{
			ImplicitBOperand::DSSI => self.load_memory_u8(mem, self.ds, self.si),
			ImplicitBOperand::ESDI => self.load_memory_u8_noov(mem, self.es, self.di),
		}
	}

	fn store_operand(&mut self, mem: &mut Memory, dst: &ImplicitBOperand, val: u8)
	{
		match *dst
		{
			ImplicitBOperand::DSSI => self.store_memory_u8(mem, self.ds, self.si, val),
			ImplicitBOperand::ESDI => self.store_memory_u8_noov(mem, self.es, self.di, val),
		}
	}

	fn get_acc(&mut self) -> u8 { self.get_reg(BReg::AL) }
	fn set_acc(&mut self, val: u8) { self.set_reg(BReg::AL, val) }
	fn get_aux(&mut self) -> u8 { self.get_reg(BReg::AH) }
	fn set_aux(&mut self, val: u8) { self.set_reg(BReg::AH, val) }
}

impl OperandAccess<ImplicitWOperand> for CPU
{
	type ValueType = u16;

	fn load_operand(&self, mem: &Memory, src: &ImplicitWOperand) -> u16
	{
		match *src
		{
			ImplicitWOperand::DSSI => self.load_memory_u16(mem, self.ds, self.si),
			ImplicitWOperand::ESDI => self.load_memory_u16_noov(mem, self.es, self.di),
		}	}

	fn store_operand(&mut self, mem: &mut Memory, dst: &ImplicitWOperand, val: u16)
	{
		match *dst
		{
			ImplicitWOperand::DSSI => self.store_memory_u16(mem, self.ds, self.si, val),
			ImplicitWOperand::ESDI => self.store_memory_u16_noov(mem, self.es, self.di, val),
		}
	}

	fn get_acc(&mut self) -> u16 { self.get_reg(WReg::AX) }
	fn set_acc(&mut self, val: u16) { self.set_reg(WReg::AX, val) }
	fn get_aux(&mut self) -> u16 { self.get_reg(WReg::DX) }
	fn set_aux(&mut self, val: u16) { self.set_reg(WReg::DX, val) }
}

impl CPU
{
	pub fn load_woperand_32(&self, mem: &Memory, src: &WOperand) -> (u16, u16)
	{
		let (seg, addr) =
			match *src
			{
				WOperand::Indirect(ref reg) => self.get_ireg_addr(&reg),
				WOperand::Direct(ref addr) => (self.apply_segment_override(self.ds), *addr),
				WOperand::Indirect8iDis(ref reg, dis) =>
				{
					let (seg, addr) = self.get_ireg_addr(&reg);
					(seg, addr + ((dis as i16) as u16))
				}
				WOperand::Indirect16uDis(ref reg, dis) =>
				{
					let (seg, addr) = self.get_ireg_addr(&reg);
					(seg, addr + dis)	
				}
				WOperand::Immediate(_) => 
					panic!("Attempting to load 32-bit value from an immediate 16-bit operand (cs:ip={:04x}:{:04x})", self.cs, self.ip),
				_ => panic!("Unsupported 32-bit load from {:?}", src)
			};

		(
			self.load_memory_u16(mem, seg, addr), 
			self.load_memory_u16(mem, seg, addr + 2)
		)
	}

	/* Return value: (isInterSeg, seg, addr) */
	pub fn load_fcoperand(&self, mem: &Memory, to: &FlowControlOperand) -> (bool, u16, u16)
	{
		match *to
		{
			FlowControlOperand::DirectSeg(inc) => (false, self.cs, self.ip + inc),
			FlowControlOperand::IndirectSeg(ref op) => (false, self.cs, self.load_operand(mem, op)),
			FlowControlOperand::DirectInterSeg(cs, ip) => (true, cs, ip),
			FlowControlOperand::IndirectInterSeg(ref op) =>
			{
				let (addr, seg) = self.load_woperand_32(mem, op);
				(true, seg, addr)
			}
		}
	}
}
