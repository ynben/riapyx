use super::instruction::*;

use std::fs::File;
use std::collections::VecDeque;

pub const FLAG_C: u16 = 0b0000000000000001;
pub const FLAG_P: u16 = 0b0000000000000100;
pub const FLAG_A: u16 = 0b0000000000010000;
pub const FLAG_Z: u16 = 0b0000000001000000;
pub const FLAG_S: u16 = 0b0000000010000000;
pub const FLAG_T: u16 = 0b0000000100000000;
pub const FLAG_I: u16 = 0b0000001000000000;
pub const FLAG_D: u16 = 0b0000010000000000;
pub const FLAG_O: u16 = 0b0000100000000000;
pub const FLAG_SET_8086: u16 = 0b1111000000000000;
pub const CS_BIOS_TRAP: u16 = 0xf000;

pub enum ImplicitBOperand
{
	DSSI,
	ESDI
}
pub enum ImplicitWOperand
{
	DSSI,
	ESDI
}

/* TODO: move to helpers */
pub fn not(x: u16) -> u16
{
	x ^ 0xffff
}

pub fn phys_addr(seg: u16, addr: u16) -> u32
{
	((seg as u32) * 0x10 + (addr as u32)) & 0xfffff
}


#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum RepPrefix
{
	Rep,
	Repne
}

#[derive(Eq, PartialEq)]
pub enum CPUState
{
	Running,
	Crashed
}

pub struct CPU
{
	pub ax: u16,
	pub bx: u16,
	pub cx: u16,
	pub dx: u16,
	pub sp: u16,
	pub bp: u16,
	pub si: u16,
	pub di: u16,
	pub cs: u16,
	pub ds: u16,
	pub ss: u16,
	pub es: u16,
	pub ip: u16,
	pub flags: u16,

	pub segment_override_prefix: Option<SegReg>,
	pub rep_prefix: Option<RepPrefix>,
	pub state: CPUState,

	pub pending_interrupts: VecDeque<u8>,

	pub log: Option<File>
}

impl CPU
{
	pub fn new(cs: u16, ip: u16) -> CPU
	{
		let trace_file = 
			if cfg!(feature="cpu-trace")
			{ Some(File::create("log.txt").unwrap()) }
			else 
			{ None };
			
		CPU
		{
			ax: 0xbad0,
			bx: 0xbad0,
			cx: 0xbad0,
			dx: 0xbad0,
			bp: 0xbad0,
			sp: 0xbad0,
			si: 0xbad0,
			di: 0xbad0,
			ip: ip,
			cs: cs,
			ds: 0xbad0,
			ss: 0xbad0,
			es: 0xbad0,
			flags: FLAG_SET_8086,
			segment_override_prefix: None,
			rep_prefix: None,
			state: CPUState::Running,
			pending_interrupts: VecDeque::<u8>::new(),
			log: trace_file
		}
	}
}