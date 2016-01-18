extern crate time;

use std::fs::File;
use std::io::Write;

use cpu::CPU;
use cpu::phys_addr;
use cpu::parse_instruction;
use bios::BIOS;
use hw::HW;
use cpu::CPUState;
use bios::BIOSState;
use bios::BootDrive;
use mem::Memory;
use cpu::instruction::*;
use cpu::reg_access::*;

pub struct Machine
{
	cpu: CPU,
	bios: BIOS,
	memory: Memory,
	hw: HW,

	clock: u32,
	last_time_ns: u64, // Only updated every 1k cycles
	last_mcycle_ns: u64
}

impl Machine
{
	pub fn new(boot_drive: BootDrive, floppy_filename: Option<String>, hdd_filename: Option<String>) -> Machine
	{
		Machine
		{
			cpu: CPU::new(0xf000, 0xfff0),
			bios: BIOS::new(boot_drive),
			memory: Memory::new(1024 * 1024),
			hw: HW::new(floppy_filename, hdd_filename),
			clock: 0,
			last_time_ns: time::precise_time_ns(),
			last_mcycle_ns: time::precise_time_ns()
		}
	}

	pub fn get_pc(&self) -> (u16, u16)
	{
		(self.cpu.get_reg(SegReg::CS), self.cpu.get_reg(WReg::IP))
	}

	pub fn step(&mut self)
	{
		self.clock += 1;

		if self.clock % 1000 == 0
		{
			self.last_time_ns = time::precise_time_ns();
		}

		self.cpu.step(&mut self.memory, &mut self.hw, &mut self.bios);
		self.hw.step(&mut self.cpu, &mut self.memory, self.clock, self.last_time_ns);

		if self.clock % 10000000 == 0
		{
			machine_print!("Cycles/s: {}", 1e16 as u64 / (self.last_time_ns - self.last_mcycle_ns));
			self.last_mcycle_ns = self.last_time_ns;
		}
	}

	pub fn dump(&self)
	{
		self.cpu.dump();
		let (cs, ip) = self.get_pc();
		self.disas(cs, ip, 5);
	}

	pub fn dump_trace(&self)
	{
		self.cpu.dump();
		let (cs, ip) = self.get_pc();
		self.disas(cs, ip, 1);
	}

	pub fn print_memory(&self, seg: u16, addr: u16, size: u32)
	{
		let start: u32 = (seg as u32) * 0x10 + (addr as u32);
		print!("{:05x}: ", start);

		for x in start..(start+size)
		{
			let byte = self.memory.read_u8(x);
			print!("{:02x} ", byte);
		}

		print!("\n");
	}

	pub fn dump_memory_to_file(&self, fname: &str)
	{
		let mut file = File::create(fname).unwrap();
		const MEM_SIZE: usize = 1024 * 1024;
		let mut buf = vec![0; MEM_SIZE as usize].into_boxed_slice();
		for x in 0..MEM_SIZE
		{
			buf[x] = self.memory.read_u8(x as u32);
		}
		match file.write(&buf)
		{
			Ok(written) =>
			{
				if written != MEM_SIZE
				{ panic!("Unable to dump RAM; only wrote {} bytes", written) }
			}
			Err(e) => panic!("Unable to dump RAM; error {}", e)
		}
	}

	pub fn disas(&self, seg: u16, addr_start: u16, count: u32)
	{
		let mut addr = addr_start;
		for _ins_count in 0..count
		{
			let bytecode = self.memory.slice_from(phys_addr(seg, addr));
			let instruction = parse_instruction(bytecode);
			disas_print!("{:04x}:{:04x}: {}", seg, addr, instruction);
			addr += instruction.size as u16;
		}
	}

	pub fn is_running(&self) -> bool
	{
		self.cpu.state == CPUState::Running && self.bios.state == BIOSState::Ok
	}
}