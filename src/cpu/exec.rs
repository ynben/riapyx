use super::base::*;
use super::instruction::*;
use super::super::mem::Memory;
use super::super::bios::BIOS;
use super::super::hw::HW;
use super::parser::*;

impl CPU
{
	/* Finds the next non-prefix instruction; prefixes are handled during
	 * the search */
	fn next_non_prefix_instruction(&mut self, mem: &Memory, ip: u16) -> SizedInstruction
	{
		let bytecode = mem.slice_from(phys_addr(self.cs, ip));
		let instruction_candidate = parse_instruction(bytecode);
		match instruction_candidate.instruction
		{
			Instruction::Prefix(p) =>
			{
				self.handle_prefix(p);
				let ins = self.next_non_prefix_instruction(mem, ip + instruction_candidate.size);
				/* Includes the prefix in the instruction size */
				SizedInstruction
				{
					size: instruction_candidate.size + ins.size,
					instruction: ins.instruction
				}
			}
			_ =>  instruction_candidate
		}
	}

	pub fn step(&mut self, mem: &mut Memory, hw: &mut HW, bios: &mut BIOS)
	{
		if (self.flags & FLAG_I != 0) && (! self.pending_interrupts.is_empty())
		{
			let irq = self.pending_interrupts.pop_front().unwrap();
			cpu_print!("HW IRQ {:02x}", irq);
			self.request_interrupt(mem, irq);
		}

		if self.cs == CS_BIOS_TRAP
		{
			bios.cpu_trap(self, mem, hw);
			/* IP should point to an 'IRET' instruction or to a far call, 
			 * which will run after that. */
		}

		let cur_ip = self.ip;
		let instruction = self.next_non_prefix_instruction(mem, cur_ip);

		let mut delayed_ip_update = false;
		if self.rep_prefix == None
		{
			// The current instruction expects IP to be incremented at this point
			self.ip += instruction.size as u16; // TODO: size should be u16?
		}
		else 
		{
			delayed_ip_update = true;
		}
		
		match instruction.instruction
		{
			Instruction::NoOperand(op) => self.run_noop_ins(mem, op),
			Instruction::ImplicitBOperand(op) => self.run_imgop_ins(mem, op, &ImplicitBOperand::DSSI, &ImplicitBOperand::ESDI),
			Instruction::ImplicitWOperand(op) => self.run_imgop_ins(mem, op, &ImplicitWOperand::DSSI, &ImplicitWOperand::ESDI),
			Instruction::TwoWOperands(op, a, b) => self.run_twop_ins(mem, op, a, b),
			Instruction::TwoBOperands(op, a, b) => self.run_tbop_ins(mem, op, a, b),
			Instruction::SingleBOperand(op, a) => self.run_sbop_ins(mem, op, a),
			Instruction::SingleWOperand(op, a) => self.run_swop_ins(mem, op, a),
			Instruction::SingleBImmOperand(op, a) => self.run_sbiop_ins(mem, hw, op, a),
			Instruction::SingleFCOperand(op, a) => self.run_sfcop_ins(mem, op, a),
			Instruction::FCNoOperandSeg(op) => self.run_fcnoop_seg_ins(mem, op),
			Instruction::FCNoOperandInterSeg(op) => self.run_fcnoop_iseg_ins(mem, op),
			Instruction::ShiftRotateB(op, cnt, a) => self.run_srgop_ins(mem, op, cnt, a),
			Instruction::ShiftRotateW(op, cnt, a) => self.run_srgop_ins(mem, op, cnt, a),
			Instruction::SingleWImmFCOperand(op, a) => self.run_swfcop_ins(mem, op, a),
			Instruction::Prefix(_) => unreachable!(),
			Instruction::Invalid =>
			{
				cpu_print!("Invalid instruction");
				self.state = CPUState::Crashed
			}
		};

		if delayed_ip_update && self.rep_prefix == None
		{
			/* The current instruction got out of the rep prefix; update IP */
			self.ip += instruction.size as u16;
		}

		self.segment_override_prefix = None;
		self.rep_prefix = None;
	}
}