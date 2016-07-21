use super::instruction::*;
use super::base::*;
use super::reg_access::*;
use super::operand_access::*;
use super::super::mem::Memory;
use super::super::hw::HW;

use std::io::Write;

impl CPU
{
	fn stack_push(&mut self, mem: &mut Memory, val: u16)
	{
		self.sp -= 2;
		self.store_memory_u16_noov(mem, self.ss, self.sp, val);
	}

	fn stack_pop(&mut self, mem: &Memory) -> u16
	{
		let value = self.load_memory_u16_noov(mem, self.ss, self.sp);
		self.sp += 2;
		value
	}

	fn return_from_interrupt(&mut self, mem: &Memory)
	{
		let ip = self.stack_pop(mem);
		let cs = self.stack_pop(mem);
		
		if cfg!(feature="cpu-trace")
		{
			if let &mut Some(ref mut log) = &mut self.log
			{
				write!(log, "{:04x} --> {:04x}: {:04x} IRET {:04x}\n", self.cs, cs, self.ip, ip).unwrap();
			}
		}

		self.ip = ip;
		self.cs = cs;
		self.flags = self.stack_pop(mem);
	}

	// returns true if the CPU should run the next instruction; TODO: use a CPU attribute for this?
	pub fn run_noop_ins(&mut self, mem: &mut Memory, op: NoOperandOpCode)
	{
		match op
		{
			NoOperandOpCode::CLI => self.flags = self.flags & not(FLAG_I),
			NoOperandOpCode::CLD => self.flags = self.flags & not(FLAG_D),
			NoOperandOpCode::STI => self.flags = self.flags | FLAG_I,
			NoOperandOpCode::STD => self.flags = self.flags | FLAG_D,
			NoOperandOpCode::STC => self.set_flag(FLAG_C),
			NoOperandOpCode::CLC => self.clear_flag(FLAG_C),
			NoOperandOpCode::CMC => 
			{
				let flags = self.flags;
				self.set_flag_value(FLAG_C, flags & FLAG_C == 0)
			}
			NoOperandOpCode::CBW =>
			{
				let sign = self.get_reg(BReg::AL) & 0x80;
				let sign_extended = if sign != 0 {0xff} else {0};
				self.set_reg(BReg::AH, sign_extended);
			}
			NoOperandOpCode::CWD =>
			{
				let sign = self.get_reg(WReg::AX) & 0x8000;
				let sign_extended = if sign != 0 {0xffff} else {0};
				self.set_reg(WReg::DX, sign_extended);
			}
			NoOperandOpCode::PUSHF => 
			{
				// TODO: restrict pushed flags?
				let f = self.flags;
				self.stack_push(mem, f)
			},
			NoOperandOpCode::POPF =>
			{
				// TODO: restrict popped flags?
				let f = self.stack_pop(mem);
				self.flags = f | FLAG_SET_8086;
			}
			NoOperandOpCode::IRET => self.return_from_interrupt(mem),
			NoOperandOpCode::XLAT =>
			{
				let old_al = self.get_reg(BReg::AL);
				let new_al = self.load_memory_u8(mem, self.ds, self.bx + (old_al as u16));
				self.set_reg(BReg::AL, new_al);
			}
			NoOperandOpCode::LAHF =>
			{
				let flags = self.flags as u8; // Drop hi bits of flags register
				self.set_reg(BReg::AH, flags);
			}
			NoOperandOpCode::SAHF =>
			{
				let new_flags_lo = self.get_reg(BReg::AH) as u16;
				self.flags = (self.flags & 0xff00) | new_flags_lo;
			}
			NoOperandOpCode::DAA =>
			{
				let val = self.get_reg(BReg::AL);
				let mut lo = val & 0xf;
				let mut hi = val >> 4;
				let mut carry = false;
				if self.flags & FLAG_A > 0
				{
					lo -= 10;
					hi += 1;
				}
				if lo > 9
				{
					lo -= 10;
					hi += 1;
				}
				if hi > 9 || self.flags & FLAG_C > 0
				{
					hi -= 10;
					carry = true;
				}
				let result = lo | (hi<<4);
				self.set_szp_flags(&result);
				self.set_reg(BReg::AL, result);
				self.set_flag_value(FLAG_C, carry);
				self.clear_flag(FLAG_A);
			}
			NoOperandOpCode::HLT => cpu_print!("HLT"),
			//NoOperandOpCode::WAIT => cpu_print!("WAIT?"),
			_ => panic!("Unhandled no-operand opcode: {:?} ({:04x}:{:04x})", op, self.cs, self.ip)
		}
	}

	pub fn queue_hw_interrupt(&mut self, interrupt_number: u8)
	{
		self.pending_interrupts.push_back(interrupt_number);
	}

	pub fn request_interrupt(&mut self, mem: &mut Memory, interrupt_number: u8)
	{
		/*if interrupt_number == 0x21 || interrupt_number == 0x29
		{
			println!("DOS CALL; irq={:02x}, AX={:04x}", interrupt_number, self.ax);
		}*/

		let flags = self.flags;
		let ip = self.ip;
		let cs = self.cs;
		self.stack_push(mem, flags); // WARNING: user code may rely on flag register format
		self.flags = self.flags & not(FLAG_I) & not(FLAG_T);
		self.stack_push(mem, cs);
		self.stack_push(mem, ip);

		let ivt_addr = (interrupt_number as u32) * 4;
		let handler_ip = mem.read_u16(ivt_addr);
		let handler_cs = mem.read_u16(ivt_addr + 2);

		if cfg!(feature="cpu-trace")
		{
			if let &mut Some(ref mut log) = &mut self.log
			{
				write!(log, "{:04x} -> {:04x}: {:04x} int(#{:02x},AX={:04x}) {:04x}\n", self.cs, handler_cs, self.ip, interrupt_number, self.ax, handler_ip).unwrap();
			}
		}

		self.cs = handler_cs;
		self.ip = handler_ip;
	}

	pub fn run_sbiop_ins(&mut self, mem: &mut Memory, hw: &mut HW, op: SingleBImmOperandOpCode, operand: u8)
	{
		let signed = operand as i8;
		/* TODO: this kind of branching instructions actually uses signed operands
		 * should we update the Instruction structure? */

		match op
		{
			SingleBImmOperandOpCode::JMPS => self.ip += signed as u16,
			SingleBImmOperandOpCode::JB =>
				if self.flags & FLAG_C != 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNB =>
				if self.flags & FLAG_C == 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JO =>
				if self.flags & FLAG_O != 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNO =>
				if self.flags & FLAG_O == 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JP =>
				if self.flags & FLAG_P != 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNP =>
				if self.flags & FLAG_P == 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JBE =>
				if self.flags & FLAG_Z != 0 || self.flags & FLAG_C != 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNBE =>
				if self.flags & FLAG_Z == 0 && self.flags & FLAG_C == 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JLE =>
				if self.flags & FLAG_Z != 0 || ((self.flags & FLAG_S != 0) != (self.flags & FLAG_O != 0))
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNLE =>
				if self.flags & FLAG_Z == 0 && ((self.flags & FLAG_S != 0) == (self.flags & FLAG_O != 0))
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JE =>
				if self.flags & FLAG_Z != 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNE =>
				if self.flags & FLAG_Z == 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JS =>
				if self.flags & FLAG_S != 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNS =>
				if self.flags & FLAG_S == 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JL =>
				if (self.flags & FLAG_S != FLAG_S) != (self.flags & FLAG_O != FLAG_O)
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JNL =>
				if (self.flags & FLAG_S != FLAG_S) == (self.flags & FLAG_O != FLAG_O)
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::JCXZ =>
				if self.cx == 0
				{
					self.ip += signed as u16
				},
			SingleBImmOperandOpCode::LOOP =>
			{
				self.cx -= 1;
				if self.cx != 0
				{
					self.ip += signed as u16
				}
			},
			SingleBImmOperandOpCode::LOOPNZ =>
			{
				self.cx -= 1;
				if self.cx != 0 && (self.flags & FLAG_Z == 0)
				{
					self.ip += signed as u16
				}
			},
			SingleBImmOperandOpCode::LOOPZ =>
			{
				self.cx -= 1;
				if self.cx != 0 && (self.flags & FLAG_Z != 0)
				{
					self.ip += signed as u16
				}
			},
			SingleBImmOperandOpCode::INB =>
			{
				let ret = self.io_inb(operand as u16, hw);
				self.set_reg(BReg::AL, ret)
			}
			SingleBImmOperandOpCode::OUTB =>
			{
				let val = self.get_reg(BReg::AL);
				self.io_outb(operand as u16, val, hw)
			}
			SingleBImmOperandOpCode::OUTW =>
			{
				let val = self.get_reg(WReg::AX);
				self.io_outw(operand as u16, val, hw)
			}
			SingleBImmOperandOpCode::INW =>
			{
				let ret = self.io_inw(operand as u16, hw);
				self.set_reg(WReg::AX, ret)
			}
			SingleBImmOperandOpCode::OUTVB =>
			{
				let val = self.get_reg(BReg::AL);
				let dest = self.dx;
				self.io_outb(dest, val, hw)
			}
			SingleBImmOperandOpCode::OUTVW =>
			{
				let val = self.get_reg(WReg::AX);
				let dest = self.dx;
				self.io_outw(dest, val, hw)
			}
			SingleBImmOperandOpCode::INVB =>
			{
				let src = self.dx;
				let ret = self.io_inb(src, hw);
				self.set_reg(BReg::AL, ret)
			}
			SingleBImmOperandOpCode::INVW =>
			{
				let src = self.dx;
				let ret = self.io_inw(src, hw);
				self.set_reg(WReg::AX, ret)
			}
			SingleBImmOperandOpCode::INT => self.request_interrupt(mem, operand),
		}
	}

	pub fn run_twop_ins(&mut self, mem: &mut Memory, op: TwoOperandsOpCode, from: WOperand, to: WOperand)
	{
		/* TODO: use interleaved match blocks and only create variables when needed, 
		 * solving the ownership fight */
		let store = |cpu: &mut CPU, mem: &mut Memory, result: u16|
		{
			cpu.store_operand(mem, &to, result)
		};

		match op
		{
			TwoOperandsOpCode::LDS =>
			{
				let (w1, w2) = self.load_woperand_32(mem, &from);
				store(self, mem, w1); // First word is the address
				self.ds = w2; // DS receives the second word
			},
			TwoOperandsOpCode::LES =>
			{
				let (w1, w2) = self.load_woperand_32(mem, &from);
				store(self, mem, w1); // First word is the address
				self.es = w2; // DS receives the second word
			},
			TwoOperandsOpCode::LEA =>
			{
				let offset = match from
				{
					WOperand::Reg(_) | WOperand::SegReg(_) | WOperand::Immediate(_) 
						=> unreachable!(),
					WOperand::Direct(addr) => addr,
					WOperand::Indirect(ref ir) => self.get_ireg_addr(ir).1,
					WOperand::Indirect8iDis(ref ir, dis) => self.get_ireg_addr(ir).1 + ((dis as i16) as u16),
					WOperand::Indirect16uDis(ref ir, dis) => self.get_ireg_addr(ir).1 + dis,
				};

				store(self, mem, offset);
			},
			_ => self.run_tgop_ins(mem, op, &from, &to)
		}
	}

	pub fn set_szp_flags<ValueType>(&mut self, val: &ValueType)
		where ValueType: Operand<ValueType>
	{
		self.set_flag_value(FLAG_S, val.sign_flag());
		self.set_flag_value(FLAG_Z, val.zero_flag());
		self.set_flag_value(FLAG_P, val.parity_flag());
	}

	fn set_oa_flags(&mut self, oa: (bool, bool))
	{
		let (overflow, half_carry) = oa;
			
		self.set_flag_value(FLAG_O, overflow);
		self.set_flag_value(FLAG_A, half_carry);
	}

	fn set_oc_flags(&mut self, oc: (bool, bool))
	{
		let (overflow, carry) = oc;
			
		self.set_flag_value(FLAG_O, overflow);
		self.set_flag_value(FLAG_C, carry);
	}

	fn set_oca_flags(&mut self, oca: (bool, bool, bool))
	{
		let (overflow, carry, half_carry) = oca;
			
		self.set_flag_value(FLAG_O, overflow);
		self.set_flag_value(FLAG_C, carry);
		self.set_flag_value(FLAG_A, half_carry);
	}

	fn clear_oc_flags(&mut self)
	{
		self.clear_flag(FLAG_O);
		self.clear_flag(FLAG_C);
	}

	pub fn run_tgop_ins<OperandType>(&mut self, mem: &mut Memory, op: TwoOperandsOpCode, from: &OperandType, to: &OperandType)
		where CPU: OperandAccess<OperandType>
	{
		let from_value = self.load_operand(mem, &from);
		let to_value = self.load_operand(mem, &to); // TODO: load_operand
		let store = |cpu: &mut CPU, mem: &mut Memory, result: <CPU as OperandAccess<OperandType>>::ValueType|
		{
			cpu.store_operand(mem, &to, result)
		};

		match op
		{
			TwoOperandsOpCode::ADD => 
			{
				let result = from_value + to_value;
				self.set_oca_flags(result.add_oca_flags(to_value, from_value));
				self.set_szp_flags(&result);
				store(self, mem, result)
			},
			TwoOperandsOpCode::ADC => 
			{
				let has_carry = (self.flags & FLAG_C) == FLAG_C;
				let mut result = from_value + to_value;
				if has_carry
				{
					result.inc();
				}
				self.set_oca_flags(result.add_oca_flags(to_value, from_value));
				self.set_szp_flags(&result);
				store(self, mem, result)
			},
			TwoOperandsOpCode::SUB => 
			{
				let result = to_value - from_value;
				self.set_oca_flags(result.sub_oca_flags(to_value, from_value));
				self.set_szp_flags(&result);
				store(self, mem, result)
			},
			TwoOperandsOpCode::SBB => 
			{
				let has_borrow = (self.flags & FLAG_C) == FLAG_C;
				let mut result = to_value - from_value;
				if has_borrow
				{
					result.dec();
				}
				self.set_oca_flags(result.sub_oca_flags(to_value, from_value));
				self.set_szp_flags(&result);
				store(self, mem, result)
			},
			TwoOperandsOpCode::AND => 
			{
				let result = from_value & to_value;
				self.set_szp_flags(&result);
				self.clear_oc_flags();
				store(self, mem, result)
			},
			TwoOperandsOpCode::XOR =>
			{
				let result = from_value ^ to_value;
				self.set_szp_flags(&result);
				self.clear_oc_flags();
				store(self, mem, result)
			},
			TwoOperandsOpCode::OR =>
			{
				let result = from_value | to_value;
				self.set_szp_flags(&result);
				self.clear_oc_flags();
				store(self, mem, result)
			},
			TwoOperandsOpCode::MOV => store(self, mem, from_value),
			TwoOperandsOpCode::CMP => 
			{
				let result = to_value - from_value;
				self.set_oca_flags(result.sub_oca_flags(to_value, from_value));
				self.set_szp_flags(&result);
				/* TODO */
			},
			TwoOperandsOpCode::XCHG => 
			{
				self.store_operand(mem, &from, to_value);
				self.store_operand(mem, &to, from_value);
			},
			TwoOperandsOpCode::TEST => 
			{
				let val = from_value & to_value;
				self.set_szp_flags(&val);
				self.set_oc_flags((false, false));
			}
			_ => panic!("Unhandled two-wide-operands-opcode: {:?}", op)
		}
	}

	pub fn run_tbop_ins(&mut self, mem: &mut Memory, op: TwoOperandsOpCode, from: BOperand, to: BOperand)
	{
		/* TODO: use interleaved match blocks and only create variables when needed, 
		 * solving the ownership fight */

		match op
		{
			TwoOperandsOpCode::LDS => panic!("LDS cannot be used with byte operands"),
			_ => self.run_tgop_ins(mem, op, &from, &to)
		}
	}

	fn get_srcount(&self, shift_count: ShiftRotateCount) -> u8
	{
		match shift_count
		{
			ShiftRotateCount::CL => self.get_reg(BReg::CL),
			ShiftRotateCount::One => 1,
			ShiftRotateCount::Imm(x) => x
		}
	}

	pub fn run_srgop_ins<OperandType>(&mut self, mem: &mut Memory, op: ShiftRotateOpCode, shift_count: ShiftRotateCount, operand: OperandType)
		where CPU: OperandAccess<OperandType>
	{
		let from_value = self.load_operand(mem, &operand);
		/* Processors >= 286 mask the count with 0x1f, but not the 8086... */
		let cnt = self.get_srcount(shift_count);

		/* We should not do anything if cnt == 0; rather than handling this case
		 * in many places, just discard it here */
		if cnt == 0
		{ return }

		/* 8 for u8, 16 for u16 */
		let bit_count = <CPU as OperandAccess<OperandType>>::ValueType::bit_count();
		let zero = <CPU as OperandAccess<OperandType>>::ValueType::zero();
		match op
		{
			ShiftRotateOpCode::SHL =>
			{
				/* Rust seems to prevent us from shifting by more than the amount of bits of 
				 * the considered type. In the generated code, CL is bitwise and-ed with
				 * 0xf for 16-bit numbers... */
				let to_value = 
					if cnt < bit_count { from_value << cnt }
					else { zero };
				self.set_szp_flags(&to_value);
				if cnt == 1
				{ self.set_oc_flags(from_value.shl1_oc_flags_fo()); }
				else
				{ self.set_flag_value(FLAG_C, from_value.shlcl_c_flag_fo(cnt)); }
				self.store_operand(mem, &operand, to_value)
			}
			ShiftRotateOpCode::SHR =>
			{
				let to_value = 
					if cnt < bit_count { from_value >> cnt }
					else { zero };
				self.set_szp_flags(&to_value);
				if cnt == 1
				{ self.set_oc_flags(from_value.shr1_oc_flags_fo()); }
				else
				{ self.set_flag_value(FLAG_C, from_value.shrcl_c_flag_fo(cnt)); }
				self.store_operand(mem, &operand, to_value)
			}
			ShiftRotateOpCode::SAR =>
			{
				let to_value = from_value.sar(cnt);
				self.set_szp_flags(&to_value);
				if cnt == 1
				{ self.set_oc_flags(from_value.sar1_oc_flags_fo()); }
				else
				{ self.set_flag_value(FLAG_C, from_value.sarcl_c_flag_fo(cnt)); }
				self.store_operand(mem, &operand, to_value)
			}
			ShiftRotateOpCode::ROL =>
			{
				let to_value = from_value.rol(cnt);
				if cnt == 1
				{ self.set_oc_flags(from_value.shl1_oc_flags_fo()); }
				else
				{ self.set_flag_value(FLAG_C, from_value.rolcl_c_flag_fo(cnt)); }
				self.store_operand(mem, &operand, to_value)
			}
			ShiftRotateOpCode::RCL =>
			{
				let has_carry = self.flags & FLAG_C != 0;
				let to_value = from_value.rcl(has_carry, cnt);
				if cnt == 1
				{ self.set_oc_flags(from_value.shl1_oc_flags_fo()); }
				else
				{ self.set_flag_value(FLAG_C, from_value.rclcl_c_flag_fo(has_carry, cnt)); }
				self.store_operand(mem, &operand, to_value)
			}
			ShiftRotateOpCode::ROR =>
			{
				let to_value = from_value.ror(cnt);
				if cnt == 1
				{ self.set_oc_flags(from_value.ror1_oc_flags_fo()); }
				else
				{ self.set_flag_value(FLAG_C, from_value.rorcl_c_flag_fo(cnt)); }
				self.store_operand(mem, &operand, to_value)
			}
			ShiftRotateOpCode::RCR =>
			{
				let has_carry = self.flags & FLAG_C != 0;
				let to_value = from_value.rcr(has_carry, cnt);
				if cnt == 1
				{ self.set_oc_flags(from_value.rcr1_oc_flags_fo(has_carry)); }
				else
				{ self.set_flag_value(FLAG_C, from_value.rcrcl_c_flag_fo(has_carry, cnt)); }
				self.store_operand(mem, &operand, to_value)
			}
		}
	}

	pub fn run_sgop_ins<OperandType>(&mut self, mem: &mut Memory, op: SingleOperandOpCode, operand: &OperandType)
		where CPU: OperandAccess<OperandType>, <CPU as OperandAccess<OperandType>>::ValueType: Operand<<CPU as OperandAccess<OperandType>>::ValueType>
	{
		let value = self.load_operand(mem, &operand);
		// TODO: is this lambda necessary?
		let store = |cpu: &mut CPU, mem: &mut Memory, result: <CPU as OperandAccess<OperandType>>::ValueType|
		{
			cpu.store_operand(mem, &operand, result)
		};

		match op
		{
			SingleOperandOpCode::DEC =>
			{
				let mut result = value;
				result.dec();
				self.set_szp_flags(&result);
				self.set_oa_flags(result.dec_oa_flags());
				store(self, mem, result);
			},
			SingleOperandOpCode::INC =>
			{
				let mut result = value;
				result.inc();
				self.set_szp_flags(&result);
				self.set_oa_flags(result.inc_oa_flags());
				store(self, mem, result);
			},
			SingleOperandOpCode::NEG =>
			{
				let mut result = value;
				result.neg();
				self.set_szp_flags(&result);
				self.set_oca_flags(result.neg_oca_flags());
				store(self, mem, result);
			},
			SingleOperandOpCode::NOT =>
			{
				let mut result = value;
				result.not();
				store(self, mem, result);
			},
			SingleOperandOpCode::MUL =>
			{
				let (hi, lo, overflow) = value.mul(self.get_acc());
				self.set_acc(lo);
				self.set_aux(hi);
				self.set_flag_value(FLAG_C, overflow);
				self.set_flag_value(FLAG_O, overflow);
			},
			SingleOperandOpCode::IMUL =>
			{
				let (hi, lo, overflow) = value.imul(self.get_acc());
				self.set_acc(lo);
				self.set_aux(hi);
				self.set_flag_value(FLAG_C, overflow);
				self.set_flag_value(FLAG_O, overflow);
			},
			SingleOperandOpCode::DIV =>
			{
				match value.div(self.get_aux(), self.get_acc())
				{
					None => self.request_interrupt(mem, 0),
					Some((quot, remain)) =>
					{
						self.set_acc(quot);
						self.set_aux(remain);
					}
				}
			}
			SingleOperandOpCode::IDIV =>
			{
				match value.idiv(self.get_aux(), self.get_acc())
				{
					None => self.request_interrupt(mem, 0),
					Some((quot, remain)) =>
					{
						self.set_acc(quot);
						self.set_aux(remain);
					}
				}
			}
			_ => panic!("Unsupported opcode: {:?}", op)
		}
	}

	pub fn run_sbop_ins(&mut self, mem: &mut Memory, op: SingleOperandOpCode, oper: BOperand)
	{
		self.run_sgop_ins(mem, op, &oper)
	}

	pub fn run_swop_ins(&mut self, mem: &mut Memory, op: SingleOperandOpCode, oper: WOperand)
	{
		let op_value = self.load_operand(mem, &oper);

		match op
		{
			SingleOperandOpCode::PUSH => 
			{
				// TODO: PUSH SP behaves differently between 8086 and 286+ processors
				self.stack_push(mem, op_value)
			},
			SingleOperandOpCode::POP =>
			{
				if oper == WOperand::SegReg(SegReg::CS)
				{
					println!("Error: POP CS instruction encountered.");
					println!("Either the code is 8086-specific and really intends to this horrible thing, either you are running 186+ code.");
					println!("Aborting before this gets ugly...");
					self.state = CPUState::Crashed;
				}
				else
				{
					let val = self.stack_pop(mem);
					self.store_operand(mem, &oper, val)
				}
			},
			_ => self.run_sgop_ins(mem, op, &oper)
		}
	}

	pub fn handle_prefix(&mut self, prefix: Prefix)
	{
		match prefix
		{
			Prefix::LOCK => cpu_print!("Warning: unhandled prefix: LOCK{}",""),
			Prefix::SEGMENT(sr) => self.segment_override_prefix = Some(sr),
			Prefix::REP => self.rep_prefix = Some(RepPrefix::Rep),
			Prefix::REPNE => self.rep_prefix = Some(RepPrefix::Repne)
		}
	}

	pub fn run_imgop_ins<OperandType>(&mut self, mem: &mut Memory, op: ImplicitOperandOpCode, src: &OperandType, dst: &OperandType)
		where CPU: OperandAccess<OperandType>
	{
		let operand_size = (<CPU as OperandAccess<OperandType>>::ValueType::bit_count() / 8) as i16;
		let increment: u16 = if self.flags & FLAG_D != 0 {(-1 * operand_size) as u16} else {operand_size as u16};
		
		if self.rep_prefix != None && self.cx == 0
		{
			/* We can reach this case if somebody
			 * attempts to run REP XXX with CX=0 */
			self.rep_prefix = None;
			return;
		}

		match op
		{
			ImplicitOperandOpCode::MOVS =>
			{
				let value = self.load_operand(mem, src);
				self.store_operand(mem, dst, value);
				self.si += increment;
				self.di += increment;
			},
			ImplicitOperandOpCode::LODS =>
			{
				let value = self.load_operand(mem, src);
				self.set_acc(value);
				self.si += increment;
			},
			ImplicitOperandOpCode::STOS =>
			{
				let value = self.get_acc();
				self.store_operand(mem, dst, value);
				self.di += increment;
			},
			ImplicitOperandOpCode::CMPS =>
			{
				let from_value = self.load_operand(mem, src);
				let to_value = self.load_operand(mem, dst);
				let result = from_value - to_value;
				self.set_oca_flags(result.sub_oca_flags(from_value, to_value));
				self.set_szp_flags(&result);

				self.si += increment;
				self.di += increment;
			},
			ImplicitOperandOpCode::SCAS =>
			{
				let from_value = self.get_acc();
				let to_value = self.load_operand(mem, dst);
				let result = from_value - to_value;
				self.set_oca_flags(result.sub_oca_flags(from_value, to_value));
				self.set_szp_flags(&result);

				self.di += increment;
			}
		}

		/* Handle prefix (flags & CX update) */
		if let Some(rep_mode) = self.rep_prefix
		{
			self.cx -= 1;

			let should_rep_end = self.cx == 0 ||
				match op
				{
					ImplicitOperandOpCode::MOVS | ImplicitOperandOpCode::STOS | ImplicitOperandOpCode::LODS => false, // LODS can actually be used with REP... TODO: Why?
					ImplicitOperandOpCode::CMPS | ImplicitOperandOpCode::SCAS =>
					match rep_mode
					{
						RepPrefix::Rep => self.flags & FLAG_Z == 0, // REPE
						RepPrefix::Repne => self.flags & FLAG_Z != 0,
					}
				};

			if should_rep_end
			{
				self.rep_prefix = None;

				if cfg!(feature="cpu-trace") && op == ImplicitOperandOpCode::MOVS && self.ds != self.es
				{
					if let &mut Some(ref mut log) = &mut self.log
					{
						write!(log, "{:04x} --> {:04x}: COPY\n", self.ds, self.es).unwrap();
					}
				}
			}
		}
	}

	pub fn run_sfcop_ins(&mut self, mem: &mut Memory, op: SingleOperandFCOpCode, to: FlowControlOperand)
	{
		let (is_far, cs, ip) = self.load_fcoperand(mem, &to);

		match op
		{
			SingleOperandFCOpCode::JMP =>
			{
				if cfg!(feature="cpu-trace") && is_far
				{
					if let &mut Some(ref mut log) = &mut self.log
					{
						write!(log, "{:04x} -> {:04x}: {:04x} jumps to {:04x}\n", self.cs, cs, self.ip, ip).unwrap();
					}
				}
			}
			SingleOperandFCOpCode::CALL =>
			{
				let (old_cs, old_ip) = (self.cs, self.ip);

				if is_far
				{
					if cfg!(feature="cpu-trace")
					{
						if let &mut Some(ref mut log) = &mut self.log
						{
							write!(log, "{:04x} -> {:04x}: {:04x} calls {:04x}\n", old_cs, cs, old_ip, ip).unwrap();
						}
					}

					self.stack_push(mem, old_cs);
				}
				self.stack_push(mem, old_ip);
			}
		}

		self.cs = cs;
		self.ip = ip;
	}

	pub fn run_fcnoop_seg_ins(&mut self, mem: &Memory, op: NoOpFCOpCode)
	{
		match op
		{
			NoOpFCOpCode::RET => self.ip = self.stack_pop(mem)
		}
	}

	pub fn run_fcnoop_iseg_ins(&mut self, mem: &Memory, op: NoOpFCOpCode)
	{
		match op
		{
			NoOpFCOpCode::RET =>
			{
				let ip = self.stack_pop(mem);
				let cs = self.stack_pop(mem);

				if cfg!(feature="cpu-trace")
				{
					if let &mut Some(ref mut log) = &mut self.log
					{
						write!(log, "{:04x} --> {:04x}: {:04x} returns to {:04x}\n", self.cs, cs, self.ip, ip).unwrap();
					}
				}

				self.ip = ip;
				self.cs = cs;
			}
		}
	}

	pub fn run_swfcop_ins(&mut self, mem: &Memory, op: SingleWImmFCOpCode, x: SingleWImmFCOperand)
	{
		match op
		{
			SingleWImmFCOpCode::RETANDADDTOSP =>
			{
				match x
				{
					SingleWImmFCOperand::Seg(to_add) =>
					{
						self.ip = self.stack_pop(mem);
						self.sp += to_add;
					}
					SingleWImmFCOperand::InterSeg(to_add) =>
					{
						let ip = self.stack_pop(mem);
						let cs = self.stack_pop(mem);

						if cfg!(feature="cpu-trace")
						{
							if let &mut Some(ref mut log) = &mut self.log
							{
								write!(log, "{:04x} --> {:04x}: {:04x} returns to {:04x},sp+{}=\n", self.cs, cs, self.ip, ip, to_add).unwrap();
							}
						}

						self.ip = ip;
						self.cs = cs;
						self.sp += to_add;
					}
				}
			}
		}
	}
}