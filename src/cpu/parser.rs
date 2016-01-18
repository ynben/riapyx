use super::instruction::*;

fn get_breg(reg: u8) -> BReg
{
	match reg
	{
		0 => BReg::AL,
		1 => BReg::CL,
		2 => BReg::DL,
		3 => BReg::BL,
		4 => BReg::AH,
		5 => BReg::CH,
		6 => BReg::DH,
		7 => BReg::BH,
		_ => panic!("Unexpected register")
	}
}

fn get_wreg(reg: u8) -> WReg
{
	match reg
	{
		0 => WReg::AX,
		1 => WReg::CX,
		2 => WReg::DX,
		3 => WReg::BX,
		4 => WReg::SP,
		5 => WReg::BP,
		6 => WReg::SI,
		7 => WReg::DI,
		_ => panic!("Unexpected register")
	}
}

fn get_segreg(sreg: u8) -> SegReg
{
	match sreg
	{
		0 => SegReg::ES,
		1 => SegReg::CS,
		2 => SegReg::SS,
		3 => SegReg::DS,
		_ => panic!("Unexpected segment register")
	}
}

fn get_indirect_register(rm: u8) -> IndReg
{
	match rm
	{
		0 => IndReg::BXSI,
		1 => IndReg::BXDI,
		2 => IndReg::BPSI,
		3 => IndReg::BPDI,
		4 => IndReg::SI,
		5 => IndReg::DI,
		6 => IndReg::BP,
		7 => IndReg::BX,
		_ => panic!("Unexpected register")
	}
}

enum RmOperand
{
	Direct(u16),
	Indirect(IndReg),
	Indirect8iDis(IndReg, i8),
	Indirect16uDis(IndReg, u16),
	Reg(u8)
}

fn get_simple_rm_operand(rm: u8, data: &[u8]) -> (RmOperand, u16)
{
	if rm == 6
	{
		(RmOperand::Direct((data[2] as u16) + ((data[3] as u16) << 8)), 2)
	}
	else
	{
	    (RmOperand::Indirect(get_indirect_register(rm)), 0)
	}
}

fn get_8idisplaced_rm_operand(rm: u8, data: &[u8]) -> RmOperand
{
	let disp = data[2] as i8;

	RmOperand::Indirect8iDis(get_indirect_register(rm), disp)
}

fn get_16udisplaced_rm_operand(rm: u8, data: &[u8]) -> RmOperand
{
	let disp: u16 = (data[2] as u16) + ((data[3] as u16) << 8);

	RmOperand::Indirect16uDis(get_indirect_register(rm), disp)
}

fn get_rm_operand(data: &[u8]) -> (RmOperand, u16)
{
	let rm = data[1] & 0b111;
	let mode = data[1] >> 6;

	match mode
	{
		0 => get_simple_rm_operand(rm, data),
		1 => (get_8idisplaced_rm_operand(rm, data), 1),
		2 => (get_16udisplaced_rm_operand(rm, data), 2),
		3 => (RmOperand::Reg(rm), 0),
		_ => panic!("Unexpected mode value")
	}
}

fn get_rm_boperand(data: &[u8]) -> (BOperand, u16)
{
	let (op, sz) = get_rm_operand(data);
	
	(
		match op
		{
			RmOperand::Direct(addr) => BOperand::Direct(addr),
			RmOperand::Indirect(ind_reg) => BOperand::Indirect(ind_reg),
			RmOperand::Indirect8iDis(ind_reg, dis) => BOperand::Indirect8iDis(ind_reg, dis),
			RmOperand::Indirect16uDis(ind_reg, dis) => BOperand::Indirect16uDis(ind_reg, dis),
			RmOperand::Reg(reg) => BOperand::Reg(get_breg(reg))
		}
		, sz
	)
}

fn get_rm_woperand(data: &[u8]) -> (WOperand, u16)
{
	let (op, sz) = get_rm_operand(data);
	
	(
		match op
		{
			RmOperand::Direct(addr) => WOperand::Direct(addr),
			RmOperand::Indirect(ind_reg) => WOperand::Indirect(ind_reg),
			RmOperand::Indirect8iDis(ind_reg, dis) => WOperand::Indirect8iDis(ind_reg, dis),
			RmOperand::Indirect16uDis(ind_reg, dis) => WOperand::Indirect16uDis(ind_reg, dis),
			RmOperand::Reg(reg) => WOperand::Reg(get_wreg(reg))
		}
		, sz
	)
}

fn dispatch_w <OpCodeType, ByteFunc, WordFunc> (opcode: OpCodeType, w: u8, byte: ByteFunc, word: WordFunc) -> SizedInstruction
	where ByteFunc: Fn(OpCodeType) -> SizedInstruction, WordFunc: Fn(OpCodeType) -> SizedInstruction
{
	match w
	{
		0 => byte(opcode),
		1 => word(opcode),
		_ => panic!("Unexpected w")
	}
}

pub fn parse_instruction(bytecode: &[u8]) -> SizedInstruction
{
	/* TODO: ** prefixes ** */
	let sized_ins = prefix4bit(bytecode);
	sized_ins
}

// Catch immediate mov to reg
fn prefix4bit(bytecode: &[u8]) -> SizedInstruction
{
	let tentative_opcode = bytecode[0] >> 4;
	let opcode_byte = bytecode[0];
	let make_imm_reg_instruction = |opcode: TwoOperandsOpCode|
	{
		let w = (opcode_byte & 0b00001000) >> 3;
		match w
		{
			0 =>
			{
				let register = get_breg(opcode_byte & 0b111);
				let imm = bytecode[1];
				SizedInstruction
				{
					instruction: Instruction::TwoBOperands(opcode, BOperand::Immediate(imm), BOperand::Reg(register)),
					size: 2
				}
			},
			1 =>
			{
				let register = get_wreg(opcode_byte & 0b111);
				let imm = (bytecode[1] as u16) + ((bytecode[2] as u16) << 8);
				SizedInstruction
				{
					instruction: Instruction::TwoWOperands(opcode, WOperand::Immediate(imm), WOperand::Reg(register)),
					size: 3
				}
			},
			_ => panic!("Unexpected w")
		}
	};

	match tentative_opcode
	{
		0b1011 => make_imm_reg_instruction(TwoOperandsOpCode::MOV),
		_ => prefix5bit(bytecode)
	}
}

// Catch register instructions
fn prefix5bit(bytecode: &[u8]) -> SizedInstruction
{
	let tentative_opcode = bytecode[0] >> 3;
	let reg_id = bytecode[0] & 0b00000111;
	let register = get_wreg(reg_id);
	let make_reg_instruction = |op: SingleOperandOpCode| -> SizedInstruction
	{
		SizedInstruction
		{
			instruction: Instruction::SingleWOperand(op, WOperand::Reg(register)),
			size: 1
		}
	};

	match tentative_opcode
	{
		0b01010 => make_reg_instruction(SingleOperandOpCode::PUSH),
		0b01011 => make_reg_instruction(SingleOperandOpCode::POP),
		0b01000 => make_reg_instruction(SingleOperandOpCode::INC),
		0b01001 => make_reg_instruction(SingleOperandOpCode::DEC),
		0b10010 => 
		{
			SizedInstruction
			{
				instruction: Instruction::TwoWOperands(
					TwoOperandsOpCode::XCHG,
					WOperand::Reg(WReg::AX),
					WOperand::Reg(register)),
				size: 1
			}
		}
		// 0b11011 => esc, <-- TODO
		_ => prefix6bit(bytecode)
	}
}

fn apply_swap<Operand> (a: Operand, b: Operand, swap: u8) -> (Operand, Operand)
{
	match swap
	{
		0 => (a,b),
		1 => (b,a),
		_ => panic!("Bad swap value")
	}
}

fn get_u16(off: usize, data: &[u8]) -> u16
{
	data[off] as u16 | ((data[off+1] as u16) << 8)
}

fn two_operands_instruction_reg_rm(opcode: TwoOperandsOpCode, d: u8, w: u8, bytecode: &[u8]) -> SizedInstruction
{
	let addressing_byte = bytecode[1];
	let reg = (addressing_byte & 0b00111000) >> 3;

	let (instruction, rm_operand_size) = 
		match w
		{
			0 =>
			{
				let (rm_operand, rm_operand_size) = get_rm_boperand(bytecode);
				let (op1, op2) = apply_swap(BOperand::Reg(get_breg(reg)), rm_operand, d);
				(Instruction::TwoBOperands(opcode, op1, op2), rm_operand_size)
			},
			1 =>
			{
				let (rm_operand, rm_operand_size) = get_rm_woperand(bytecode);
				let (op1, op2) = apply_swap(WOperand::Reg(get_wreg(reg)), rm_operand, d);
				(Instruction::TwoWOperands(opcode, op1, op2), rm_operand_size)
			},
			_ => panic!("Invalid w")
		};
	

	SizedInstruction
	{
		instruction: instruction,
		size: 2 + rm_operand_size
	}
}

fn prefix6bit(bytecode: &[u8]) -> SizedInstruction
{
	let opcode_byte = bytecode[0];
	let tentative_opcode = bytecode[0] >> 2;
	let d = (opcode_byte & 0b00000010) >> 1;
	let w = (opcode_byte & 0b00000001) >> 0;

	let usual_instruction = |opcode: TwoOperandsOpCode|
		two_operands_instruction_reg_rm(opcode, d, w, bytecode);

	match tentative_opcode
	{
		0b101000 => // Mov immediate addressing <-> acc
		{
			let addr = (bytecode[1] as u16) + ((bytecode[2] as u16) << 8);

			let instruction =
				match w
				{
					0 =>
					{
						let (op1, op2) = apply_swap(BOperand::Direct(addr), BOperand::Reg(BReg::AL), d);
						Instruction::TwoBOperands(TwoOperandsOpCode::MOV, op1, op2)
					},
					1 =>
					{
						let (op1, op2) = apply_swap(WOperand::Direct(addr), WOperand::Reg(WReg::AX), d);
						Instruction::TwoWOperands(TwoOperandsOpCode::MOV, op1, op2)
					},
					_ => panic!("Invalid w")
				};

			SizedInstruction
			{
				instruction: instruction,
				size: 3
			}
		}

		/* No-mindfuck 6-bit prefix instructions, yay */
		0b100010 => usual_instruction(TwoOperandsOpCode::MOV),
		0b000000 => usual_instruction(TwoOperandsOpCode::ADD), /* go back here once we know what conflicts with this */
		0b000100 => usual_instruction(TwoOperandsOpCode::ADC),
		0b001010 => usual_instruction(TwoOperandsOpCode::SUB),
		0b000110 => usual_instruction(TwoOperandsOpCode::SBB),
		0b001110 => usual_instruction(TwoOperandsOpCode::CMP),
		0b001000 => usual_instruction(TwoOperandsOpCode::AND),
		0b000010 => usual_instruction(TwoOperandsOpCode::OR),
		0b001100 => usual_instruction(TwoOperandsOpCode::XOR),
		_ => prefix3_3_bit(bytecode)
	}
}

// Catch segment register push/pop
fn prefix3_3_bit(bytecode: &[u8]) -> SizedInstruction
{
	let opcode_byte = bytecode[0];
	let tentative_opcode = opcode_byte & 0b11100111;
	let sreg = (opcode_byte & 0b00011000) >> 3;

	let instruction = |opcode: SingleOperandOpCode|
	{
		SizedInstruction
		{
			instruction: Instruction::SingleWOperand(opcode, WOperand::SegReg(get_segreg(sreg))),
			size: 1
		}
	};

	match tentative_opcode
	{
		0b00000111 => instruction(SingleOperandOpCode::POP),
		0b00000110 => instruction(SingleOperandOpCode::PUSH),
		0b00100110 => 
		SizedInstruction
		{
			instruction: Instruction::Prefix(Prefix::SEGMENT(get_segreg(sreg))),
			size: 1
		},
		_ => prefix7bit(bytecode)
	}
}

// Catch 7-bits opcodes?
fn prefix7bit(bytecode: &[u8]) -> SizedInstruction
{
	let tentative_opcode = bytecode[0] >> 1;
	let w = bytecode[0] & 0b1;

	let two_op_ins_with_acc = |opcode: TwoOperandsOpCode|
	{
		match w
		{
			0 => SizedInstruction
			{
				instruction: Instruction::TwoBOperands(opcode, BOperand::Immediate(bytecode[1]), BOperand::Reg(BReg::AL)),
				size: 2
			},
			1 => 
			{
				let imm = (bytecode[1] as u16) + ((bytecode[2] as u16) << 8);
				SizedInstruction
				{
					instruction: Instruction::TwoWOperands(opcode, WOperand::Immediate(imm), WOperand::Reg(WReg::AX)),
					size: 3
				}
			},
			_ => panic!("Invalid w")
		}
	};

	let string_ins = |opcode: ImplicitOperandOpCode|
	{
		SizedInstruction
		{
			instruction: match w
				{
					0 => Instruction::ImplicitBOperand(opcode),
					1 => Instruction::ImplicitWOperand(opcode),
					_ => panic!("invalid w")
				},
			size: 1
		}
	};

	match tentative_opcode
	{
		0b1000011 => two_operands_instruction_reg_rm(TwoOperandsOpCode::XCHG, 0, w, bytecode),
		0b0000010 => two_op_ins_with_acc(TwoOperandsOpCode::ADD),
		0b0001010 => two_op_ins_with_acc(TwoOperandsOpCode::ADC),
		0b0010110 => two_op_ins_with_acc(TwoOperandsOpCode::SUB),
		0b0001110 => two_op_ins_with_acc(TwoOperandsOpCode::SBB),
		0b0010010 => two_op_ins_with_acc(TwoOperandsOpCode::AND),
		0b1010100 => two_op_ins_with_acc(TwoOperandsOpCode::TEST),
		0b1000010 => two_operands_instruction_reg_rm(TwoOperandsOpCode::TEST, 0, w, bytecode), // NOTE: the 8086 user manual is mistaken here
		0b0000110 => two_op_ins_with_acc(TwoOperandsOpCode::OR),
		0b0011010 => two_op_ins_with_acc(TwoOperandsOpCode::XOR),
		0b0011110 => two_op_ins_with_acc(TwoOperandsOpCode::CMP),
		0b1010010 => string_ins(ImplicitOperandOpCode::MOVS),
		0b1010011 => string_ins(ImplicitOperandOpCode::CMPS),
		0b1010111 => string_ins(ImplicitOperandOpCode::SCAS),
		0b1010110 => string_ins(ImplicitOperandOpCode::LODS),
		0b1010101 => string_ins(ImplicitOperandOpCode::STOS),
		_ => prefix_8bit(bytecode)
	}
}

fn prefix_8bit(bytecode: &[u8]) -> SizedInstruction
{
	let tentative_opcode = bytecode[0];
	let no_op_ins = |opcode: NoOperandOpCode|
	{
		SizedInstruction
		{
			instruction: Instruction::NoOperand(opcode),
			size: 1
		}
	};

	let jump_ins = |opcode: SingleBImmOperandOpCode|
	{
		SizedInstruction
		{
			instruction: Instruction::SingleBImmOperand(opcode, bytecode[1]),
			size: 2
		}
	};

	let iov_ins = |opcode: SingleBImmOperandOpCode|
	{
		SizedInstruction
		{
			instruction: Instruction::SingleBImmOperand(opcode, 0x42),
			size: 1
		}
	};

	let direct_intraseg_fc_ins = |opcode: SingleOperandFCOpCode|
	{
		let op = FlowControlOperand::DirectSeg((bytecode[1] as u16) + ((bytecode[2] as u16) << 8));
		SizedInstruction
		{
			instruction: Instruction::SingleFCOperand(opcode, op),
			size: 3
		}
	};

	let direct_interseg_fc_ins = |opcode: SingleOperandFCOpCode|
	{
		let op = FlowControlOperand::DirectInterSeg(
			(bytecode[3] as u16) + ((bytecode[4] as u16) << 8), // CS
			(bytecode[1] as u16) + ((bytecode[2] as u16) << 8)); // IP
		SizedInstruction
		{
			instruction: Instruction::SingleFCOperand(opcode, op),
			size: 5
		}
	};

	let prefix_ins = |prefix: Prefix|
	{
		SizedInstruction
		{
			instruction: Instruction::Prefix(prefix),
			size: 1
		}
	};

	match tentative_opcode
	{
		0b11010111 => no_op_ins(NoOperandOpCode::XLAT),
		0b10001101 => two_operands_instruction_reg_rm(TwoOperandsOpCode::LEA, 1, 1, bytecode),
		0b11000101 => two_operands_instruction_reg_rm(TwoOperandsOpCode::LDS, 1, 1, bytecode),
		0b11000100 => two_operands_instruction_reg_rm(TwoOperandsOpCode::LES, 1, 1, bytecode),
		0b10011111 => no_op_ins(NoOperandOpCode::LAHF),
		0b10011110 => no_op_ins(NoOperandOpCode::SAHF),
		0b10011100 => no_op_ins(NoOperandOpCode::PUSHF),
		0b10011101 => no_op_ins(NoOperandOpCode::POPF),
		0b00110111 => no_op_ins(NoOperandOpCode::AAA),
		0b00100111 => no_op_ins(NoOperandOpCode::DAA),
		0b00111111 => no_op_ins(NoOperandOpCode::AAS),
		0b00101111 => no_op_ins(NoOperandOpCode::DAS),
		0b10011000 => no_op_ins(NoOperandOpCode::CBW),
		0b10011001 => no_op_ins(NoOperandOpCode::CWD),
		0b11101000 => direct_intraseg_fc_ins(SingleOperandFCOpCode::CALL),
		0b10011010 => direct_interseg_fc_ins(SingleOperandFCOpCode::CALL),
		0b11101001 => direct_intraseg_fc_ins(SingleOperandFCOpCode::JMP),
		0b11101011 => jump_ins(SingleBImmOperandOpCode::JMPS),
		0b11101010 => direct_interseg_fc_ins(SingleOperandFCOpCode::JMP),
		0b11000011 => /* ret intraseg */
			SizedInstruction
			{
				instruction: Instruction::FCNoOperandSeg(NoOpFCOpCode::RET),
				size: 1
			},
		0b11000010 => /* ret intraseg; sp += data */
		{
			let op = SingleWImmFCOperand::Seg((bytecode[1] as u16) + ((bytecode[2] as u16) << 8));
			SizedInstruction
			{
				instruction: Instruction::SingleWImmFCOperand(SingleWImmFCOpCode::RETANDADDTOSP, op),
				size: 3
			}
		},
		0b11001011 => /* ret interseg */
			SizedInstruction
			{
				instruction: Instruction::FCNoOperandInterSeg(NoOpFCOpCode::RET),
				size: 1
			},
		0b11001010 => /* ret interseg; sp += data */
		{
			let op = SingleWImmFCOperand::InterSeg((bytecode[1] as u16) + ((bytecode[2] as u16) << 8));
			SizedInstruction
			{
				instruction: Instruction::SingleWImmFCOperand(SingleWImmFCOpCode::RETANDADDTOSP, op),
				size: 3
			}
		},
		0b01110100 => jump_ins(SingleBImmOperandOpCode::JE),
		0b01111100 => jump_ins(SingleBImmOperandOpCode::JL),
		0b01111110 => jump_ins(SingleBImmOperandOpCode::JLE),
		0b01110010 => jump_ins(SingleBImmOperandOpCode::JB),
		0b01110110 => jump_ins(SingleBImmOperandOpCode::JBE),
		0b01111010 => jump_ins(SingleBImmOperandOpCode::JP),
		0b01110000 => jump_ins(SingleBImmOperandOpCode::JO),
		0b01111000 => jump_ins(SingleBImmOperandOpCode::JS),
		0b01110101 => jump_ins(SingleBImmOperandOpCode::JNE),
		0b01111101 => jump_ins(SingleBImmOperandOpCode::JNL),
		0b01111111 => jump_ins(SingleBImmOperandOpCode::JNLE),
		0b01110011 => jump_ins(SingleBImmOperandOpCode::JNB),
		0b01110111 => jump_ins(SingleBImmOperandOpCode::JNBE),
		0b01111011 => jump_ins(SingleBImmOperandOpCode::JNP),
		0b01110001 => jump_ins(SingleBImmOperandOpCode::JNO),
		0b01111001 => jump_ins(SingleBImmOperandOpCode::JNS),
		0b11100010 => jump_ins(SingleBImmOperandOpCode::LOOP),
		0b11100001 => jump_ins(SingleBImmOperandOpCode::LOOPZ),
		0b11100000 => jump_ins(SingleBImmOperandOpCode::LOOPNZ),
		0b11100011 => jump_ins(SingleBImmOperandOpCode::JCXZ),
		/* end control transfer */
		0b11001101 => jump_ins(SingleBImmOperandOpCode::INT), // actually same kind as jumps instruction...
		0b11100100 => jump_ins(SingleBImmOperandOpCode::INB),
		0b11100101 => jump_ins(SingleBImmOperandOpCode::INW),
		0b11100110 => jump_ins(SingleBImmOperandOpCode::OUTB),
		0b11100111 => jump_ins(SingleBImmOperandOpCode::OUTW),
		0b11101100 => iov_ins(SingleBImmOperandOpCode::INVB),
		0b11101101 => iov_ins(SingleBImmOperandOpCode::INVW),
		0b11101110 => iov_ins(SingleBImmOperandOpCode::OUTVB),
		0b11101111 => iov_ins(SingleBImmOperandOpCode::OUTVW),
		/*0b11001100 => int 3 ?*/
		0b11001110 => no_op_ins(NoOperandOpCode::INTO),
		0b11001111 => no_op_ins(NoOperandOpCode::IRET),
		0b11111000 => no_op_ins(NoOperandOpCode::CLC),
		0b11110101 => no_op_ins(NoOperandOpCode::CMC),
		0b11111001 => no_op_ins(NoOperandOpCode::STC),
		0b11111100 => no_op_ins(NoOperandOpCode::CLD),
		0b11111101 => no_op_ins(NoOperandOpCode::STD),
		0b11111010 => no_op_ins(NoOperandOpCode::CLI),
		0b11111011 => no_op_ins(NoOperandOpCode::STI),
		0b11110100 => no_op_ins(NoOperandOpCode::HLT),
		0b10011011 => no_op_ins(NoOperandOpCode::WAIT),
		0b11110010 => prefix_ins(Prefix::REPNE),
		0b11110011 => prefix_ins(Prefix::REP),
		0b11110000 => prefix_ins(Prefix::LOCK),
		0b11001100 => SizedInstruction
		{
			size: 1,
			instruction: Instruction::SingleBImmOperand(SingleBImmOperandOpCode::INT, 0x3)
		},
		_ => prefix7_1_1bit(bytecode)
	}
}

fn prefix7_1_1bit(bytecode: &[u8]) -> SizedInstruction
{
	let opcode_bytes = ((bytecode[0] as u16) << 8) + bytecode[1] as u16;
	let masked = opcode_bytes & 0b1111110100100000;

	match masked
	{
		0b1000110000000000 => 
		{
			/* Mov from/to SegReg */
			let sr = (bytecode[1] & 0b00011000) >> 3;
			let d = (bytecode[0] & 0b10) >> 1;
			let (rm_op, rm_sz) = get_rm_woperand(bytecode);
			let (op1, op2) = apply_swap(WOperand::SegReg(get_segreg(sr)), rm_op, d);
			SizedInstruction
			{
				instruction: Instruction::TwoWOperands(TwoOperandsOpCode::MOV, op1, op2),
				size: rm_sz + 2
			}
		}
		_ => prefix6_3bit(bytecode)
	}
}

fn prefix6_3bit(bytecode: &[u8]) -> SizedInstruction
{
	let masked = 0b1111110000111000 & (((bytecode[0] as u16) << 8) | bytecode[1] as u16);
	let w = bytecode[0] & 0b1;
	let sv = (bytecode[0] & 0b10) >> 1;

	let tp_imm_rm_w = |opcode: TwoOperandsOpCode|
	{
		let (op, sz) = get_rm_woperand(bytecode);
		let (imm, immsz) = 
			match sv
			{
				0 => (get_u16(2+sz as usize, bytecode), 2),
				1 => ((bytecode[2+sz as usize] as i8) as u16, 1),
				_ => panic!("Invalid s bit")
			};

		SizedInstruction
		{
			instruction: Instruction::TwoWOperands(opcode, WOperand::Immediate(imm), op),
			size: 2 + sz + immsz
		}
	};

	let tp_imm_rm_b = |opcode: TwoOperandsOpCode|
	{
		let (op, sz) = get_rm_boperand(bytecode);

		SizedInstruction
		{
			instruction: Instruction::TwoBOperands(opcode, BOperand::Immediate(bytecode[2 + sz as usize]), op),
			size: 2 + sz + 1
		}
	};

	let tp_imm_rm = |opcode: TwoOperandsOpCode| 
		dispatch_w(opcode, w, tp_imm_rm_b, tp_imm_rm_w);

	let sr_rm = |opcode: ShiftRotateOpCode|
	{
		let count =
			match sv
			{
				0 => ShiftRotateCount::One,
				1 => ShiftRotateCount::CL,
				_ => panic!("invalid sv")
			};

		match w
		{
			0 =>
			{
				let (op, sz) = get_rm_boperand(bytecode);
				SizedInstruction
				{
					instruction: Instruction::ShiftRotateB(opcode, count, op),
					size: 2+sz
				}
			},
			1 =>
			{
				let (op, sz) = get_rm_woperand(bytecode);
				SizedInstruction
				{
					instruction: Instruction::ShiftRotateW(opcode, count, op),
					size: 2+sz
				}
			}
			_ => panic!("invalid w")
		}
	};

	match masked
	{
		0b1000000000000000 => tp_imm_rm(TwoOperandsOpCode::ADD),
		0b1000000000010000 => tp_imm_rm(TwoOperandsOpCode::ADC),
		0b1000000000101000 => tp_imm_rm(TwoOperandsOpCode::SUB),
		0b1000000000011000 => tp_imm_rm(TwoOperandsOpCode::SBB),
		0b1000000000111000 => tp_imm_rm(TwoOperandsOpCode::CMP),
		0b1101000000100000 => sr_rm(ShiftRotateOpCode::SHL),
		0b1101000000101000 => sr_rm(ShiftRotateOpCode::SHR),
		0b1101000000111000 => sr_rm(ShiftRotateOpCode::SAR),
		0b1101000000000000 => sr_rm(ShiftRotateOpCode::ROL),
		0b1101000000001000 => sr_rm(ShiftRotateOpCode::ROR),
		0b1101000000010000 => sr_rm(ShiftRotateOpCode::RCL),
		0b1101000000011000 => sr_rm(ShiftRotateOpCode::RCR),
		_ => prefix7_3bit(bytecode)
	}
}

fn prefix7_3bit(bytecode: &[u8]) -> SizedInstruction
{
	let masked = 0b1111111000111000 & (((bytecode[0] as u16) << 8) | bytecode[1] as u16);
	let w = bytecode[0] & 0b1;

	let tp_ins_imm_rm_b = |opcode: TwoOperandsOpCode|
	{
		let (op, sz) = get_rm_boperand(bytecode);
		let imm = bytecode[2+sz as usize];
		SizedInstruction
		{
			instruction: Instruction::TwoBOperands(opcode, BOperand::Immediate(imm), op),
			size: 3 + sz
		}
	};

	let tp_ins_imm_rm_w = |opcode: TwoOperandsOpCode|
	{
		let (op, sz) = get_rm_woperand(bytecode);
		let imm = ((bytecode[(2 + sz + 1) as usize] as u16) << 8) | (bytecode[(2 + sz) as usize] as u16);
		SizedInstruction
		{
			instruction: Instruction::TwoWOperands(opcode, WOperand::Immediate(imm), op),
			size: 4 + sz
		}
	};

	/* TODO: this VS a single function that matches w() */
	let tp_ins_imm_rm = |opcode: TwoOperandsOpCode| 
		dispatch_w(opcode, w, tp_ins_imm_rm_b, tp_ins_imm_rm_w);

	let sp_ins_rm_b = |opcode: SingleOperandOpCode|
	{
		let (op, sz) = get_rm_boperand(bytecode);
		SizedInstruction
		{
			instruction: Instruction::SingleBOperand(opcode, op),
			size: 2 + sz
		}
	};

	let sp_ins_rm_w = |opcode: SingleOperandOpCode|
	{
		let (op, sz) = get_rm_woperand(bytecode);
		SizedInstruction
		{
			instruction: Instruction::SingleWOperand(opcode, op),
			size: 2 + sz
		}
	};

	let sp_ins_rm = |opcode: SingleOperandOpCode|
		{ dispatch_w(opcode, w, sp_ins_rm_b, sp_ins_rm_w) };

	match masked
	{
		0b1100011000000000 => tp_ins_imm_rm(TwoOperandsOpCode::MOV),
		0b1111111000000000 => sp_ins_rm(SingleOperandOpCode::INC),
		0b1111111000001000 => sp_ins_rm(SingleOperandOpCode::DEC),
		0b1111011000011000 => sp_ins_rm(SingleOperandOpCode::NEG),
		0b1111011000100000 => sp_ins_rm(SingleOperandOpCode::MUL),
		0b1111011000101000 => sp_ins_rm(SingleOperandOpCode::IMUL),
		0b1111011000110000 => sp_ins_rm(SingleOperandOpCode::DIV),
		0b1111011000111000 => sp_ins_rm(SingleOperandOpCode::IDIV),
		0b1111011000010000 => sp_ins_rm(SingleOperandOpCode::NOT),
		0b1000000000100000 => tp_ins_imm_rm(TwoOperandsOpCode::AND),
		0b1111011000000000 => tp_ins_imm_rm(TwoOperandsOpCode::TEST),
		0b1000000000001000 => tp_ins_imm_rm(TwoOperandsOpCode::OR),
		0b1000000000110000 => tp_ins_imm_rm(TwoOperandsOpCode::XOR),
		_ => prefix8_3bit(bytecode)
	}
}

// Catch opcodes that can only be distinguished by using two bytes...
fn prefix8_3bit(bytecode: &[u8]) -> SizedInstruction
{
	let opcode_bytes = ((bytecode[0] as u16) << 8) + bytecode[1] as u16;
	let masked = opcode_bytes & 0b1111111100111000;

	let get_op_sz = || get_rm_woperand(bytecode);

	let stack_ins = |opcode: SingleOperandOpCode|
	{
		let (op, sz) = get_op_sz();

		SizedInstruction
		{
			instruction: Instruction::SingleWOperand(opcode, op),
			size: 2 + sz
		}
	};

	let fc_ins = |opcode: SingleOperandFCOpCode, op_wrap: fn(WOperand) -> FlowControlOperand|
	{
		let (op, sz) = get_op_sz();

		SizedInstruction
		{
			instruction: Instruction::SingleFCOperand(opcode, op_wrap(op)),
			size: 2 + sz
		}
	};


	let tp_ins_imm8ext_rm = |opcode: TwoOperandsOpCode|
	{
		let (op, sz) = get_rm_woperand(bytecode);
		let imm = bytecode[2+sz as usize];
		SizedInstruction
		{
			instruction: Instruction::TwoWOperands(opcode, WOperand::Immediate((imm as i8) as u16), op),
			size: 3 + sz
		}
	};

	let sr_ins_imm8_rm = |opcode: ShiftRotateOpCode|
	{
		let (op, sz) = get_rm_woperand(bytecode);
		let imm = bytecode[2+sz as usize];
		SizedInstruction
		{
			instruction: Instruction::ShiftRotateW(opcode, ShiftRotateCount::Imm(imm), op),
			size: 3 + sz
		}
	};

	match masked
	{
		0b1111111100110000 => stack_ins(SingleOperandOpCode::PUSH),
		0b1000111100000000 => stack_ins(SingleOperandOpCode::POP),
		0b1100000100100000 => sr_ins_imm8_rm(ShiftRotateOpCode::SHL), // REMOVE ME: unsupported by 8086
		0b1000001100100000 => tp_ins_imm8ext_rm(TwoOperandsOpCode::AND), // REMOVE ME: unsupported by a 8086
		0b1000001100001000 => tp_ins_imm8ext_rm(TwoOperandsOpCode::OR), // REMOVE ME: unsupported by a 8086
		0b1000001100110000 => tp_ins_imm8ext_rm(TwoOperandsOpCode::XOR), // REMOVE ME: unsupported by a 8086
		0b1111111100010000 => fc_ins(SingleOperandFCOpCode::CALL, FlowControlOperand::IndirectSeg),
		0b1111111100011000 => fc_ins(SingleOperandFCOpCode::CALL, FlowControlOperand::IndirectInterSeg),
		0b1111111100100000 => fc_ins(SingleOperandFCOpCode::JMP, FlowControlOperand::IndirectSeg),
		0b1111111100101000 => fc_ins(SingleOperandFCOpCode::JMP, FlowControlOperand::IndirectInterSeg),
		_ => invalid_instruction()
	}
}

fn invalid_instruction() -> SizedInstruction
{
	SizedInstruction
	{
		size: 0,
		instruction: Instruction::Invalid
	}
}
