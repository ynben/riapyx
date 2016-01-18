#[cfg(test)]
mod tests
{
	use super::super::instruction::*;
	use super::super::parser::*;

	fn ins(size: u16, instruction: Instruction) -> SizedInstruction
	{
		SizedInstruction
		{
			size: size,
			instruction: instruction
		}
	}

	#[test]
	fn test()
	{
		let test_cases = vec!
		[
			(vec![0xB8, 0x2A, 0x00],
				ins(3, Instruction::TwoWOperands(
					TwoOperandsOpCode::MOV, 
					WOperand::Immediate(42), 
					WOperand::Reg(WReg::AX)))),
			(vec![0xb0, 0x2b],
				ins(2, Instruction::TwoBOperands(
					TwoOperandsOpCode::MOV, 
					BOperand::Immediate(43),
					BOperand::Reg(BReg::AL)))),
			(vec![0x26], ins(1, Instruction::Prefix(Prefix::SEGMENT(SegReg::ES)))),
			(vec![0x2E], ins(1, Instruction::Prefix(Prefix::SEGMENT(SegReg::CS)))),
			(vec![0xf3], ins(1, Instruction::Prefix(Prefix::REP))),
			(vec![0x8e, 0xd0], ins(2, Instruction::TwoWOperands(
					TwoOperandsOpCode::MOV,
					WOperand::Reg(WReg::AX),
					WOperand::SegReg(SegReg::SS)))),
			(vec![0xbc, 0x00, 0x7c], ins(3, Instruction::TwoWOperands(
					TwoOperandsOpCode::MOV,
					WOperand::Immediate(0x7c00),
					WOperand::Reg(WReg::SP)))),
			(vec![0xc5, 0x37], ins(2, Instruction::TwoWOperands(
					TwoOperandsOpCode::LDS,
					WOperand::Indirect(IndReg::BX),
					WOperand::Reg(WReg::SI)))),
			(vec![0xea, 0x00, 0x7c, 0x00, 0x00], ins(5, Instruction::SingleFCOperand(
				SingleOperandFCOpCode::JMP, 
				FlowControlOperand::DirectInterSeg(0x0, 0x7c00)))),
			(vec![0xB4, 0x0E],
				ins(2, Instruction::TwoBOperands(
					TwoOperandsOpCode::MOV, 
					BOperand::Immediate(0x0E), 
					BOperand::Reg(BReg::AH)))),
			(vec![0x05, 0x03, 0x00],
				ins(3, Instruction::TwoWOperands(
					TwoOperandsOpCode::ADD, 
					WOperand::Immediate(0x3), 
					WOperand::Reg(WReg::AX)))),
			(vec![0x81, 0xcb, 0x00, 0x00],
				ins(4, Instruction::TwoWOperands(
					TwoOperandsOpCode::OR, 
					WOperand::Immediate(0x0), 
					WOperand::Reg(WReg::BX)))),
			(vec![0x74, 0x09],
				ins(2, Instruction::SingleBImmOperand(
					SingleBImmOperandOpCode::JE, 0x09))),
			(vec![0x80, 0x3e, 0x02, 0x2e, 0x01],
				ins(5, Instruction::TwoBOperands(
					TwoOperandsOpCode::CMP, 
					BOperand::Immediate(0x1), 
					BOperand::Direct(0x2e02)))),
			(vec![0x81, 0x3e, 0x34, 0x12, 0x78, 0x56],
				ins(6, Instruction::TwoWOperands(
					TwoOperandsOpCode::CMP,
					WOperand::Immediate(0x5678), 
					WOperand::Direct(0x1234)))),
			(vec![0x8c, 0xd9],
				ins(2, Instruction::TwoWOperands(
					TwoOperandsOpCode::MOV,
					WOperand::SegReg(SegReg::DS),
					WOperand::Reg(WReg::CX)))),
			(vec![0xd3, 0xe0],
				ins(2, Instruction::ShiftRotateW(
					ShiftRotateOpCode::SHL,
					ShiftRotateCount::CL,
					WOperand::Reg(WReg::AX)))),
			(vec![0xd1, 0xe0],
				ins(2, Instruction::ShiftRotateW(
					ShiftRotateOpCode::SHL,
					ShiftRotateCount::One,
					WOperand::Reg(WReg::AX)))),
			(vec![0x86, 0xe9],
				ins(2, Instruction::TwoBOperands(
					TwoOperandsOpCode::XCHG,
					BOperand::Reg(BReg::CH),
					BOperand::Reg(BReg::CL)))),
			(vec![0xe5, 0x42],
				ins(2, Instruction::SingleBImmOperand(
					SingleBImmOperandOpCode::INW, 0x42))),
			(vec![0xe6, 0x43],
				ins(2, Instruction::SingleBImmOperand(
					SingleBImmOperandOpCode::OUTB, 0x43))),
			(vec![0xef],
				ins(1, Instruction::SingleBImmOperand(
					SingleBImmOperandOpCode::OUTVW, 0x42))),
		];

		for (bytecode, expected_instruction) in test_cases
		{
			let instruction = parse_instruction(&bytecode);
			assert_eq!(expected_instruction, instruction);
		}
	}
}