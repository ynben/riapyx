use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum WReg
{
	AX,
	BX,
	CX,
	DX,
	SI, DI,
	BP, SP,
	IP // TODO: don't expose here? (instructions cannot access it directly)
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BReg
{
	AH, AL,
	BH, BL,
	CH, CL,
	DH, DL
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum SegReg
{
	CS, DS, ES, SS
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum IndReg
{
	BXSI, 
	BXDI, 
	BPSI,
	BPDI,
	SI,
	DI,
	BP,
	BX
}

#[derive(Debug, Eq, PartialEq)]
pub enum BOperand
{
	Reg(BReg),
	Indirect(IndReg),
	Immediate(u8),
	Direct(u16),
	Indirect8iDis(IndReg, i8),
	Indirect16uDis(IndReg, u16)
}

#[derive(Debug, Eq, PartialEq)]
pub enum WOperand
{
	Reg(WReg),
	SegReg(SegReg),
	Indirect(IndReg),
	Immediate(u16),
	Direct(u16),
	Indirect8iDis(IndReg, i8),
	Indirect16uDis(IndReg, u16)
}

/* TODO: opcode -> mnemonic? */
#[derive(Debug, Eq, PartialEq)]
pub enum NoOperandOpCode
{
	HLT,
	CLI,
	STI,
	XLAT,
	LAHF,
	SAHF,
	PUSHF,
	POPF,
	AAA,
	DAA,
	AAS,
	DAS,
	CBW,
	CWD,
	INTO,
	IRET,
	CLC,
	STD,
	CMC,
	STC,
	CLD,
	WAIT
}

#[derive(Debug, Eq, PartialEq)]
pub enum ImplicitOperandOpCode
{
    MOVS,
    CMPS,
    SCAS,
    LODS,
    STOS
}

#[derive(Debug, Eq, PartialEq)]
pub enum SingleOperandOpCode
{
	PUSH,
	POP,
	INC,
	DEC,
	NEG,
	MUL,
	IMUL,
	DIV,
	IDIV,
	NOT
}

#[derive(Debug, Eq, PartialEq)]
pub enum TwoOperandsOpCode
{
	ADD,
	ADC,
	MOV,
	SUB,
	SBB,
	AND,
	OR,
	XOR,
	CMP,
	TEST,
	XCHG,
	LEA,
	LDS,
	LES
}

#[derive(Debug, Eq, PartialEq)]
pub enum SingleOperandFCOpCode
{
	CALL,
	JMP
}

#[derive(Debug, Eq, PartialEq)]
pub enum NoOpFCOpCode
{
	RET
}

#[derive(Debug, Eq, PartialEq)]
pub enum SingleBImmOperandOpCode
{
	JMPS,
	JE,
	JL,
	JLE,
	JB,
	JBE,
	JP,
	JO,
	JS,
	JNE,
	JNL,
	JNLE,
	JNB,
	JNBE,
	JNP,
	JNO,
	JNS,
	LOOP,
	LOOPZ,
	LOOPNZ,
	JCXZ,
	INT,
	INB, // TODO: create one category for IN/OUT?
	INW,
	OUTB,
	OUTW,
	INVB,
	INVW,
	OUTVB,
	OUTVW
}

#[derive(Debug, Eq, PartialEq)]
pub enum ShiftRotateOpCode
{
	SHL,
	SHR,
	SAR,
	ROL,
	ROR,
	RCL,
	RCR
}

#[derive(Debug, Eq, PartialEq)]
pub enum ShiftRotateCount
{
	One,
	CL,
	Imm(u8) // REMOVEME: unsupported on a 8086
}

#[derive(Debug, Eq, PartialEq)]
pub enum SingleWImmFCOpCode
{
	RETANDADDTOSP
}

#[derive(Debug, Eq, PartialEq)]
pub enum SingleWImmFCOperand
{
	Seg(u16),
	InterSeg(u16)
}

#[derive(Debug, Eq, PartialEq)]
pub enum FlowControlOperand
{
	DirectSeg(u16), // TODO: should be DirectIntraSeg?
	IndirectSeg(WOperand),
	DirectInterSeg(u16, u16),
	IndirectInterSeg(WOperand)
}

#[derive(Debug, Eq, PartialEq)]
pub enum Prefix
{
	LOCK,
	SEGMENT(SegReg),
	REP,
	REPNE
}

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction
{
	NoOperand(NoOperandOpCode),
	ImplicitBOperand(ImplicitOperandOpCode),
	ImplicitWOperand(ImplicitOperandOpCode),
	SingleBOperand(SingleOperandOpCode, BOperand),
	SingleWOperand(SingleOperandOpCode, WOperand),
	TwoBOperands(TwoOperandsOpCode, BOperand, BOperand),
	TwoWOperands(TwoOperandsOpCode, WOperand, WOperand),
	SingleFCOperand(SingleOperandFCOpCode, FlowControlOperand),
	/* TODO: split signed (branching)/unsigned (int); will also improve disas display */
	SingleBImmOperand(SingleBImmOperandOpCode, u8),
	FCNoOperandSeg(NoOpFCOpCode),
	FCNoOperandInterSeg(NoOpFCOpCode),
	SingleWImmFCOperand(SingleWImmFCOpCode, SingleWImmFCOperand),
	ShiftRotateB(ShiftRotateOpCode, ShiftRotateCount, BOperand),
	ShiftRotateW(ShiftRotateOpCode, ShiftRotateCount, WOperand),
	Prefix(Prefix),
	Invalid
}

#[derive(Debug, Eq, PartialEq)]
pub struct SizedInstruction
{
	pub instruction: Instruction,
	pub size: u16
}

impl Display for SizedInstruction
{
	fn fmt(&self, f: &mut Formatter) -> Result
	{
		write!(f, "{}", self.instruction)
	}
}

impl Display for Instruction
{
	fn fmt(&self, f: &mut Formatter) -> Result
	{
		match *self
		{
			Instruction::NoOperand(ref op) => write!(f, "{:?}", op),
			Instruction::ImplicitBOperand(ref op) => write!(f, "{:?} BYTE", op),
			Instruction::ImplicitWOperand(ref op) => write!(f, "{:?} WORD", op),
			Instruction::SingleBOperand(ref op, ref a) => write!(f, "{:?} BYTE {}", op, a),
			Instruction::SingleWOperand(ref op, ref a) => write!(f, "{:?} WORD {}", op, a),
			Instruction::TwoBOperands(ref op, ref a, ref b) => write!(f, "{:?} BYTE {}, {}", op, a, b),
			Instruction::TwoWOperands(ref op, ref a, ref b) => write!(f, "{:?} WORD {}, {}", op, a, b),
			Instruction::SingleFCOperand(ref op, ref a) => write!(f, "{:?} {:?}", op, a),
			Instruction::SingleBImmOperand(ref op, ref a) => write!(f, "{:?} #{:x}", op, a),
			Instruction::FCNoOperandSeg(ref op) => write!(f, "{:?} NEAR", op),
			Instruction::FCNoOperandInterSeg(ref op) => write!(f, "{:?} FAR", op),
			Instruction::SingleWImmFCOperand(ref op, ref a) => write!(f, "{:?} {:?}", op, a),
			Instruction::ShiftRotateB(ref op, ref rot, ref a) => write!(f, "{:?} {:?}, {:?}", op, rot, a),
			Instruction::ShiftRotateW(ref op, ref rot, ref a) => write!(f, "{:?} {:?}, {:?}", op, rot, a),
			Instruction::Prefix(ref op) => write!(f, "{:?}", op),
			Instruction::Invalid => write!(f, "BAD")
		}
	}
}

impl Display for WOperand
{
	fn fmt(&self, f: &mut Formatter) -> Result
	{
		match *self
		{
			WOperand::Reg(ref r) => write!(f, "{:?}", r),
			WOperand::SegReg(sr) => write!(f, "{:?}", sr),
			WOperand::Indirect(ref ir) => write!(f, "[{:?}]", ir),
			WOperand::Immediate(ref imm) => write!(f, "#{:x}", imm),
			WOperand::Direct(ref addr) => write!(f, "[{:04x}]", addr),
			WOperand::Indirect8iDis(ref ir, ref dis) => write!(f, "[{:?} + {:x}]", ir, dis),
			WOperand::Indirect16uDis(ref ir, ref dis) => write!(f, "[{:?} + {:x}]", ir, dis),
		}
	}
}

impl Display for BOperand
{
	fn fmt(&self, f: &mut Formatter) -> Result
	{
		match *self
		{
			BOperand::Reg(ref r) => write!(f, "{:?}", r),
			BOperand::Indirect(ref ir) => write!(f, "[{:?}]", ir),
			BOperand::Immediate(ref imm) => write!(f, "#{:x}", imm),
			BOperand::Direct(ref addr) => write!(f, "[{:04x}]", addr),
			BOperand::Indirect8iDis(ref ir, ref dis) => write!(f, "[{:?} + {:x}]", ir, dis),
			BOperand::Indirect16uDis(ref ir, ref dis) => write!(f, "[{:?} + {:x}]", ir, dis),
		}
	}
}