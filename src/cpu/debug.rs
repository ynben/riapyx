use super::base::*;

impl CPU
{
	pub fn dump(&self)
	{
		cpu_print!("AX: {:04x} BX: {:04x} CX: {:04x} DX: {:04x}", self.ax, self.bx, self.cx, self.dx);
		cpu_print!("SP: {:04x} BP: {:04x} SI: {:04x} DI: {:04x}", self.sp, self.bp, self.si, self.di);
		cpu_print!("CS: {:04x} DS: {:04x} SS: {:04x} ES: {:04x}", self.cs, self.ds, self.ss, self.es);
		cpu_print!("IP: {:04x} Flags: {:016b}", self.ip, self.flags);
		if self.segment_override_prefix != None
		{
			cpu_print!("Segment override: {:?}", self.segment_override_prefix);
		}
		if self.rep_prefix != None
		{
			cpu_print!("Rep prefix: {:?}", self.rep_prefix);
		}
	}
}