use super::base::*;
use super::super::hw::HW;

impl CPU
{
	pub fn io_inb(&mut self, port: u16, hw: &mut HW) -> u8
	{
		match port
		{
			0x60 =>
			{
				match hw.keyboard.io_get_scancode()
				{
					Some(scancode) => scancode,
					None => 0x0
				}
			}
			0x61 => hw.keyboard.get_ppi_a(),
			0x3da =>
			{
				hw.display.get_status_reg()
			}
			0x3f8 => hw.com1.read_rtd(),
			0x3f9 => hw.com1.read_ier(),
			0x3fa => hw.com1.read_iir(),
			0x3fb => hw.com1.read_lc(),
			0x3fc => hw.com1.read_mc(),
			0x3fd => hw.com1.read_lsr(),
			_ =>
			{
				cpu_print!("Warning: byte input from unknown IO port {:04x}", port);
				0x0
			}
		}
	}

	pub fn io_inw(&mut self, port: u16, _hw: &mut HW) -> u16
	{
		match port
		{
			_ =>
			{
				cpu_print!("Warning: word input from unknown IO port {:04x}", port);
				0x0
			}
		}
	}

	pub fn io_outb(&mut self, port: u16, value: u8, hw: &mut HW)
	{
		match port
		{
			0x61 => hw.keyboard.set_ppi_a(value),
			0x3f8 => hw.com1.write_rtd(value),
			0x3f9 => hw.com1.write_ier(value),
			0x3fb => hw.com1.write_lc(value),
			0x3fc => hw.com1.write_mc(value),
			_ =>
			{
				cpu_print!("Warning: byte output {:04x} to unknown IO port {:04x}", value, port);
			}
		}
	}

	pub fn io_outw(&mut self, port: u16, _value: u16, _hw: &mut HW)
	{
		match port
		{
			_ =>
			{
				cpu_print!("Warning: word output {:04x} to unknown IO port {:04x}", _value, port);
			}
		}
	}
}