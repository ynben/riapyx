extern crate sdl2;
extern crate num;

use std::collections::vec_deque::VecDeque;

use super::scancodes::*;
use super::super::cpu::CPU;
use super::super::mem::Memory;
use super::super::bios::ByteBdaEntry;

#[derive(Clone,Copy)]
pub struct Keystroke
{
	pub scancode: u8,
	pub ascii: u8
}

const RSHIFT_FLAG: u8 = 0x1;
const RSHIFT_SCANCODE: u8 = 0x36;
const LSHIFT_FLAG: u8 = 0x2;
const LSHIFT_SCANCODE: u8 = 0x2a;
const CTRL_FLAG: u8 = 0x4;
const CTRL_SCANCODE: u8 = 0x1d;
const ALT_FLAG: u8 = 0x8;
const ALT_SCANCODE: u8 = 0x38;
/*const SCROLL_FLAG: u8 = 0x5;
const SCROLL_SCANCODE: u8 = 0x7E;
const NUMLOCK_FLAG: u8 = 0x6;
const NUMLOCK_SCANCODE: u8 = 0x77;
const INSERT_FLAG: u8 = 0x7;
const INSERT_SCANCODE: u8 = 0x52;*/

pub struct Keyboard
{
	bios_queue: VecDeque<Keystroke>, // TODO: do we need a queue? It can actually never be filled with more than one element...
	io_queue: VecDeque<u8>,
	ppi_a: u8,
	enable_irq: bool, // Disabled for BIOS wait_for_keystroke
	shift_flags: ByteBdaEntry
}

impl Keystroke
{
	pub fn new(scancode: u8, ascii: u8) -> Keystroke
	{
		Keystroke { scancode: scancode, ascii: ascii }
	}
}

impl Keyboard
{
	pub fn new() -> Keyboard
	{
		Keyboard
		{
			bios_queue: VecDeque::<Keystroke>::new(),
			io_queue: VecDeque::<u8>::new(),
			ppi_a: 0,
			enable_irq: true,
			shift_flags: ByteBdaEntry::new(0x17)
		}
	}

	pub fn on_keydown(&mut self, cpu: &mut CPU, sdlk_opt: Option<sdl2::keyboard::Scancode>)
	{
		if let Some(sdlk) = sdlk_opt
		{
			if let Some(scancode) = sdlk_to_scancode(sdlk)
			{
				keyboard_print!("Pressed: scancode={:x}", scancode);
				self.io_queue.push_back(scancode);
				cpu.queue_hw_interrupt(0x9);
			}
		}
	}

	pub fn on_keyup(&mut self, cpu: &mut CPU, sdlk_opt: Option<sdl2::keyboard::Scancode>)
	{
		if let Some(sdlk) = sdlk_opt
		{
			if let Some(scancode) = sdlk_to_scancode(sdlk)
			{
				keyboard_print!("Released: scancode={:x}", scancode);
				self.io_queue.push_back(scancode | 0x80);
				cpu.queue_hw_interrupt(0x9);
			}
		}
	}

	pub fn bios_pump_keystrokes(&mut self, mem: &mut Memory)
	{
		while ! self.io_queue.is_empty()
		{
			let scancode = self.io_queue.pop_front().unwrap();
			
			let is_released = scancode & 0x80 != 0;
			let shift_flags = self.shift_flags.get(mem);
			let is_shift_pressed = shift_flags & (RSHIFT_FLAG | LSHIFT_FLAG) != 0;

			/* Shift flags handling */
			{
				let mut set_flag = |flag: u8, set: bool| // TODO: why mut?
				{
					let new_val = if set {shift_flags | flag } else { shift_flags & (flag ^ 0xff) };
					self.shift_flags.set(mem, new_val)
				};
				match scancode & 0x7f
				{
					LSHIFT_SCANCODE => set_flag(LSHIFT_FLAG, !is_released),
					RSHIFT_SCANCODE => set_flag(RSHIFT_FLAG, !is_released),
					ALT_SCANCODE => set_flag(ALT_FLAG, !is_released),
					CTRL_SCANCODE => set_flag(CTRL_FLAG, !is_released),
					_ => ()
				}
			}

			// Only consider the 'pressed' event
			if is_released
			{
				continue
			}

			let ascii = 
				match scancode_to_ascii(scancode, is_shift_pressed)
				{
					Some(s) => s,
					None => 0
				};

			self.bios_queue.push_back(Keystroke::new(scancode, ascii));
		}
	}

	pub fn try_pop_keystroke(&mut self) -> Option<Keystroke>
	{
		/* We may encounter scancodes that cannot be used by the machine, so 
		 * always check that we got something and loop if not */
		self.bios_queue.pop_front()
	}

	pub fn check_keystroke(&mut self) -> Option<Keystroke>
	{
		match self.bios_queue.front()
		{
			Some(k) => Some(*k),
			None => None
		}
	}

	pub fn io_get_scancode(&mut self) -> Option<u8>
	{
		keyboard_print!("IO: Get scancode");

		match self.io_queue.front()
		{
			Some(k) => 
			{
				keyboard_print!("Scancode {:02x}", k);
				Some(*k)
			}
			None => None
		}	
	}

	pub fn set_ppi_a(&mut self, ppi_a: u8)
	{
		keyboard_print!("Set ppi_a to {:x}", ppi_a);

		if ((self.ppi_a & 0x80) != 0) && ((ppi_a & 0x80) == 0)
		{
			// Keyboard was disabled and has been enabled: drop one entry from the IO queue
			assert!(! self.io_queue.is_empty());

			self.io_queue.pop_front();
		}

		self.ppi_a = ppi_a;
	}

	pub fn get_ppi_a(&self) -> u8
	{
		self.ppi_a
	}

	pub fn get_shift_flags(&self, mem: &Memory) -> u8
	{
		self.shift_flags.get(mem)
	}

	pub fn set_irq(&mut self, enabled: bool)
	{
		self.enable_irq = enabled;
	}
}
