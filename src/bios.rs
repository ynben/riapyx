use cpu::*;
use mem::Memory;
use hw::HW;
use hw::storage;
use hw::display;

use std::io::prelude::*;

pub struct ByteBdaEntry
{
	idx: u8
}

impl ByteBdaEntry
{
	pub fn new(idx: u8) -> ByteBdaEntry
	{ 
		ByteBdaEntry { idx: idx } 
	}
	
	pub fn get(&self, mem: &Memory) -> u8
	{
		mem.read_u8(0x400 + self.idx as u32)
	}

	pub fn set(&self, mem: &mut Memory, val: u8)
	{
		mem.write_u8(0x400 + self.idx as u32, val)
	}
}

pub struct WordBdaEntry
{
	idx: u8
}

impl WordBdaEntry
{
	pub fn new(idx: u8) -> WordBdaEntry
	{ 
		WordBdaEntry { idx: idx } 
	}
	
	/*
	pub fn get(&self, mem: &Memory) -> u16
	{
		mem.read_u16(0x400 + self.idx as u32)
	}*/

	pub fn set(&self, mem: &mut Memory, val: u16)
	{
		mem.write_u16(0x400 + self.idx as u32, val)
	}
}

/* The BIOS should be mostly stateless and simply dispatch calls to the hardware
 * In the BIOS call implementations in hardware emulation code, the state should
 * reside in the emulated memory through the BDA. TODO: actually do this */

#[derive(Eq, PartialEq)]
pub enum BIOSState
{
	Ok,
	Crashed
}

pub enum BootDrive
{
	Floppy,
	HardDrive
}

pub struct BIOS
{
	pub state: BIOSState,
	boot_drive: BootDrive
}

const BIOS_SEGMENT: u16 = 0xf000;
const ROM_CONF_TABLE_ADDR: u16 = 0xE6F5;
const EQUIPMENT_WORD: u16 = 0x21;
const EQUIPMENT_WORD_ADDR: u32 = 0x410;

impl BIOS
{
	pub fn new(boot_drive: BootDrive) -> BIOS
	{
		BIOS
		{
			state: BIOSState::Ok,
			boot_drive: boot_drive
		}
	}

	pub fn cpu_trap(&mut self, cpu: &mut CPU, mem: &mut Memory, hw: &mut HW)
	{
		let ip = cpu.get_reg(WReg::IP);
		/* IP = 0xFFF0 for boot, interrupt number otherwise */

		match ip
		{
			0xfff0 => self.boot(cpu, mem, hw),
			0x0 ... 0xff => self.handle_interrupt(cpu, mem, hw, ip as u8),
			_ => panic!("Invalid IP value for BIOS call: {}", ip)
		}
	}

	fn handle_interrupt(&mut self, cpu: &mut CPU, mem: &mut Memory, hw: &mut HW, interrupt_number: u8)
	{
		match interrupt_number
		{
			0x0 =>
			{
				/* CPU exception */
				bios_print!("Unhandled CPU exception");
				self.state = BIOSState::Crashed;
			}
			0x8 =>
			{}
			0x9 =>
			{
				bios_print!("Keyboard HW interrupt");
				hw.keyboard.bios_pump_keystrokes(mem);
			}
			0x10 =>
			{
				/* Video services */
				// TODO: move logs to display
				let ah = cpu.get_reg(BReg::AH);
				match ah
				{
					0x0 =>
					{
						let mode = cpu.get_reg(BReg::AL);
						bios_print!("Set video mode {}", mode);
						hw.display.set_mode(mem, display::GraphicMode::from_bios(mode))
					}
					0x1 =>
					{
						bios_print!("UNIMPLEMENTED: Set cursor shape")
					}
					0x2 =>
					{
						let page = cpu.get_reg(BReg::BH);
						let y = cpu.get_reg(BReg::DH);
						let x = cpu.get_reg(BReg::DL);
						bios_print!("Set cursor coords for page {}: {} {}", page, x, y);
						hw.display.tty_setcoords(mem, page, x, y);
						mem.write_u8(0x450, x);
						mem.write_u8(0x451, y);
					}
					0x3 =>
					{
						let mut page = cpu.get_reg(BReg::BH);
						bios_print!("Get cursor position and size, page={}", page);
						if page > 8
						{
							bios_print!("Warning: invalid page value; using current page instead");
							page = hw.display.cur_page();
						}
						let (x, y) = hw.display.tty_coords(mem, page);
						cpu.set_reg(BReg::CH, y * 8); // Start scan line
						cpu.set_reg(BReg::CL, y * 8 + 7); // End scan line
						cpu.set_reg(BReg::DH, y); // Row
						cpu.set_reg(BReg::DL, x); // Col
					}
					0x5 =>
					{
						let page = cpu.get_reg(BReg::AL);
						bios_print!("Select page {}", page);
						hw.display.set_page(page);
					}
					0x6 =>
					{
						let cnt = match cpu.get_reg(BReg::AL)
						{
							0 => 25,
							x => x
						};

						let page = hw.display.cur_page();
						let y1 = cpu.get_reg(BReg::CH);
						let x1 = cpu.get_reg(BReg::CL);
						let y2 = cpu.get_reg(BReg::DH);
						let x2 = cpu.get_reg(BReg::DL);
						let attr = cpu.get_reg(BReg::BH);
						bios_print!("Scroll, page={}, cnt={}; {}/{} -> {}/{}", page, cnt, x1, y1, x2, y2);
						for _ in 0 .. cnt
						{
							hw.display.tty_scroll(mem, page, x1, y1, x2, y2, attr);
						}
					}
					0x8 =>
					{
						let page = cpu.get_reg(BReg::BH);
						bios_print!("Read char at cursor, page={}", page);
						let (chr, attr) = hw.display.tty_read_at_cur(mem, page);
						cpu.set_reg(BReg::AH, attr);
						cpu.set_reg(BReg::AL, chr);
					}
					0x9 =>
					{
						bios_print!("Write char+attr at pos");
						let chr = cpu.get_reg(BReg::AL);
						let attr = cpu.get_reg(BReg::BL);
						let page = cpu.get_reg(BReg::BH);
						let cnt = cpu.get_reg(WReg::CX);
						hw.display.write_char_at_cur(mem, page, chr, attr, cnt);
					}
					0x0e =>
					{
						let char_to_print = cpu.get_reg(BReg::AL);
						let page = cpu.get_reg(BReg::BH);
						let foreground_color = cpu.get_reg(BReg::BL);
						bios_print!("tty output to page {}; chr={:02x}", page, char_to_print);
						hw.display.tty_output(mem, page, char_to_print, foreground_color)
					},
					0xf =>
					{
						let mode = hw.display.get_mode().to_bios();
						let page = hw.display.cur_page();
						let cols = hw.display.get_mode().cols() as u8;
						bios_print!("Get video mode: page={}, mode={}, cols={}", page, mode, cols);
						cpu.set_reg(BReg::AH, cols);
						cpu.set_reg(BReg::AL, mode); // Video mode
						cpu.set_reg(BReg::BH, page);
					},
					0xb => 
					{
						bios_print!("IMPLEMENT ME: AH={:02x}", ah);
					}
					0x11 | 0x1a | 0xef | 0xfa | 0x10 | 0xf0 | 0x30 | 0x6f | 0xfe | 0xcc =>
					{
						bios_print!("Not implemented: video service; AX={:04x}", cpu.get_reg(WReg::AX));
						self.set_carry(cpu, mem);
					}
					0x12 =>
					{
						assert!(cpu.get_reg(BReg::BL) == 0x10);
						bios_print!("BAD IMPL: get ega info, count={}, bl={}", cpu.get_reg(BReg::AL), cpu.get_reg(BReg::BL));
					}
					0x1b =>
					{
						bios_print!("Video: functionality/state information (not supported)");
						cpu.set_reg(BReg::AL, 0); // Not 1B = unsupported
						self.set_carry(cpu, mem);
					}
					_ => panic!("Unhandled video service (int 0x10): {:x}", ah)
				}
			}
			0x11 =>
			{
				/* Get equipment list
				 * Just return what we have: 80x25 color, a floppy disk and that's all */
				bios_print!("Get equipment list");
				let equipment_list_word = mem.read_u16(EQUIPMENT_WORD_ADDR);
				cpu.set_reg(WReg::AX, equipment_list_word);
			}
			0x12 =>
			{
				/* Get memory size */
				let memory_size_kb: u16 = 639;
				bios_print!("Get memory size = 0x{:x}kB", memory_size_kb);
				cpu.set_reg(WReg::AX, memory_size_kb);
				self.clear_carry(cpu, mem);
			}
			0x13 =>
			{
				/* Disk services */
				let ah = cpu.get_reg(BReg::AH);
				let drive = cpu.get_reg(BReg::DL);

				let target_storage = match drive
				{
					0x0 => hw.floppy.as_mut(),
					0x80 => hw.hdd.as_mut(),
					_ => None
				};

				match target_storage
				{
					None =>
					match ah
					{
						0x15 => // Get disk type
						{ cpu.set_reg(BReg::AH, 0x0); /* No such drive */ }
						_ => self.set_carry(cpu, mem)
					},
					Some(storage) =>
					match ah
					{
						0x0 => 
						{
							let status = storage.reset(mem);
							cpu.set_reg(BReg::AH, status.get_bios_code());
							self.set_carry_value(cpu, mem, status != storage::Status::Success);
						}
						0x2 =>
						{
							let sector_count = cpu.get_reg(BReg::AL);
							let cx = cpu.get_reg(WReg::CX);
							let cylinder = ((cx & 0xff00)>>8) + ((cx & 0x00c0) << 2);
							let sector = (cx & 0x003f) as u8;
							let head = cpu.get_reg(BReg::DH);
							let data_seg = cpu.get_reg(SegReg::ES);
							let data_addr = cpu.get_reg(WReg::BX);
							let (status, read) = 
								storage.read_chs(mem, sector_count, cylinder, head, sector, data_seg, data_addr);
							cpu.set_reg(BReg::AH, status.get_bios_code());
							cpu.set_reg(BReg::AL, read);
							self.set_carry_value(cpu, mem, status != storage::Status::Success);

						}
						0x3 =>
						{
							let sector_count = cpu.get_reg(BReg::AL);
							let cx = cpu.get_reg(WReg::CX);
							let cylinder = ((cx & 0xff00)>>8) + ((cx & 0x00c0) << 2);
							let sector = (cx & 0x003f) as u8;
							let head = cpu.get_reg(BReg::DH);
							let data_seg = cpu.get_reg(SegReg::ES);
							let data_addr = cpu.get_reg(WReg::BX);
							let (status, written) = 
								storage.write_chs(mem, sector_count, cylinder, head, sector, data_seg, data_addr);
							cpu.set_reg(BReg::AH, status.get_bios_code());
							cpu.set_reg(BReg::AL, written);
							self.set_carry_value(cpu, mem, status != storage::Status::Success);
						}
						0x8 =>
						{
							/* Get drive parameters */
							bios_print!("Get drive parameters for drive 0x{:02x}", drive);

							cpu.set_reg(BReg::DL, 0x1); // Drive count

							let drive_type = match drive
							{
								0 => 4, // 1.44M floppy
								0x80 => 0,
								_ => unreachable!()
							};

							let cyls = storage.parameters.cylinders() as u16;
							let heads_min1 = (storage.parameters.heads() - 1) as u8;
							let sectors_per_track = storage.parameters.sectors_per_track() as u8;

							cpu.set_reg(WReg::AX, 0);
							cpu.set_reg(BReg::BL, drive_type);
							cpu.set_reg(BReg::CH, cyls as u8);
							cpu.set_reg(BReg::CL, sectors_per_track | (((cyls >> 8) as u8) << 6));
							cpu.set_reg(BReg::DH, heads_min1);
							self.clear_carry(cpu, mem);
						}
						0x15 =>
						{
							/* Get disk type */
							bios_print!("Get disk type for drive {}", drive);
							match drive
							{
								0 => cpu.set_reg(BReg::AH, 2), // Floppy with change line support
								0x80 => 
								{
									cpu.set_reg(BReg::AH, 3); // Hard drive
									// Need to return sector count for hard drives
									let cyls = storage.parameters.cylinders();
									let heads = storage.parameters.heads();
									let sectors_per_track = storage.parameters.sectors_per_track();
									let sector_count = (cyls as u32) * (heads as u32) * (sectors_per_track as u32);
									cpu.set_reg(WReg::CX, (sector_count >> 16) as u16);
									cpu.set_reg(WReg::DX, sector_count as u16);
								}
								_ => unreachable!()
							};

							self.clear_carry(cpu, mem);
						}
						0x16 =>
						{
							/* Disk change is not supported */
							bios_print!("Detect disk change for disk 0x{:02x}", drive);
							self.clear_carry(cpu, mem);
							cpu.set_reg(BReg::AH, 0);
						}
						_ => panic!("Unhandled disk service (int 0x13): {:x}", ah)
					}
				}				
			}
			0x14 =>
			{
				/* Serial */
				let ah = cpu.get_reg(BReg::AH);
				match ah
				{
					0x0 =>
					{
						/* Initialize port */
						bios_print!("Serial port initialize (?)");
						cpu.set_reg(BReg::AH, 0x80); // timeout
					}
					_ => panic!("Unhandled serial service: {:x}", ah)
				}
			}
			0x15 =>
			{
				let ah = cpu.get_reg(BReg::AH);
				match ah
				{
					0x41 =>
					{
						bios_print!("BAD IMPLEMENTATION: wait on external event");
					}
					0x88 => 
					{
						bios_print!("Get extended memory size");
						/* This is not a 286+ machine */
						self.set_carry(cpu, mem);
						cpu.set_reg(BReg::AH, 0x86); // Unsupported function
					}
					0xC0 =>
					{
						/* Get configuration */
						bios_print!("Get configuration");

						cpu.set_reg(WReg::AX, 0);
						cpu.set_reg(WReg::BX, ROM_CONF_TABLE_ADDR);
						cpu.set_reg(SegReg::ES, BIOS_SEGMENT);
						self.clear_carry(cpu, mem);
					}
					0xC1 =>
					{
						bios_print!("Get EBDA address (unsupported)");
						self.set_carry(cpu, mem);
					}
					_ => panic!("Unhandled 0x15 irq (ah={:x})", ah)
				}
			}
			0x16 =>
			{
				/* Keyboard services */
				let ah = cpu.get_reg(BReg::AH);
				match ah
				{
					0x0 =>
					{
						bios_print!("Wait for keystroke");
						/* Disable keyboard interrupt as, if we pump a scancode, 
						 * we will use it immediately and pop it from the IO queue*/ 
						hw.keyboard.set_irq(false);
						loop
						{
							hw.keyboard.bios_pump_keystrokes(mem);
							if let Some(keystroke) = hw.keyboard.try_pop_keystroke()
							{
								cpu.set_reg(BReg::AH, keystroke.scancode);
								cpu.set_reg(BReg::AL, keystroke.ascii);
								break;
							}
							hw.wait_for_event(cpu);
						}
						hw.keyboard.set_irq(true);
					}
					0x1 =>
					{
						// bios_print!("Check for keystroke");
						match hw.keyboard.check_keystroke()
						{
							None => self.set_flag_value(cpu, mem, FLAG_Z, true),
							Some(keystroke) =>
							{
								bios_print!("Check for keystroke and got a keystroke");
								self.set_flag_value(cpu, mem, FLAG_Z, false);
								cpu.set_reg(BReg::AH, keystroke.scancode); // BIOS scancode
								cpu.set_reg(BReg::AL, keystroke.ascii);    // ASCII scancode
							}
						}
					}
					0x2 =>
					{
						// bios_print!("Get shift flags");
						cpu.set_reg(BReg::AL, hw.keyboard.get_shift_flags(mem));
					}
					0x92 | 0x55 | 0x03 | 0xff =>
					{
						bios_print!("Unimplemented keyboard service: AX={:04x}", cpu.get_reg(WReg::AX));
						self.set_carry(cpu, mem);
					}
					_ => panic!("Unhandled keyboard service: {:x}", ah)
				}
			}
			0x17 =>
			{
				/* Printer services */
				let ah = cpu.get_reg(BReg::AH);
				match ah
				{
					0x1 => 
					{
						/* Initialize port */
						bios_print!("Initialize printer?");
						cpu.set_reg(BReg::AH, 0b00001000); // IO error
						self.set_carry(cpu, mem);				
					}
					_ => panic!("Unhandled printer service: {:x}", ah)
				}
			}
			0x1a =>
			{
				/* Time services */
				let ah = cpu.get_reg(BReg::AH);
				match ah
				{
					0x0 =>
					{
						/* Get system time */
						let lo = mem.read_u16(0x46c);
						let hi = mem.read_u16(0x46e);
						let mf = mem.read_u8(0x470);
						if mf != 0
						{
							mem.write_u8(0x470, 0);
						}
						bios_print!("Get system time - {:04x}{:04x}", hi, lo);
						cpu.set_reg(BReg::AL, mf);
						cpu.set_reg(WReg::CX, hi);
						cpu.set_reg(WReg::DX, lo);
					}
					0x1 =>
					{
						/* Set system time */
						bios_print!("UNIMPLEMENTED: Set system time")
					}
					0x2 =>
					{
						/* Get real-time time */
						bios_print!("BAD IMPLEMENTATION: Get real-time clock time");
						self.clear_carry(cpu, mem);
						cpu.set_reg(BReg::CH, 0); // Hours
						cpu.set_reg(BReg::CL, 0); // Minutes
						cpu.set_reg(BReg::DH, 0); // Seconds
						cpu.set_reg(BReg::DL, 0); // Daylight saving time flag
					}
					0x4 =>
					{
						bios_print!("Unimplemented time service; ah = {:x}", ah);
						self.set_carry(cpu, mem);
					}
					_ => panic!("Unhandled time service: {:x}", ah)
				}
			}
			_ => panic!("Unhandled interrupt: 0x{:x} (ah={:x})", interrupt_number, cpu.get_reg(BReg::AH))
		}
	}

	fn boot(&mut self, cpu: &mut CPU, mem: &mut Memory, hw: &mut HW)
	{
		bios_print!("Boot");

		self.init_ivt(mem);
		
		{
			let mut storage_init = 
				|storage_opt: &mut Option<storage::Storage>|
				{
					storage_opt.as_mut().map(|storage|
					{
						let (irq, seg, addr) = storage.init(mem);
						self.write_ivt_entry(mem, irq, seg, addr);
					});
				};

			storage_init(&mut hw.floppy);
			storage_init(&mut hw.hdd);
		}

		self.init_romconf(mem);

		bios_print!("Loading MBR... ");
		let (boot_storage, bios_drive) = 
			match self.boot_drive
			{
				BootDrive::Floppy => (hw.floppy.as_mut().expect("Floppy disk image not specified"), 0x0),
				BootDrive::HardDrive => (hw.hdd.as_mut().expect("Hard drive image not specified"), 0x80)
			};
		cpu.set_reg(BReg::DL, bios_drive); // DL should contain the BIOS id of the boot drive

		let (result, read) = boot_storage.read(mem, 0x0, 0x7c00, 512);
		if result != storage::Status::Success || read != 512
		{
			panic!("Unable to read MBR");
		}

		/* Write a "jmp far 0:0x7c00" instruction at our current location, 
		 * which should be 0xf000:0xfff0 */
		let far_jmp_7c00: [u8; 5] = [0xea, 0x00, 0x7c, 0x00, 0x00];
		for i in 0..far_jmp_7c00.len()
		{
			mem.write_u8(phys_addr(BIOS_SEGMENT, 0xfff0 + i as u16), far_jmp_7c00[i]);
		}
		
		// Sets 80x25 display mode
		hw.display.set_mode(mem, display::GraphicMode::T8025);

		bios_print!("All done; now running the MBR")
	}

	fn write_ivt_entry(&self, mem: &mut Memory, number: u8, seg: u16, addr: u16)
	{
		const IVT_OFFSET: u32 = 0;
		mem.write_u16(IVT_OFFSET + (number as u32) * 4, addr);
		mem.write_u16(IVT_OFFSET + (number as u32) * 4 + 2, seg);
	}

	/*fn read_ivt_entry(&self, mem: &Memory, number: u8) -> (u16, u16)
	{
		const IVT_OFFSET: u32 = 0;
		(
			mem.read_u16(IVT_OFFSET + (number as u32) * 4 + 2), // Segment
			mem.read_u16(IVT_OFFSET + (number as u32) * 4) // Address
		)
	}*/

	fn init_ivt(&mut self, mem: &mut Memory)
	{
		const IRET: u8 = 0b11001111;

		for irq in 0..0xff
		{
			/* CS = BIOS_SEGMENT; ip = #IRQ */
			self.write_ivt_entry(mem, irq, BIOS_SEGMENT, irq as u16);
			mem.write_u8(phys_addr(BIOS_SEGMENT, irq as u16), IRET); // Interrupt handler in ROM
		}
	}

	
	fn init_romconf(&self, mem: &mut Memory)
	{
		bios_print!("Setting rom configuration...");
		mem.write_u16(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR + 0), 8); // Table size - this entry (2bytes)
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  2), 0xFC); // Model: Linux DOSEMU (should be fine?)
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  3), 0); // Submodel
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  4), 0); // BIOS revision
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  5), 0b00000000); // Feature byte 1
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  6), 0b00000000); // Feature byte 2
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  7), 0b00000000); // Feature byte 3
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  8), 0b00000000); // Feature byte 4
		mem.write_u8(phys_addr(BIOS_SEGMENT, ROM_CONF_TABLE_ADDR +  9), 0b00000000); // Feature byte 5

		mem.write_u16(EQUIPMENT_WORD_ADDR, EQUIPMENT_WORD); // TODO: phys_addr + BDA_SEG?
	}

	/* TODO: should be moved out of impl? */
	fn set_flag_value(&self, cpu: &CPU, mem: &mut Memory, flag_mask: u16, set: bool)
	{
		/* We cannot change the CPU flags directly as they will be restored
		 * by IRET. However, we can change the flags we saved on the stack, 
		 * which we do */

		let flags_addr = phys_addr(cpu.get_reg(SegReg::SS), cpu.get_reg(WReg::SP) + 4);
		let mut flags = mem.read_u16(flags_addr);
		if set
		{
			flags = flags | flag_mask;
		}
		else 
		{
			flags = flags & not(flag_mask);
		}
		mem.write_u16(flags_addr, flags);
	}

	fn set_carry_value(&self, cpu: &CPU, mem: &mut Memory, set: bool)
	{
		self.set_flag_value(cpu, mem, FLAG_C, set);
	}

	fn set_carry(&self, cpu: &CPU, mem: &mut Memory)
	{
		self.set_carry_value(cpu, mem, true);
	}

	fn clear_carry(&self, cpu: &CPU, mem: &mut Memory)
	{
		self.set_carry_value(cpu, mem, false);
	}
}