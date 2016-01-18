use mem::Memory;
use cpu::*;
use std::io::SeekFrom;
use std::io::prelude::*;
use std::fs::File;
use std::fs::OpenOptions;

const DISKETTE_PARAMETERS_IRQ: u8 = 0x1E;
const HDD_PARAMETERS_IRQ: u8 = 0x41;

pub struct DisketteParameters
{
	/* See INTERRUP.E for more details */
	step_rate_hi_head_unload_time_lo: u8,
	head_load_time_hi7b_non_dma_lo: u8,
	delay_until_motor_off: u8,
	bytes_per_sector: u8,
	sectors_per_track: u8,
	gap_len_between_sectors: u8,
	data_length: u8,
	gap_length: u8,
	format_filler: u8,
	head_settle_time: u8,
	motor_start_time: u8
}

pub struct HardDiskParameters
{
	cylinders: u16,
	heads: u8,
	control_byte: u8,
	landing_zone_cylinder: u16,
	sectors_per_track: u8
}

pub enum DriveParameters
{
	Floppy(DisketteParameters),
	HardDisk(HardDiskParameters)
}
pub struct Storage
{
	file: File,
	pub parameters: DriveParameters
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Status
{
	Success,
	//InvalidParam,
	SectorNotFound,
}

impl DisketteParameters
{
	fn store(&self, mem: &mut Memory, addr: u32)
	{
		mem.write_u8(addr + 0x0, self.step_rate_hi_head_unload_time_lo);
		mem.write_u8(addr + 0x1, self.head_load_time_hi7b_non_dma_lo);
		mem.write_u8(addr + 0x2, self.delay_until_motor_off);
		mem.write_u8(addr + 0x3, self.bytes_per_sector);
		mem.write_u8(addr + 0x4, self.sectors_per_track);
		mem.write_u8(addr + 0x5, self.gap_len_between_sectors);
		mem.write_u8(addr + 0x6, self.data_length);
		mem.write_u8(addr + 0x7, self.gap_length);
		mem.write_u8(addr + 0x8, self.format_filler);
		mem.write_u8(addr + 0x9, self.head_settle_time);
		mem.write_u8(addr + 0xA, self.motor_start_time);

		storage_print!("Floppy parameters set");
	}

	fn load(&mut self, mem: &Memory, addr: u32)
	{
		// TODO: reloading params from the memory prevents DOS 3 from booting because of improper floppy params
		self.step_rate_hi_head_unload_time_lo = mem.read_u8(addr + 0x0);
		self.head_load_time_hi7b_non_dma_lo = mem.read_u8(addr + 0x1);
		self.delay_until_motor_off = mem.read_u8(addr + 0x2);
		self.bytes_per_sector = mem.read_u8(addr + 0x3);
		self.sectors_per_track = mem.read_u8(addr + 0x4);
		self.gap_len_between_sectors = mem.read_u8(addr + 0x5);
		self.data_length = mem.read_u8(addr + 0x6);
		self.gap_length = mem.read_u8(addr + 0x7);
		self.format_filler = mem.read_u8(addr + 0x8);
		self.head_settle_time = mem.read_u8(addr + 0x9);
		self.motor_start_time = mem.read_u8(addr + 0xA);

		storage_print!("Diskette parameters loaded")
	}

	pub fn floppy_1_44mb() -> DisketteParameters
	{
		DisketteParameters
		{
			step_rate_hi_head_unload_time_lo: 0xff,
			head_load_time_hi7b_non_dma_lo: 0x02,
			delay_until_motor_off: 0,
			bytes_per_sector: 0x2,
			sectors_per_track: 0x12,
			gap_len_between_sectors: 0x1B,
			data_length: 0x0,
			gap_length: 0x6c,
			format_filler: 0xf6,
			head_settle_time: 0x0,
			motor_start_time: 0x0
		}
	}
}

impl HardDiskParameters
{
	fn new(cylinders: u16, heads: u8, sectors_per_track: u8) -> HardDiskParameters
	{
		// Required for control byte
		assert!(heads > 8);

		HardDiskParameters
		{
			cylinders: cylinders,
			heads: heads,
			control_byte: 0xc8, // No access retry, ECC retry, more than 8 heads
			landing_zone_cylinder: cylinders, // Seems to be what qemu does...
			sectors_per_track: sectors_per_track
		}
	}

	fn load(&mut self, mem: &Memory, addr: u32)
	{
		self.cylinders = mem.read_u16(addr + 0x0);
		self.heads = mem.read_u8(addr + 0x2);
		self.control_byte = mem.read_u8(addr + 0x8);
		self.landing_zone_cylinder = mem.read_u16(addr + 0xc);
		self.sectors_per_track = mem.read_u8(addr + 0xe);
	}

	fn store(&self, mem: &mut Memory, addr: u32)
	{
		storage_print!("Hard disk parameters set");
		mem.write_u16(addr + 0x0, self.cylinders);
		mem.write_u8 (addr + 0x2, self.heads);
		mem.write_u8 (addr + 0x8, self.control_byte);
		mem.write_u16(addr + 0xc, self.landing_zone_cylinder);
		mem.write_u8 (addr + 0xe, self.sectors_per_track);
		mem.write_u8 (addr + 0xf, 0);
	}
}


// TODO: just use a trait?
impl DriveParameters
{
	fn params_addr(&self, mem: &Memory) -> u32
	{
		let irq = match self
		{
			&DriveParameters::Floppy(_) => DISKETTE_PARAMETERS_IRQ,
			&DriveParameters::HardDisk(_) => HDD_PARAMETERS_IRQ,
		} as u32;

		// TODO: have a helper 'read_ivt_entry'
		let addr = mem.read_u16(irq * 4);
		let seg = mem.read_u16(irq * 4 + 2);
		phys_addr(seg, addr)
	}

	fn load(&mut self, mem: &mut Memory)
	{
		let params_addr = self.params_addr(mem);
		
		match self
		{
			&mut DriveParameters::Floppy(ref mut diskette_params) => diskette_params.load(mem, params_addr),
			&mut DriveParameters::HardDisk(ref mut hdd_params) => hdd_params.load(mem, params_addr)
		}

		storage_print!("Cylinders: {}", self.cylinders());
		storage_print!("Heads: {}", self.heads());
		storage_print!("Sectors per track: {}", self.sectors_per_track())
	}

	/*fn store(&self, mem: &mut Memory)
	{
		let params_addr = self.params_addr(mem);
		
		match self
		{
			&DriveParameters::Floppy(ref diskette_params) => diskette_params.store(mem, params_addr),
			&DriveParameters::HardDisk(ref hdd_params) => hdd_params.store(mem, params_addr)
		}		
	}*/

	pub fn heads(&self) -> u32
	{
		match self
		{
			&DriveParameters::Floppy(_) => 2,
			&DriveParameters::HardDisk(ref hdd_params) => hdd_params.heads as u32
		}
	}

	pub fn sectors_per_track(&self) -> u32
	{
		match self
		{
			&DriveParameters::Floppy(ref diskette_params) => diskette_params.sectors_per_track as u32,
			&DriveParameters::HardDisk(ref hdd_params) => hdd_params.sectors_per_track as u32
		}
	}

	pub fn cylinders(&self) -> u32
	{
		match self
		{
			&DriveParameters::Floppy(_) => 80,
			&DriveParameters::HardDisk(ref hdd_params) => hdd_params.cylinders as u32
		}	
	}
}

impl Storage
{
	pub fn new_floppy(filename: &str) -> Storage
	{
		Storage
		{
			file: OpenOptions::new().read(true).write(true).open(filename).unwrap(),
			parameters: DriveParameters::Floppy(DisketteParameters::floppy_1_44mb())
		}
	}

	pub fn new_hdd(filename: &str) -> Storage
	{
		let hdd_file = OpenOptions::new().read(true).write(true).open(filename).unwrap();
		let hdd_size = hdd_file.metadata().unwrap().len();
		let sectors_per_track: u8 = 63;
		let heads: u8 = 16;
		let cylinders: u16 = (hdd_size / (sectors_per_track as u64) / (heads as u64) / 512) as u16;
		storage_print!("Hard drive: {}MB; C/H/S = {}/{}/{}", hdd_size / 1024 / 1024, cylinders, heads, sectors_per_track);
		Storage
		{
			file: hdd_file,
			parameters: DriveParameters::HardDisk(HardDiskParameters::new(cylinders, heads, sectors_per_track))
		}
	}

	pub fn reset(&mut self, mem: &mut Memory) -> Status
	{
		self.parameters.load(mem);
		Status::Success
	}

	/* -> (status, sectors_read) */
	pub fn read_chs(&mut self, mem: &mut Memory, sector_count: u8, cylinder: u16, head: u8, sector: u8, data_seg: u16, data_addr: u16)
		-> (Status, u8)
	{
		// TODO: WARNING: how should we handle wrapping around in a segment?
		// Seems the BIOS relies on a DMA, which may not take segmentation into account
		// (but is affected by 64k boundaries...)

		if head as u32 >= self.parameters.heads() || sector as u32 > self.parameters.sectors_per_track() || sector == 0 || cylinder as u32 >= self.parameters.cylinders()
		{
			storage_print!("Warning: attempted to read from invalid CHS {}/{}/{}", cylinder, head, sector);
			return (Status::SectorNotFound,0)
		}

		let lba = 
			(
				(cylinder as u32) * self.parameters.heads() +
				(head as u32)
			) * self.parameters.sectors_per_track() +
			((sector - 1) as u32);

		storage_print!("Read {} bytes from LBA {} (pos {}) (CHS {},{},{}) to {:04x}:{:04x}",
			(sector_count as u32) * 512, lba, lba * 512, cylinder, head, sector, data_seg, data_addr);
		let (status, read) = self.read(mem, lba * 512, phys_addr(data_seg, data_addr), (sector_count as u32) * 512);
		(status, (read / 512) as u8)
	}

	/* -> (Status, bytes_read) */
	/* TODO: use standard Rust results; BIOS statuses should only be used for read_chs */
	pub fn read(&mut self, mem: &mut Memory, from: u32, to: u32, len: u32) -> (Status, u32)
	{
		let pos = self.file.seek(SeekFrom::Start(from as u64));
		if pos.unwrap() != from as u64
		{
			storage_print!("Unable to seek file to {}", from);
			return (Status::SectorNotFound, 0)
		}

		let mut buf = vec![0; len as usize].into_boxed_slice();

		match self.file.read(&mut buf)
		{
			Ok(read) =>
			{
				assert!(read <= len as usize);

				if read < (len as usize)
				{
					panic!("Unable to read enough data from file: expected {}, got {}", len, read);
					/* TODO: should we use a "Success" status in such case? */
				}

				for i in 0..(read as u32)
				{
					mem.write_u8(to + i, buf[i as usize]);
				}
				(Status::Success, read as u32)
			},
			Err(e) => panic!("Error while reading file: {}", e)
		}
	}

	/* TODO: factor with read_chs; -> (status, sectors_read) */
	pub fn write_chs(&mut self, mem: &Memory, sector_count: u8, cylinder: u16, head: u8, sector: u8, data_seg: u16, data_addr: u16)
		-> (Status, u8)
	{
		// TODO: WARNING: how should we handle wrapping around in a segment?
		// Seems the BIOS relies on a DMA, which may not take segmentation into account
		// (but is affected by 64k boundaries...)

		if head as u32 >= self.parameters.heads() || sector as u32 > self.parameters.sectors_per_track() || sector == 0 || cylinder as u32 >= self.parameters.cylinders()
		{
			storage_print!("Warning: attempted to read from invalid CHS {}/{}/{}", cylinder, head, sector);
			return (Status::SectorNotFound,0)
		}

		let lba = 
			(
				(cylinder as u32) * self.parameters.heads() +
				(head as u32)
			) * self.parameters.sectors_per_track() +
			((sector - 1) as u32);

		storage_print!("Write {} bytes to LBA {} (pos {}) (CHS {},{},{}) from {:04x}:{:04x}",
			(sector_count as u32) * 512, lba, lba * 512, cylinder, head, sector, data_seg, data_addr);
		let (status, written) = self.write(mem, lba * 512, phys_addr(data_seg, data_addr), (sector_count as u32) * 512);
		(status, (written / 512) as u8)
	}

	pub fn write(&mut self, mem: &Memory, to: u32, from: u32, len: u32) -> (Status, u32)
	{
		let pos = self.file.seek(SeekFrom::Start(to as u64));
		if pos.unwrap() != to as u64
		{
			storage_print!("Unable to seek file to {}", to);
			return (Status::SectorNotFound, 0)
		}

		let input_buf = mem.slice(from, len);

		match self.file.write(&input_buf)
		{
			Ok(written) =>
			{
				assert!(written <= len as usize);

				if written < (len as usize)
				{
					panic!("Unable to write enough data from file: expected {}, got {}", len, written);
					/* TODO: should we use a "Success" status in such case? */
				}
				(Status::Success, written as u32)
			},
			Err(e) => panic!("Error while writing file: {}", e)
		}
	}

	/* -> (ivt_entry_to_overwrite, seg, addr) */
	pub fn init(&self, mem: &mut Memory) -> (u8, u16, u16)
	{
		const BIOS_SEGMENT: u16 = 0xf000;
		match self.parameters
		{
			DriveParameters::Floppy(ref floppy_params) =>
			{
				const USUAL_FLOPPY_PARAMS_LOCATION_IN_ROM: u16 = 0xEFC7; // See INTERRUP.E / INT1E
				floppy_params.store(mem, phys_addr(BIOS_SEGMENT, USUAL_FLOPPY_PARAMS_LOCATION_IN_ROM));
				(DISKETTE_PARAMETERS_IRQ, BIOS_SEGMENT, USUAL_FLOPPY_PARAMS_LOCATION_IN_ROM)
			}
			DriveParameters::HardDisk(ref hdd_params) =>
			{
				const USUAL_HDD_PARAMS_LOCATION_IN_ROM: u16 = 0xE401; // See INTERRUP.N / INT41
				hdd_params.store(mem, phys_addr(BIOS_SEGMENT, USUAL_HDD_PARAMS_LOCATION_IN_ROM));
				(HDD_PARAMETERS_IRQ, BIOS_SEGMENT, USUAL_HDD_PARAMS_LOCATION_IN_ROM)
			}
		}
	}
}

impl Status
{
	pub fn get_bios_code(self) -> u8
	{
		match self
		{
			Status::Success => 0x00,
			//Status::InvalidParam => 0x01,
			Status::SectorNotFound => 0x04
		}
	}
}