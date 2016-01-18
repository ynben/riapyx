use std::collections::vec_deque::VecDeque;
use super::super::cpu::CPU;

pub trait SerialDevice<DeviceType>
{
	fn new() -> DeviceType;
	fn read(&mut self) -> Option<u8>;
	fn on_rts_enabled(&mut self);
}

enum SerialIrq
{
	DataAvail,
	//TransmitterEmpty,
	//Error,
	//StatusChange
}

/* Retain the RTD register value for some cycles before
 * sending a new byte. */
const CYCLES_BEFORE_NEXT_READ: u32 = 1;

impl SerialIrq
{
	fn iir_value(&self) -> u8
	{
		match *self
		{
			SerialIrq::DataAvail => 0x0,
			//SerialIrq::TransmitterEmpty => 0x1,
			//SerialIrq::Error => 0x2,
			//SerialIrq::StatusChange => 0x3
		}
	}
}

pub struct SerialPort<T: SerialDevice<T>>
{
	/* Interrupts */
	data_avail_irq: bool,
	transmitter_empty_irq: bool,
	error_irq: bool,
	status_change_irq: bool,

	/* Line control */
	word_length: u8,
	stop_bits: u8,
	parity_enable: bool,
	baud_rate_divisor_latch: bool,
	baud_rate_divisor: u16,

	/* Modem control */
	data_terminal_ready: bool,
	request_to_send: bool,
	interrupt_enable: bool,

	cycles_before_next_read: u32,
	current_byte_read: bool,
	current_read_byte: u8,
	data_for_guest: VecDeque<u8>,
	irq_queue: VecDeque<SerialIrq>,

	pub device: T
}

impl<T: SerialDevice<T>> SerialPort<T>
{
	pub fn new() -> SerialPort<T>
	{
		SerialPort
		{
			data_avail_irq: false,
			transmitter_empty_irq: false,
			error_irq: false,
			status_change_irq: false,
			word_length: 8,
			stop_bits: 1,
			parity_enable: false,
			baud_rate_divisor_latch: false,
			baud_rate_divisor: 2,
			data_terminal_ready: false,
			request_to_send: false,
			interrupt_enable: false,
			cycles_before_next_read: 0,
			current_byte_read: false,
			current_read_byte: 0,
			data_for_guest: VecDeque::<u8>::new(),
			irq_queue: VecDeque::<SerialIrq>::new(),
			device: T::new()
		}
	}

	pub fn write_rtd(&mut self, val: u8)
	{
		if ! self.baud_rate_divisor_latch
		{
			serial_print!("UNIMPLEMENTED: Write to device {:02x}", val);
		}
		else
		{
			self.baud_rate_divisor |= val as u16;
			serial_print!("Write baud rate lo {:02x}", val);
		}
		self.dump();
	}

	pub fn read_rtd(&mut self) -> u8
	{
		if ! self.baud_rate_divisor_latch
		{
			let val = self.current_read_byte;
			self.current_byte_read = true;
			serial_print!("Read from device {:02x}", val);
			val as u8
		}
		else
		{
			serial_print!("Read baud rate lo");
			self.baud_rate_divisor as u8
		}
	}

	pub fn write_ier(&mut self, val: u8)
	{
		if ! self.baud_rate_divisor_latch
		{
			serial_print!("Write to interrupt enable register {:02x}", val);
			self.data_avail_irq = val & 0x1 > 0;
			self.transmitter_empty_irq = val & 0x2 > 0;
			self.error_irq = val & 0x4 > 0;
			self.status_change_irq = val & 0x8 > 0;
		}
		else
		{
			serial_print!("Write baud rate hi {:02x}", val);
			self.baud_rate_divisor |= (val as u16) << 8;
		}
		self.dump();
	}

	pub fn read_ier(&mut self) -> u8
	{
		if ! self.baud_rate_divisor_latch
		{
			let mut val = 0;
			{
				let mut set = |flag: u8, flag_set: bool|
					{
						if flag_set
						{
							val |= flag;
						}
					};
				set(0x1, self.data_avail_irq);
				set(0x2, self.transmitter_empty_irq);
				set(0x4, self.error_irq);
				set(0x8, self.status_change_irq);
			}
			serial_print!("Read interrupt enable register: {:02x}", val);
			val
		}
		else
		{
			(self.baud_rate_divisor >> 8) as u8
		}
	}

	pub fn read_iir(&self) -> u8
	{
		let ret = 
			if self.irq_queue.is_empty()
			{
				0
			}
			else
			{
				1 | (self.irq_queue.front().unwrap().iir_value() << 1)
			};

		serial_print!("Read interrupt identification register: {:02x}", ret);
		ret
	}

	pub fn read_lc(&self) -> u8
	{
		let mut val = 0;
		match self.word_length
		{
			5 => val |= 0x0,
			6 => val |= 0x1,
			7 => val |= 0x2,
			8 => val |= 0x3,
			_ => unreachable!()
		};

		match self.stop_bits
		{
			1 => val |= 0x0,
			2 => val |= 0x4,
			_ => unreachable!()
		}

		if self.parity_enable
		{
			panic!("Parity is not supported");
		}

		if self.baud_rate_divisor_latch
		{
			val |= 0x80
		}

		serial_print!("Read line control: {:02x}", val);
		val
	}

	pub fn write_lc(&mut self, val: u8)
	{
		serial_print!("Write line control: {:02x}", val);
		self.word_length =
			match val & 0x3
			{
				0 => 5,
				1 => 6,
				2 => 7,
				3 => 8,
				_ => unreachable!()
			};

		self.stop_bits =
			match val & 0x4
			{
				0 => 1,
				1 => 2,
				_ => unreachable!()
			};

		assert!(val & 0x8 == 0);

		assert!(val & 0x40 == 0);

		self.baud_rate_divisor_latch = val & 0x80 > 0;
		self.dump();
	}

	pub fn write_mc(&mut self, val: u8)
	{
		serial_print!("Write modem control: {:02x}", val);
		self.data_terminal_ready = val & 0x1 > 0;
		let request_to_send = val & 0x2 > 0;
		if request_to_send && !self.request_to_send
		{
			self.device.on_rts_enabled();
			self.cycles_before_next_read = 0;
			self.data_for_guest.pop_front();
		}
		self.request_to_send = request_to_send;
		self.interrupt_enable = val & 0x8 > 0;
		self.dump();
	}

	pub fn read_mc(&self) -> u8
	{
		serial_print!("Read modem control");
		let mut val = 0;
		{
			let mut set = |flag: u8, flag_set: bool|
				{
					if flag_set
					{
						val |= flag;
					}
				};
			set(0x1, self.data_terminal_ready);
			set(0x2, self.request_to_send);
			set(0x8, self.interrupt_enable);
		}
		val
	}

	pub fn read_lsr(&self) -> u8
	{
		//let init_val = 0x60; // All transmitter registers empty, no data available
		let init_val = 0;
		let ret = 
			if self.current_byte_read
			{
				init_val
			}
			else
			{
				init_val|1 // Data available
			};
		
		serial_print!("Read line status: {:02x}", ret);
		ret
	}

	fn dump(&self)
	{
		serial_print!("IRQ: DA:{},TE:{},E:{},S:{} | BL: {} | BR: {}",
			self.data_avail_irq,
			self.transmitter_empty_irq,
			self.error_irq,
			self.status_change_irq,
			self.baud_rate_divisor_latch,
			115200 / (self.baud_rate_divisor as u32));

		serial_print!("WL: {} | SB: {} | PAR: {} | DTR: {} | RTS: {} | IE: {}", 
			self.word_length,
			self.stop_bits,
			self.parity_enable, 
			self.data_terminal_ready, 
			self.request_to_send, 
			self.interrupt_enable);
	}

	pub fn step(&mut self, cpu: &mut CPU)
	{
		if self.cycles_before_next_read > 0
		{
			self.cycles_before_next_read -= 1;
		}

		if self.cycles_before_next_read == 0 && self.current_byte_read
		{
			if let Some(val) = self.device.read()
			{
				self.cycles_before_next_read = CYCLES_BEFORE_NEXT_READ;
				self.current_read_byte = val;
				self.current_byte_read = false;
				if self.interrupt_enable && self.data_avail_irq
				{
					cpu.queue_hw_interrupt(0x0C);
					self.irq_queue.push_back(SerialIrq::DataAvail);
				}
			}
		}
	}
}