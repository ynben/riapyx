extern crate sdl2;
use super::mem::Memory;
use super::cpu::CPU;

pub mod storage;
pub mod keyboard;
pub mod display;
pub mod serial;
pub mod mouse;
mod scancodes;

// 55 ms
const TIMER_PERIOD_NS: u64 = 55000000;
// 1ms
const EVENT_PUMP_PERIOD_NS: u64 = 1000000;

pub struct HW
{
	sdl: sdl2::Sdl,
	pub event_pump: sdl2::EventPump,

	pub floppy: Option<storage::Storage>,
	pub hdd: Option<storage::Storage>,
	pub keyboard: keyboard::Keyboard,
	pub display: display::Display,
	pub com1: serial::SerialPort<mouse::Mouse>,

	last_event_pump_ns: u64
}

impl HW
{
	pub fn new(floppy_filename: Option<String>, hdd_filename: Option<String>) -> HW
	{
		let sdl = sdl2::init().unwrap();
		let display = display::Display::new(&sdl);
		sdl.mouse().show_cursor(false);
		let event_pump = sdl.event_pump().unwrap();

		HW
		{
			sdl: sdl,
			floppy: floppy_filename.map(
				|fname| -> storage::Storage
				{ storage::Storage::new_floppy(&fname[..]) }),
			hdd: hdd_filename.map(
				|fname| -> storage::Storage
				{ storage::Storage::new_hdd(&fname[..]) }),
			keyboard: keyboard::Keyboard::new(),
			com1: serial::SerialPort::<mouse::Mouse>::new(),
			display: display,
			event_pump: event_pump,
			last_event_pump_ns: 0
		}
	}

	fn pump_events(sdl: &sdl2::Sdl, keyboard: &mut keyboard::Keyboard, mouse: &mut mouse::Mouse, cpu: &mut CPU, event_iterator: &mut Iterator<Item=sdl2::event::Event>, at_most_one: bool)
	{
		for event in event_iterator
		{
			use self::sdl2::event::Event;
			match event
			{
				Event::KeyDown {timestamp:_,window_id:_,keycode:_,scancode,keymod:_,repeat:_} => 
					{ keyboard.on_keydown(cpu, scancode) }
				Event::KeyUp {timestamp:_,window_id:_,keycode:_,scancode,keymod:_,repeat:_} => 
					{ keyboard.on_keyup(cpu, scancode) }
				Event::MouseMotion {timestamp:_,window_id:_,which:_,mousestate:_,x:_,y:_,xrel,yrel} =>
					{ mouse.on_motion(xrel, yrel) }
				Event::MouseButtonDown {timestamp:_,window_id:_,which:_,mouse_btn,x:_,y:_} =>
					{ mouse.on_button_down(mouse_btn) }
				Event::MouseButtonUp {timestamp:_,window_id:_,which:_,mouse_btn,x:_,y:_} =>
					{ mouse.on_button_up(mouse_btn) },
				Event::MouseWheel {timestamp:_, window_id:_, which:_, x:_, y} =>
					{
						if y > 0 { sdl.mouse().set_relative_mouse_mode(false); mouse.set_speed_divisor(1); }
						else if y < 0 { sdl.mouse().set_relative_mouse_mode(true); mouse.set_speed_divisor(1); }
					}
				Event::Quit {timestamp:_} => super::std::process::exit(0),
				_ => {}
			};

			if at_most_one { break }
		}
	}

	pub fn wait_for_event(&mut self, cpu: &mut CPU)
	{
		let mut event_it = self.event_pump.wait_iter();
		/* At most one event as, else, we will iterate
		 * indefinitely and the iterator will always wait */
		HW::pump_events(&self.sdl, &mut self.keyboard, &mut self.com1.device, cpu, &mut event_it, true);
	}

	pub fn step(&mut self, cpu: &mut CPU, mem: &mut Memory, clock: u32, time_ns: u64)
	{
		self.display.render(&self.event_pump, mem, clock, time_ns);
		self.com1.step(cpu);

		if time_ns > self.last_event_pump_ns + EVENT_PUMP_PERIOD_NS
		{
			let mut event_it = self.event_pump.poll_iter();
			HW::pump_events(&self.sdl, &mut self.keyboard, &mut self.com1.device, cpu, &mut event_it, false);
			self.last_event_pump_ns = time_ns;
		}

		/* TODO: move system time handling somewhere else */
		let old_system_clock = mem.read_u16(0x46c) as u32 | ((mem.read_u16(0x46e) as u32) << 16);
		let system_clock = ((time_ns / TIMER_PERIOD_NS) % 0x1800B0) as u32;
		if old_system_clock != system_clock
		{
			if (system_clock - old_system_clock) > 1
			{
				println!("Warning: system too slow for timer interrupt!");
			}

			cpu.queue_hw_interrupt(0x8);
		}

		if system_clock < old_system_clock
		{
			/* Midnight flag */
			mem.write_u16(0x470, 0x1);
		}
		mem.write_u16(0x46c, (system_clock & 0xffff) as u16);
		mem.write_u16(0x46e, (system_clock >> 16) as u16);
	}
}