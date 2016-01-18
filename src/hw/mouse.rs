use std::collections::vec_deque::VecDeque;

use super::serial::SerialDevice;

use super::sdl2::mouse::Mouse as SdlMouse;

pub struct Mouse
{
	hw_init: bool,
	initialized: bool,
	current_event_byte_count: usize,
	events: VecDeque<[u8; 3]>,
	left_button_pressed: bool,
	right_button_pressed: bool,
	speed_divisor: i32
}

impl SerialDevice<Mouse> for Mouse
{
	fn new() -> Mouse
	{
		Mouse
		{
			hw_init: false,
			initialized: false,
			left_button_pressed: false,
			right_button_pressed: false,
			current_event_byte_count: 0,
			events: VecDeque::<[u8; 3]>::new(),
			speed_divisor: 1
		}
	}

	fn read(&mut self) -> Option<u8>
	{
		if ! self.hw_init
		{
			return None;
		}

		if self.initialized
		{
			if self.current_event_byte_count >= 3
			{
				self.current_event_byte_count = 0;
				self.events.pop_front().unwrap();
			}

			match self.events.front()
			{
				Some(event) =>
				{
					let ret = event[self.current_event_byte_count];
					self.current_event_byte_count += 1;
					Some(ret)
				}
				None => None
			}
		}
		else
		{
			self.initialized = true;
			Some('M' as u8)
		}
	}

	fn on_rts_enabled(&mut self)
	{
		mouse_print!("Mouse reset");
		self.hw_init = true;
		self.initialized = false;
		self.current_event_byte_count = 0;
		self.right_button_pressed = false;
		self.left_button_pressed = false;
		self.events.clear();
	}
}

impl Mouse
{
	fn push_event(&mut self, dx: u8, dy: u8)
	{
		let first_byte = 
			0x40 |
			if self.left_button_pressed { 0x20 } else { 0x0 } |
			if self.right_button_pressed { 0x10 } else { 0x0 } |
			(dy >> 6) << 2 |
			(dx >> 6);
		let second_byte = dx & 0x3f;
		let third_byte = dy & 0x3f;
		self.events.push_back([first_byte, second_byte, third_byte]);
	}

	pub fn on_motion(&mut self, dx: i32, dy: i32)
	{
		mouse_print!("Motion: {} {}", dx, dy);
		/* Send multiple events if we exceed the u8 capacities */
		let clamp = |x: i32| -> i32
			{
				if x < -128
				{ -128 }
				else if x > 127
				{ 127 }
				else
				{ x }
			};
		let mut dx_remain = dx / self.speed_divisor;
		let mut dy_remain = dy / self.speed_divisor;
		while dx_remain != 0 || dy_remain != 0
		{
			let dx_cur = clamp(dx_remain);
			let dy_cur = clamp(dy_remain);
			mouse_print!("Partial motion: {} {}", dx_cur, dy_cur);
			dx_remain -= dx_cur;
			dy_remain -= dy_cur;
			self.push_event(dx_cur as i8 as u8, dy_cur as i8 as u8);
		}
	}

	pub fn on_button_down(&mut self, button: SdlMouse)
	{
		match button
		{
			SdlMouse::Left => 
			{
				self.left_button_pressed = true;
				mouse_print!("Left pressed")
			}
			SdlMouse::Right => 
			{
				self.right_button_pressed = true;
				mouse_print!("Right pressed")
			}
			_ => {}
		}

		self.push_event(0, 0);
	}

	pub fn on_button_up(&mut self, button: SdlMouse)
	{
		match button
		{
			SdlMouse::Left => 
			{
				self.left_button_pressed = false;
				mouse_print!("Left released")
			}
			SdlMouse::Right => 
			{
				self.right_button_pressed = false;
				mouse_print!("Right released")
			}
			_ => {}
		}
		
		self.push_event(0, 0);
	}

	pub fn set_speed_divisor(&mut self, speed_divisor: i32)
	{
		self.speed_divisor = speed_divisor
	}
}