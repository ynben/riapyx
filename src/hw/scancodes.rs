extern crate sdl2;

use super::sdl2::keyboard::Scancode;

pub fn sdlk_to_scancode(sdlk: Scancode) -> Option<u8>
{
	match sdlk
	{
		Scancode::Escape => Some(0x01),
		Scancode::Num1 => Some(0x02),
		Scancode::Num2 => Some(0x03),
		Scancode::Num3 => Some(0x04),
		Scancode::Num4 => Some(0x05),
		Scancode::Num5 => Some(0x06),
		Scancode::Num6 => Some(0x07),
		Scancode::Num7 => Some(0x08),
		Scancode::Num8 => Some(0x09),
		Scancode::Num9 => Some(0x0a),
		Scancode::Num0 => Some(0x0b),
		Scancode::Minus => Some(0x0c),
		Scancode::Equals => Some(0x0d),
		Scancode::Backspace => Some(0x0e),
		Scancode::Tab => Some(0x0f),
		Scancode::Q => Some(0x10),
		Scancode::W => Some(0x11),
		Scancode::E => Some(0x12),
		Scancode::R => Some(0x13),
		Scancode::T => Some(0x14),
		Scancode::Y => Some(0x15),
		Scancode::U => Some(0x16),
		Scancode::I => Some(0x17),
		Scancode::O => Some(0x18),
		Scancode::P => Some(0x19),
		Scancode::LeftBracket => Some(0x1a),
		Scancode::RightBracket => Some(0x1b),
		Scancode::Return => Some(0x1c),
		Scancode::LCtrl => Some(0x1d),
		Scancode::A => Some(0x1e),
		Scancode::S => Some(0x1f),
		Scancode::D => Some(0x20),
		Scancode::F => Some(0x21),
		Scancode::G => Some(0x22),
		Scancode::H => Some(0x23),
		Scancode::J => Some(0x24),
		Scancode::K => Some(0x25),
		Scancode::L => Some(0x26),
		Scancode::Semicolon => Some(0x27),
		Scancode::Apostrophe => Some(0x28),
		Scancode::Grave => Some(0x29),
		Scancode::LShift => Some(0x2a),
		Scancode::Backslash => Some(0x2b),
		//on a 102-key keyboard
		Scancode::Z => Some(0x2c),
		Scancode::X => Some(0x2d),
		Scancode::C => Some(0x2e),
		Scancode::V => Some(0x2f),
		Scancode::B => Some(0x30),
		Scancode::N => Some(0x31),
		Scancode::M => Some(0x32),
		Scancode::Comma => Some(0x33),
		Scancode::Period => Some(0x34),
		Scancode::Slash => Some(0x35),
		Scancode::RShift => Some(0x36),
		//Scancode::*/PrtScn => Some(0x37 (Keypad-*) or on a 83/84-key keyboard),
		Scancode::LAlt => Some(0x38),
		Scancode::Space => Some(0x39),
		Scancode::CapsLock => Some(0x3a),
		Scancode::F1 => Some(0x3b),
		Scancode::F2 => Some(0x3c),
		Scancode::F3 => Some(0x3d),
		Scancode::F4 => Some(0x3e),
		Scancode::F5 => Some(0x3f),
		Scancode::F6 => Some(0x40),
		Scancode::F7 => Some(0x41),
		Scancode::F8 => Some(0x42),
		Scancode::F9 => Some(0x43),
		Scancode::F10 => Some(0x44),
		//Scancode::NumLock => Some(0x45),
		Scancode::ScrollLock => Some(0x46),
		Scancode::Home => Some(0x47),
		Scancode::Up => Some(0x48),
		Scancode::PageUp => Some(0x49),
		//Scancode::=> Some(0x4a),
		Scancode::Left => Some(0x4b),
		//Scancode::=> Some(0x4c),
		Scancode::Right => Some(0x4d),
		//Scancode::=> Some(0x4e),
		Scancode::End => Some(0x4f),
		Scancode::Down => Some(0x50),
		Scancode::PageDown => Some(0x51),
		Scancode::Insert => Some(0x52),
		Scancode::Delete => Some(0x53),
		//Scancode::Alt-SysRq => Some(0x54 on a 84+ key keyboard),
		_ => None
	}
}

pub fn scancode_to_ascii(scancode: u8, shifted: bool) -> Option<u8>
{
	if !shifted
	{
		match scancode
		{
			0x01 => Some(0x1b),
			0x02 => Some('1' as u8),
			0x03 => Some('2' as u8),
			0x04 => Some('3' as u8),
			0x05 => Some('4' as u8),
			0x06 => Some('5' as u8),
			0x07 => Some('6' as u8),
			0x08 => Some('7' as u8),
			0x09 => Some('8' as u8),
			0x0a => Some('9' as u8),
			0x0b => Some('0' as u8),
			0x0c => Some('-' as u8),
			0x0d => Some('=' as u8),
			0x0e => Some(0x8 /* \b */),
			0x0f => Some('\t' as u8),
			0x10 => Some('q' as u8),
			0x11 => Some('w' as u8),
			0x12 => Some('e' as u8),
			0x13 => Some('r' as u8),
			0x14 => Some('t' as u8),
			0x15 => Some('y' as u8),
			0x16 => Some('u' as u8),
			0x17 => Some('i' as u8),
			0x18 => Some('o' as u8),
			0x19 => Some('p' as u8),
			0x1a => Some('[' as u8),
			0x1b => Some(']' as u8),
			0x1c => Some('\r' as u8),
			0x1e => Some('a' as u8),
			0x1f => Some('s' as u8),
			0x20 => Some('d' as u8),
			0x21 => Some('f' as u8),
			0x22 => Some('g' as u8),
			0x23 => Some('h' as u8),
			0x24 => Some('j' as u8),
			0x25 => Some('k' as u8),
			0x26 => Some('l' as u8),
			0x27 => Some(';' as u8),
			0x28 => Some('\'' as u8),
			0x29 => Some('`' as u8),
			0x2b => Some('\\' as u8),
			0x2c => Some('z' as u8),
			0x2d => Some('x' as u8),
			0x2e => Some('c' as u8),
			0x2f => Some('v' as u8),
			0x30 => Some('b' as u8),
			0x31 => Some('n' as u8),
			0x32 => Some('m' as u8),
			0x33 => Some(',' as u8),
			0x34 => Some('.' as u8),
			0x35 => Some('/' as u8),
			0x39 => Some(' ' as u8),
			_ => None
		}
	}
	else // shifted
	{
		match scancode
		{
			0x01 => Some(0x1b),
			0x02 => Some('!' as u8),
			0x03 => Some('@' as u8),
			0x04 => Some('#' as u8),
			0x05 => Some('$' as u8),
			0x06 => Some('%' as u8),
			0x07 => Some('^' as u8),
			0x08 => Some('&' as u8),
			0x09 => Some('*' as u8),
			0x0a => Some('(' as u8),
			0x0b => Some(')' as u8),
			0x0c => Some('_' as u8),
			0x0d => Some('+' as u8),
			0x0e => Some(0x8 /* \b */),
			0x0f => Some('\t' as u8),
			0x10 => Some('Q' as u8),
			0x11 => Some('W' as u8),
			0x12 => Some('E' as u8),
			0x13 => Some('R' as u8),
			0x14 => Some('T' as u8),
			0x15 => Some('Y' as u8),
			0x16 => Some('U' as u8),
			0x17 => Some('I' as u8),
			0x18 => Some('O' as u8),
			0x19 => Some('P' as u8),
			0x1a => Some('{' as u8),
			0x1b => Some('}' as u8),
			0x1c => Some('\r' as u8),
			0x1e => Some('A' as u8),
			0x1f => Some('S' as u8),
			0x20 => Some('D' as u8),
			0x21 => Some('F' as u8),
			0x22 => Some('G' as u8),
			0x23 => Some('H' as u8),
			0x24 => Some('J' as u8),
			0x25 => Some('K' as u8),
			0x26 => Some('L' as u8),
			0x27 => Some(':' as u8),
			0x28 => Some('"' as u8),
			0x29 => Some('~' as u8),
			0x2b => Some('|' as u8),
			0x2c => Some('Z' as u8),
			0x2d => Some('X' as u8),
			0x2e => Some('C' as u8),
			0x2f => Some('V' as u8),
			0x30 => Some('B' as u8),
			0x31 => Some('N' as u8),
			0x32 => Some('M' as u8),
			0x33 => Some('<' as u8),
			0x34 => Some('>' as u8),
			0x35 => Some('?' as u8),
			0x39 => Some(' ' as u8),
			_ => None
		}
	}
}