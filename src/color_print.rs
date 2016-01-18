extern crate ansi_term;

macro_rules! color_print
{
	($color: expr, $($x: expr),*) =>
	{
		println!("{}", $color.paint(format!($($x),*)));
	};
}

macro_rules! color_printnoln
{
	($color: expr, $($x: expr),*) =>
	{
		print!("{}", $color.paint(format!($($x),*)));
	};
}


#[cfg(feature="cpu_log")]
macro_rules! cpu_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Green, concat!("[CPU] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Green, concat!("[CPU] ", $y), $($x),*)
	};
}

#[cfg(not(feature="cpu_log"))]
macro_rules! cpu_print {($($x: expr),*) => {()};}

macro_rules! disas_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Blue, concat!("[DISAS] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Blue, concat!("[DISAS] ", $y), $($x),*);
	};	
}

#[cfg(feature="bios_log")]
macro_rules! bios_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Yellow, concat!("[BIOS] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Yellow, concat!("[BIOS] ", $y), $($x),*);
	};	
}

#[cfg(not(feature="bios_log"))]
macro_rules! bios_print {($($x: expr),*) => {()};}

#[cfg(feature="storage_log")]
macro_rules! storage_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Cyan, concat!("[STORAGE] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Cyan, concat!("[STORAGE] ", $y), $($x),*);
	};
}

#[cfg(not(feature="storage_log"))]
macro_rules! storage_print {($($x: expr),*) => {()};}

macro_rules! debug_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Purple, concat!("[DEBUG] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Purple, concat!("[DEBUG] ", $y), $($x),*);
	};	
}

#[cfg(feature="serial_log")]
macro_rules! serial_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Fixed(100), concat!("[SERIAL] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Fixed(100), concat!("[SERIAL] ", $y), $($x),*);
	};	
}

#[cfg(not(feature="serial_log"))]
macro_rules! serial_print {($($x: expr),*) => {()};}

#[cfg(feature="mouse_log")]
macro_rules! mouse_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Fixed(120), concat!("[MOUSE] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Fixed(120), concat!("[MOUSE] ", $y), $($x),*);
	};	
}

#[cfg(not(feature="mouse_log"))]
macro_rules! mouse_print {($($x: expr),*) => {()};}

#[cfg(feature="keyboard_log")]
macro_rules! keyboard_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Fixed(140), concat!("[KEYBOARD] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Fixed(140), concat!("[KEYBOARD] ", $y), $($x),*);
	};	
}

#[cfg(not(feature="keyboard_log"))]
macro_rules! keyboard_print {($($x: expr),*) => {()};}

#[cfg(feature="display_log")]
macro_rules! display_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Fixed(160), concat!("[DISPLAY] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Fixed(160), concat!("[DISPLAY] ", $y), $($x),*);
	};	
}

#[cfg(not(feature="display_log"))]
macro_rules! display_print {($($x: expr),*) => {()};}

#[cfg(feature="machine_log")]
macro_rules! machine_print
{
	($y: expr) => {color_print!($crate::ansi_term::Colour::Fixed(180), concat!("[MACHINE] ", $y))};
	($y: expr, $($x: expr),*) =>
	{
		color_print!($crate::ansi_term::Colour::Fixed(180), concat!("[MACHINE] ", $y), $($x),*);
	};	
}

#[cfg(not(feature="machine_log"))]
macro_rules! machine_print {($($x: expr),*) => {()};}