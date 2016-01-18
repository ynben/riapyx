Early PC (8086) emulator in Rust
================================

The main target was only to run Windows 3.0 in real mode. As a consequence, if something is not required for running Windows 3.0, it is unlikely that it is implemented. Finally, the code quality is not really something to be proud of here.

Emulated hardware
-----------------
 * Intel 8086 CPU, with some opcodes from later processors
 * XT keyboard
 * CGA graphics adapter (only 80x25 16-colors text mode and 640x200 2-colors)
 * Serial port (read-only from the CPU: the aim was only to provide mouse support)
 * Microsoft serial mouse
 * Floppy drive and hard drive through BIOS calls

*NOT* emulated hardware
-----------------------
 * Programmable Interrupt Controller (PIC)
 * Programme Interrupt Timer (PIT)
 * DMA controller
 * Proper IDE hard drive and floppy drive

What can run, what cannot
-------------------------
This emulator is only able to run MS-DOS 6.22 (and should be able to should run MS-DOS 5.0 too). Windows 3.0 can run in real mode and most pre-installed applications seem to work.
Some DOS apps also seem to work properly (most command-line tools, DEBUG, EDIT, QBASIC, DEFRAG, HELP). Some don't (SCANDISK).

On the other hand, it can boot neither ELKS nor FreeDOS as of now. 

As floppy-switching is not supported and as this was never tested, it is unlikely that you will be able install a proper OS with this emulator. As a consequence, **pre-installed hard-drive images should be used**. For Windows 3.0, ensure that you have configured your system to use a Microsoft serial mouse, a CGA adapter and an XT keyboard.

Building
--------
You can use cargo to build the project; however, you must have it disable disable overflow checks as the emulator expects overflows to wrap instead of aborting execution. In addition, this project uses rust-sdl2, so you should follow the installation instructions of the rust-sdl2 crate for your platform at: https://github.com/AngryLawyer/rust-sdl2 (mainly for Windows and OS X users).

The following cargo command can be used to build this project with sane parameters:

    cargo rustc --release --features "storage_log machine_log vram-dirty" -- -Z force-overflow-checks=off

The following cargo features can be toggled:

 - cpu_log: various CPU logs regarding HW interrupts and unsupported I/O port access; also required for proper debugging
 - display_log: mainly logs the CGA status register read
 - mouse_log
 - serial_log
 - bios_log: traces most BIOS calls
 - storage_log
 - machine_log: basically prints each second the amount of emulation cycles per second
 - keyboard_log
 - vram-check: warns about VRAM accesses outside of the CGA range
 - cpu-trace: generates a log file that attempts to trace inter-segment calls and copies. Can be used with PlantUML by adding @startuml/@enduml
 - vram-dirty: recommended. Only refreshes the screen when something was written on screen instead of every 16ms.
 - bda-tracking: tracks read/write from/to unimplemented BDA entries.

Usage
-----
    Command-line:
        riapyx [--boot=<drive>] [--hd=<image>] [--fd=<image>]
        riapyx [--help]
    
    Options:
        --help           Display this message
        --hd=<img>       Hard drive image
        --fd=<img>       Floppy disk image
        --boot=<drive>   Boot from floppy disk (fd) or hard drive (hd) [default: hd]

Once started, press 'f' if you just want to run the emulator without debugging.

Mouse wheel down for cursor capture, mouse wheel up for cursor release.
Good luck if the emulator breaks while the cursor was captured...

The following commands are available in the debugger:

 - (nothing): pressing enter runs one instruction (step)
 - c: continue execution
 - t: trace execution (dump the CPU state each step if CPU logs are enabled); useful with file redirection and 'less -r'
 - w [filename]: dump the RAM content into a file
 - b [SEG] [ADDR]: insert a breakpoint at SEG:ADDR
 - d [SEG] [ADDR]: print 16 bytes at SEG:ADDR
 - u [SEG] [ADDR]: disassemble 5 instructions at SEG:ADDR
 - q: Quit
 - f: continue and ignore breakpoints; faster than 'c'

Implementation details
----------------------
1. BIOS
-------
BIOS calls are implemented in the emulator itself rather than as 8086 code. Basically, when the CPU finds that CS = 0xf000, it will transfer control to Rust code, which will then have a look at the CPU registers to find which BIOS call was requested and run it.

The BDA was ignored at the beginning, but now some entries of the BDA are used to store BIOS data instead of Rust structures.

Hard drive and floppy drive are only emulated through BIOS calls; attempting to use anything lower-level will result in reading a lot of 0s from I/O ports and sending a lot of stuff into the void.

Keyboard emulation mixes BIOS-related code with lower-level emulation code which should be separated from it. Basically, before attempting to run Windows 3.0, every program was fine with only emulating the keyboard at the BIOS level. However, Windows 3.0 uses its own keyboard driver and does not rely on the BIOS for this, so the keyboard hardware interrupt and the keyboard I/O ports had to be implemented afterwards.

2. CPU
------
A CPU step consists basically of two steps: a parsing step that will translate the bytecode into something more practical (see cpu/instruction.rs) and an execution step. 

3. Keyboard
-----------
All SDL events are translated to scancodes in an "I/O" queue, which can be read from the guest with the 'IN' instruction. The BIOS initially registers itself onto INT 9; when called, all scancodes in the I/O queue are transferred to a "BIOS" queue which can be read from with BIOS calls.

4. CGA adapter
--------------
In 640x200x2 mode, as palette management functions were not exported yet by rust-sdl2 into safe rust code, a "lookup surface" is used to write each byte (8x1 pixels) one by one to the window surface, which is likely to be suboptimal, especially if any kind of hardware acceleration is used. 

The SDL window is refreshed at most 60 times per second, and only if a VRAM write is detected in the meantime (unless the feature 'vram-dirty' is disabled).

5. About performances
---------------------
This emulator does not aim for good performances. I did some minimal profiling in order to do some trivial optimisations, but didn't go further.

Testing
-------
In order to make sure that CPU instructions were emulated properly, a test program relying only on BIOS calls was written in 8086 assembly. This program can be found in the cpu_test/ directory and requires a patched bin86 package (only change MACPSIZ from 128/... to 1024/... in as/const.h to make as86 able to handle the macro-hell that is the test program). The result is a floppy image whose MBR will load the test program from the floppy and run it.

All arithmetic, logic, shift-rotate and string instructions with word operands should be tested.

The program can run on QEMU too (which was the main argument for building it as a generic bootable floppy). However, as a consequence, no 8086-specific behavior is tested.

Notes
-----
* Windows 3.0 could run before any hardware interrupt was implemented, which was a bit surprising. At that time, IN/OUT CPU instructions were basically ignored and the only way to communicate with the hardware was through the BIOS and the VRAM.
However,
 - The hardware keyboard interrupt was required for keyboard support (which is necessary as soon as you want to do something better than watching the Program Manager window)
 - The serial port (COM1) hardware interrupt was required for mouse support.
 - The timer interrupt was required for various stuff. Without the timer interrupt, 
      - The mouse cursor would not be displayed
      - Blinking elements (text cursor for example) would not blink
      - The clock program would not refresh the clock unless switching between applications

* Some opcodes which, as far as I know, are not supported by the 8086, had to be implemented in order to be able to run MS-DOS and Windows 3.0 properly. I am not sure of why I was encountering them; perhaps the emulated system tried to determine the CPU and found that the CPU was something more recent (which is strange as 8086-y behaviors such as PUSH SP pushing after decrementing, high flag bits being always one, shift-rotate instructions not masking with 31, were reproduced). These opcodes are:
 - OR/XOR/AND WORD with 8-bit sign-extended immediate operand (encountered when trying to run an MS-DOS system)
 - SHL with an immediate operand (encountered when trying to run the "DOS prompt" from Windows)


License
-------
The included CGA font is the 8x8 VGA font used by SeaBIOS. Please see http://code.coreboot.org/p/seabios/source/tree/master/vgasrc/vgafonts.c. The font is public domain; the initial package was from (c) Joseph Gil. Please see cga_font.txt for more details.


The emulator code itself and the test program is given under the MIT license:

Copyright (c) 2016

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.