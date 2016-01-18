GET flags.inc

!
! Loader
!

TARGET = 0x7E00
TARGET_SEG = 0x07C0
LOADED_ADDR = 0x200

.org 0
export _main
_main:
    ! Temporary stack so we can IRET from BIOS calls
    mov ax, #0x0
    mov ss, ax
    mov sp, #0x7c00

    mov ax, #TARGET_SEG
    mov es, ax

    ! First 32KB
    mov bx, #LOADED_ADDR
    mov ah, #0x02
    mov al, #0x40 ! 64 sectors = 32K
    mov ch, #0
    mov cl, #2 ! Skip first sector (ourselves)
    mov dh, #0
    mov dl, #0
    int #0x13
    jb load_failure

    ! Reading from 32.5KB
    ! -> skip 65 sectors
    ! Floppy is 18 sector/tracks, 80 tracks/sides, 2 sides
    ! First sector = 11 + 1 = 12
    ! Head = 1
    ! Cylinder = 1
    ! Next 32KB
    ! Loading is done in two steps as we cross a 64K boundary
    mov bx, #LOADED_ADDR
    add bx, #0x8000
    mov ah, #0x02
    mov al, #1
    mov ch, #1
    mov cl, #12 ! Skip first 32KB+first sector
    mov dh, #1
    mov dl, #0
    int #0x13
    jb load_failure
    mov bx, #LOADED_ADDR
    add bx, #0x8200
    mov ah, #0x02
    mov al, #0x3e ! 62 sectors = 31 (.5K + 32K + 31K + .5K = 64K = full segment)
    mov ch, #1
    mov cl, #13 ! Skip first 32KB+first sector
    mov dh, #1
    mov dl, #0
    int #0x13
    jb load_failure

    jmp far TARGET_SEG:LOADED_ADDR

    ! After that, we should find the first 64K of the floppy loaded at CS=TARGET_SEG

load_failure:
    int #0x18

.org $1FE
    .byte 0x55
    .byte 0xAA

!
! Actual code
!
.org 0x200
DATA_SEG  = 0x07C0
STACK_SEG = 0x07C0

ES_DISPL = 0x1

boot_stage2:
    mov ax, #DATA_SEG
    mov ds, ax

    ! Stack setup
    mov ax, #STACK_SEG
    mov ss, ax

    ! Fill stack with varying data
    mov sp, #0x0
fillstack_loop:
    mov ax, sp
    push ax
    cmp sp, #0xff00 ! TODO: use constant
    jne fillstack_loop
    ! Finally setup the stack pointer
    mov sp, #0 ! Stack grows backward

    ! ES setup
    mov ax, #STACK_SEG
    add ax, #ES_DISPL
    mov es, ax

    ! Video setup: 80x25 colored
    mov ah, #0
    !mov al, #0x12
    mov al, #0x3
    int #0x10
    jb video_fail

    ! Division by zero interrupt setup
    mov ax, #0
    mov ds, ax
    mov ax, #DATA_SEG
    mov 0x2, ax
    mov ax, #div_by_zero
    mov 0x0, ax
    mov ax, #DATA_SEG
    mov ds, ax

    call run_tests

! TODO: div_by_zero -> div_exception
div_by_zero_expected:
    .byte 0
div_by_zero_str:
    .asciz "Division by zero"
div_by_zero:
    cmp byte div_by_zero_expected, #1
    je div_by_zero_ok
    mov si, #div_by_zero_str
    call rprint
    call _end
div_by_zero_ok:
    mov byte div_by_zero_expected, #0
    ! 286+ processors return to div, while
    ! 8086 processors should return to the instruction after
    ! Just set up a proper division, assuming dest=bx to handle
    ! both cases
    mov ax, #0x0bad
    mov dx, #0x0
    mov bx, #0x1
    iret
set_div_by_zero_expected:
    mov byte div_by_zero_expected, #1
    ret
set_div_by_zero_unexpected:
    mov byte div_by_zero_expected, #0
    ret


video_fail_str:
    .asciz "Unable to set VGA mode"
video_fail:
    mov si, #video_fail_str
    call rprint
    call _end

GET print.inc

GET testing.inc

GET str_testing.inc
!
! Actual tests
!

test_str:
    .asciz "Printing test. You should be able to see this message.\r\n"

run_tests:
    mov si, #test_str
    call gprint


    ! Print memory size
    puts(memsize,,"Memory size: ")
    int 0x12
    call print_numw

    ! See user manual p.23 for flags documentation, but incomplete
    ! Wikipedia seems better, but the manual might contain something
    ! more precise elsewhere.

    ! TODO: byte operands?

    !
    ! Arithmetic operations
    !

    ! ADD
    arith_test(add,0x2,0x3,0x5,ZOSZAPC,ZP,0)
    arith_test(add,0x2,0x7fff,0x8001,ZOSZAPC,ZOSA,0) ! O,S
    arith_test(add,0x8000,0x8000,0x0,ZOSZAPC,ZOZPC,0) ! O,S
    arith_test(add,0x9,0x9,0x12,ZOSZAPC,ZAP,0) ! A
    arith_test(add,0x10,0x10,0x20,ZOSZAPC,0,0) ! A
    arith_test(add,0x5,0x5,0xa,ZOSZAPC,ZP,0) ! A
    arith_test(add,0xffff,0x5,0x4,ZOSZAPC,ZAC,0) ! C
    arith_test(add,0xfffe,0x1,0xffff,ZOSZAPC,ZSP,0) ! C
    arith_test(add,0x0,0x0,0x0,ZOSZAPC,ZZP,0) ! Z
    arith_test(add,0x0f00,0x0100,0x1000,ZOSZAPC,ZP,0) ! A?

    ! ADC
    arith_test(adc,0x2,0x3,0x5,ZOSZAPC,ZP,0)
    arith_test(adc,0x2,0x7fff,0x8001,ZOSZAPC,ZOSA,0) ! O,S
    arith_test(adc,0x9,0x9,0x12,ZOSZAPC,ZAP,0) ! A
    arith_test(adc,0x10,0x10,0x20,ZOSZAPC,0,0) ! A
    arith_test(adc,0xffff,0x5,0x4,ZOSZAPC,ZAC,0) ! C (set)
    arith_test(adc,0xffff,0x5,0x5,ZOSZAPC,ZAPC,0) ! C (use)
    arith_test(add,0xffff,0x6,0x5,ZOSZAPC,ZAPC,0) ! C (ignored by add)
    arith_test(adc,0xfffe,0x1,0x0,ZOSZAPC,ZZAPC,0) ! Carry propagation

    ! AND -> SZP
    arith_test(and,0x1,0x2,0x0,ZOSZPC,ZZP,ZA) ! Z
    arith_test(and,0x1ff,0xff00,0x100,ZOSZPC,ZP,ZA) ! Value
    arith_test(and,0x8000,0xff00,0x8000,ZOSZPC,ZSP,ZA) ! S
    arith_test(and,0xff,0x01,0x01,ZOSZPC,0,ZA) ! P (not)

    ! OR -> SZP
    arith_test(or,0x1,0x2,0x3,ZOSZPC,ZP,ZA) ! P
    arith_test(or,0x8002,0x8002,0x8002,ZOSZPC,ZS,ZA) ! S
    arith_test(or,0xff00,0x01ff,0xffff,ZOSZPC,ZSP,ZA) ! Value
    arith_test(or,0x0,0x0,0x0,ZOSZPC,ZZP,ZA) ! Z
    arith_test(or,0x1,0x1,0x1,ZOSZPC,0,ZA) ! Z (not)

    ! SBB -> OSZAPC
    arith_test(sbb,0x2,0x1,0x1,ZOSZAPC,0,0) ! Value
    arith_test(sbb,0x2,0x2,0x0,ZOSZAPC,ZZP,0) ! Z
    arith_test(sbb,0x8001,0x2,0x7fff,ZOSZAPC,ZOAP,0) ! O
    arith_test(sbb,0x2,0x3,0xffff,ZOSZAPC,ZSAPC,0) ! C
    arith_test(sbb,0x2,0x2,0xffff,ZOSZAPC,ZSAPC,0) ! Carry propagation
    arith_test(sbb,0x3,0x2,0x0,ZOSZAPC,ZZP,0) ! C (not)
    arith_test(sbb,0x11,0x2,0xf,ZOSZAPC,ZAP,0) ! A

    ! SUB -> OSZAPC
    arith_test(sub,0x2,0x1,0x1,ZOSZAPC,0,0) ! Value
    arith_test(sub,0x2,0x2,0x0,ZOSZAPC,ZZP,0) ! Z
    arith_test(sub,0x8001,0x2,0x7fff,ZOSZAPC,ZOAP,0) ! O
    arith_test(sub,0x2,0x3,0xffff,ZOSZAPC,ZSAPC,0) ! C
    arith_test(sub,0x1,0x1,0x0,ZOSZAPC,ZZP,0) ! No carry propagation
    arith_test(sub,0x11,0x2,0xf,ZOSZAPC,ZAP,0) ! A

    ! XOR -> SZP
    arith_test(xor,0x1,0x1,0x0,ZOSZPC,ZZP,ZA) ! Normal, Z
    arith_test(xor,0x1,0x0,0x1,ZOSZPC,0,ZA)
    arith_test(xor,0xff10,0x00ff,0xffef,ZOSZPC,ZS,ZA)
    arith_test(xor,0xff10,0xffff,0x00ef,ZOSZPC,0,ZA)
    arith_test(xor,0xff10,0xff20,0x0030,ZOSZPC,ZP,ZA)
    arith_test(xor,0x8000,0x7fff,0xffff,ZOSZPC,ZSP,ZA)

    ! DEC -> OSZAP
    ariths_test(dec,0x1,0x0,ZOSZAP,ZZP,0)
    ariths_test(dec,0x2,0x1,ZOSZAP,0,0)
    ariths_test(dec,0x0,0xffff,ZOSZAP,ZSAP,0)
    ariths_test(dec,0x8000,0x7fff,ZOSZAP,ZOAP,0)

    ! INC -> OSZAP
    ariths_test(inc,0x0,0x1,ZOSZAP,0,0)
    ariths_test(inc,0xffff,0x0,ZOSZAP,ZZAP,0)
    ariths_test(inc,0xfffe,0xffff,ZOSZAP,ZSP,0)
    ariths_test(inc,0x7fff,0x8000,ZOSZAP,ZOSAP,0)

    ! NEG -> OSZAPC
    ariths_test(neg,0x0,0x0,ZOSZAPC,ZZP,0) ! Z,C
    ariths_test(neg,0x1,0xffff,ZOSZAPC,ZSAPC,0) ! A
    ariths_test(neg,0x2,0xfffe,ZOSZAPC,ZSAC,0) ! P
    ariths_test(neg,0xf,0xfff1,ZOSZAPC,ZSAC,0) ! P
    ariths_test(neg,0xff00,0x0100,ZOSZAPC,ZPC,0)
    ariths_test(neg,0x0100,0xff00,ZOSZAPC,ZSPC,0)
    ariths_test(neg,0x8000,0x8000,ZOSZAPC,ZOSPC,0) ! O

    ! NOT -> nothing
    ariths_test(not,0xffff,0x0000,0,0,0)
    ariths_test(not,0x0000,0xffff,0,0,0)
    ariths_test(not,0x0101,0xfefe,0,0,0)

    ! ... Byte operands
    ! ADD
    arithb_test(add,0x2,0x3,0x5,ZOSZAPC,ZP,0)
    arithb_test(add,0x2,0x7f,0x81,ZOSZAPC,ZOSAP,0) ! O,S
    arithb_test(add,0x80,0x80,0x0,ZOSZAPC,ZOZPC,0) ! O,S
    arithb_test(add,0x9,0x9,0x12,ZOSZAPC,ZAP,0) ! A
    arithb_test(add,0x10,0x10,0x20,ZOSZAPC,0,0) ! A
    arithb_test(add,0x5,0x5,0xa,ZOSZAPC,ZP,0) ! A
    arithb_test(add,0xff,0x5,0x4,ZOSZAPC,ZAC,0) ! C
    arithb_test(add,0xfe,0x1,0xff,ZOSZAPC,ZSP,0) ! C
    arithb_test(add,0x0,0x0,0x0,ZOSZAPC,ZZP,0) ! Z
    arithb_test(add,0xe0,0x10,0xf0,ZOSZAPC,ZSP,0) ! A?

    ! ADC
    arithb_test(adc,0x2,0x3,0x5,ZOSZAPC,ZP,0)
    arithb_test(adc,0x2,0x7f,0x81,ZOSZAPC,ZOSAP,0) ! O,S
    arithb_test(adc,0x9,0x9,0x12,ZOSZAPC,ZAP,0) ! A
    arithb_test(adc,0x10,0x10,0x20,ZOSZAPC,0,0) ! A
    arithb_test(adc,0xff,0x5,0x4,ZOSZAPC,ZAC,0) ! C (set)
    arithb_test(adc,0xff,0x5,0x5,ZOSZAPC,ZAPC,0) ! C (use)
    arithb_test(add,0xff,0x6,0x5,ZOSZAPC,ZAPC,0) ! C (ignored by add)
    arithb_test(adc,0xfe,0x1,0x0,ZOSZAPC,ZZAPC,0) ! Carry propagation

    ! AND -> SZP
    arithb_test(and,0x1,0x2,0x0,ZOSZPC,ZZP,ZA) ! Z
    arithb_test(and,0x1f,0xf0,0x10,ZOSZPC,0,ZA) ! Value
    arithb_test(and,0x80,0xff,0x80,ZOSZPC,ZS,ZA) ! S
    arithb_test(and,0xff,0x01,0x01,ZOSZPC,0,ZA) ! P (not)

    ! OR -> SZP
    arithb_test(or,0x1,0x2,0x3,ZOSZPC,ZP,ZA) ! P
    arithb_test(or,0x82,0x82,0x82,ZOSZPC,ZSP,ZA) ! S
    arithb_test(or,0xf0,0x1f,0xff,ZOSZPC,ZSP,ZA) ! Value
    arithb_test(or,0x0,0x0,0x0,ZOSZPC,ZZP,ZA) ! Z
    arithb_test(or,0x1,0x1,0x1,ZOSZPC,0,ZA) ! Z (not)

    ! SBB -> OSZAPC
    arithb_test(sbb,0x2,0x1,0x1,ZOSZAPC,0,0) ! Value
    arithb_test(sbb,0x2,0x2,0x0,ZOSZAPC,ZZP,0) ! Z
    arithb_test(sbb,0x81,0x2,0x7f,ZOSZAPC,ZOA,0) ! O
    arithb_test(sbb,0x2,0x3,0xff,ZOSZAPC,ZSAPC,0) ! C
    arithb_test(sbb,0x2,0x2,0xff,ZOSZAPC,ZSAPC,0) ! Carry propagation
    arithb_test(sbb,0x3,0x2,0x0,ZOSZAPC,ZZP,0) ! C (not)
    arithb_test(sbb,0x11,0x2,0xf,ZOSZAPC,ZAP,0) ! A

    ! SUB -> OSZAPC
    arithb_test(sub,0x2,0x1,0x1,ZOSZAPC,0,0) ! Value
    arithb_test(sub,0x2,0x2,0x0,ZOSZAPC,ZZP,0) ! Z
    arithb_test(sub,0x81,0x2,0x7f,ZOSZAPC,ZOA,0) ! O
    arithb_test(sub,0x2,0x3,0xff,ZOSZAPC,ZSAPC,0) ! C
    arithb_test(sub,0x1,0x1,0x0,ZOSZAPC,ZZP,0) ! No carry propagation
    arithb_test(sub,0x11,0x2,0xf,ZOSZAPC,ZAP,0) ! A

    ! XOR -> SZP
    arithb_test(xor,0x1,0x1,0x0,ZOSZPC,ZZP,ZA) ! Normal, Z
    arithb_test(xor,0x1,0x0,0x1,ZOSZPC,0,ZA)
    arithb_test(xor,0xf1,0x0f,0xfe,ZOSZPC,ZS,ZA)
    arithb_test(xor,0xf1,0xff,0x0e,ZOSZPC,0,ZA)
    arithb_test(xor,0xf1,0xf2,0x03,ZOSZPC,ZP,ZA)
    arithb_test(xor,0x80,0x7f,0xff,ZOSZPC,ZSP,ZA)

    ! DEC -> OSZAP
    arithsb_test(dec,0x1,0x0,ZOSZAP,ZZP,0)
    arithsb_test(dec,0x2,0x1,ZOSZAP,0,0)
    arithsb_test(dec,0x0,0xff,ZOSZAP,ZSAP,0)
    arithsb_test(dec,0x80,0x7f,ZOSZAP,ZOA,0)

    ! INC -> OSZAP
    arithsb_test(inc,0x0,0x1,ZOSZAP,0,0)
    arithsb_test(inc,0xff,0x0,ZOSZAP,ZZAP,0)
    arithsb_test(inc,0xfe,0xff,ZOSZAP,ZSP,0)
    arithsb_test(inc,0x7f,0x80,ZOSZAP,ZOSA,0)

    ! NEG -> OSZAPC
    arithsb_test(neg,0x0,0x0,ZOSZAPC,ZZP,0) ! Z,C
    arithsb_test(neg,0x1,0xff,ZOSZAPC,ZSAPC,0) ! A
    arithsb_test(neg,0x2,0xfe,ZOSZAPC,ZSAC,0) ! P
    arithsb_test(neg,0xf,0xf1,ZOSZAPC,ZSAC,0) ! P
    arithsb_test(neg,0xf0,0x10,ZOSZAPC,ZC,0)
    arithsb_test(neg,0x10,0xf0,ZOSZAPC,ZSPC,0)
    arithsb_test(neg,0x80,0x80,ZOSZAPC,ZOSC,0) ! O

    ! NOT -> nothing
    arithsb_test(not,0xff,0x00,0,0,0)
    arithsb_test(not,0x00,0xff,0,0,0)
    arithsb_test(not,0x01,0xfe,0,0,0)

    !
    ! Shift & rotate
    !

    ! SHL/SAL
    srw_test_one(shl,0x1,0x2,ZOSZPC,0,ZA)
    srw_test_one(shl,0x0,0x0,ZOSZPC,ZZP,ZA)
    srw_test_one(shl,0xffff,0xfffe,ZOSZPC,ZSC,ZA)
    srw_test_one(shl,0x8000,0x0,ZOSZPC,ZOZPC,ZA)
    srw_test_one(shl,0x4000,0x8000,ZOSZPC,ZOSP,ZA)
    ! srw_test uses CL instead of passing an argument for shift count
    srw_test(shl,0x1,0x2,0x4,ZSZPC,0,ZOA)
    srw_test(shl,0x4000,0x2,0x0,ZSZPC,ZZPC,ZOA)
    srw_test(shl,0x0,0x2,0x0,ZSZPC,ZZP,ZOA)
    srw_test(shl,0x8000,0x0,0x8000,0,0,ZOA) ! TODO: it seems that SHL/SHR/... X, 0 does not affect any flag
    srw_test(shl,0x0123,0x4,0x1230,ZSZPC,ZP,ZOA)
    srw_test(shl,0x0001,0xff,0x0000,ZSZPC,ZZP,ZOA)

    ! SHR
    srw_test_one(shr,0x1,0x0,ZOSZPC,ZZPC,ZA)
    srw_test_one(shr,0x0,0x0,ZOSZPC,ZZP,ZA)
    srw_test_one(shr,0xffff,0x7fff,ZOSZPC,ZOPC,ZA)
    srw_test_one(shr,0x8000,0x4000,ZOSZPC,ZOP,ZA)
    srw_test_one(shr,0x4000,0x2000,ZOSZPC,ZP,ZA)
    ! srw_test uses CL instead of passing an argument for shift count
    srw_test(shr,0x1,0x2,0x0,ZSZPC,ZZP,ZOA)
    srw_test(shr,0x2,0x2,0x0,ZSZPC,ZZPC,ZOA)
    srw_test(shr,0x4000,0x2,0x1000,ZSZPC,ZP,ZOA)
    srw_test(shr,0x0,0x2,0x0,ZSZPC,ZZP,ZOA)
    srw_test(shr,0x4,0x2,0x1,ZSZPC,0,ZOA)
    srw_test(shr,0x1230,0x4,0x0123,ZSZPC,0,ZOA)
    srw_test(shr,0x1,0x0,0x1,0,0,ZOA)
    srw_test(shr,0xffff,0xff,0x0000,ZSZPC,ZZP,ZOA)

    ! SAR
    srw_test_one(sar,0x1,0x0,ZOSZPC,ZZPC,ZA)
    srw_test_one(sar,0x0,0x0,ZOSZPC,ZZP,ZA)
    srw_test_one(sar,0xffff,0xffff,ZOSZPC,ZSPC,ZA)
    srw_test_one(sar,0x8000,0xC000,ZOSZPC,ZSP,ZA)
    srw_test_one(sar,0x4000,0x2000,ZOSZPC,ZP,ZA)
    ! srw_test uses CL instead of passing an argument for shift count
    srw_test(sar,0x1,0x2,0x0,ZSZPC,ZZP,ZOA)
    srw_test(sar,0x2,0x2,0x0,ZSZPC,ZZPC,ZOA)
    srw_test(sar,0x4000,0x2,0x1000,ZSZPC,ZP,ZOA)
    srw_test(sar,0x0,0x2,0x0,ZSZPC,ZZP,ZOA)
    srw_test(sar,0x4,0x2,0x1,ZSZPC,0,ZOA)
    srw_test(sar,0x1230,0x4,0x0123,ZSZPC,0,ZOA)
    srw_test(sar,0x8230,0x4,0xf823,ZSZPC,ZS,ZOA)
    srw_test(sar,0xffff,0xff,0xffff,ZSZPC,ZSPC,ZOA)

    ! ROL
    srw_test_one(rol,0x1,0x2,ZOC,0,0)
    srw_test_one(rol,0x8001,0x0003,ZOC,ZOC,0)
    srw_test_one(rol,0x4000,0x8000,ZOC,ZO,0)
    srw_test(rol,0x1,0x2,0x4,ZC,0,ZO)
    srw_test(rol,0x4001,0x2,0x0005,ZC,ZC,ZO)
    srw_test(rol,0x1001,0x3,0x8008,ZC,0,ZO)
    srw_test(rol,0x0001,0xff,0x8000,ZC,0,ZO)
    srw_test(rol,0x0001,0xf0,0x0001,ZC,ZC,ZO)

    ! RCL
    clc
    srw_test_one(rcl,0x1,0x2,ZOC,0,0)
    clc
    srw_test_one(rcl,0x8001,0x0002,ZOC,ZOC,0)
    srw_test_one(rcl,0x8001,0x0003,ZOC,ZOC,0) ! Carry set
    srw_test_one(rcl,0x4000,0x8001,ZOC,ZO,0)  ! Carry unset
    srw_test_one(rcl,0x4000,0x8000,ZOC,ZO,0)
    srw_test_one(rcl,0x8000,0x0000,ZOC,ZOC,0)
    srw_test_one(rcl,0x8000,0x0001,ZOC,ZOC,0) ! Carry set
    srw_test(rcl,0x1,0x2,0x6,ZC,0,ZO)
    srw_test(rcl,0x1,0x2,0x4,ZC,0,ZO)
    srw_test(rcl,0x4001,0x2,0x0004,ZC,ZC,ZO)
    srw_test(rcl,0x4001,0x2,0x0006,ZC,ZC,ZO)
    clc
    srw_test(rcl,0x1001,0x3,0x8008,ZC,0,ZO)
    ! srw_test(rcl,0x0001,0xff,0x4000,ZC,0,ZO) ! Behavior differs between 8086 and 286+
    stc
    ! srw_test(rcl,0x0001,0xff,0x6000,ZC,0,ZO)
    !TODO: test rcr XX, 0x10; test rcr, XX, 0x1Y

    ! ROR
    srw_test_one(ror,0x1,0x8000,ZOC,ZOC,0)
    srw_test_one(ror,0x8001,0xC000,ZOC,ZC,0)
    srw_test_one(ror,0x8000,0x4000,ZOC,ZO,0)
    srw_test(ror,0x4,0x2,0x1,ZC,0,ZO)
    srw_test(ror,0x4001,0x2,0x5000,ZC,0,ZO)
    srw_test(ror,0x8008,0x3,0x1001,ZC,0,ZO)
    srw_test(ror,0x0001,0xff,0x0002,ZC,0,ZO)
    ! TODO: same last test as ROL + carry with 0x8001 and multi-bit rotate

    ! RCR
    clc
    srw_test_one(rcr,0x2,0x1,ZOC,0,0)
    clc
    srw_test_one(rcr,0x8001,0x4000,ZOC,ZOC,0)
    srw_test_one(rcr,0x0003,0x8001,ZOC,ZOC,0) ! Carry set
    srw_test_one(rcr,0x8000,0xC000,ZOC,0,0)  ! Carry unset
    srw_test_one(rcr,0x8000,0x4000,ZOC,ZO,0)
    srw_test_one(rcr,0x0001,0x0000,ZOC,ZC,0)
    srw_test_one(rcr,0x0001,0x8000,ZOC,ZOC,0) ! Carry set
    srw_test(rcr,0x6,0x2,0x4001,ZC,ZC,ZO)
    srw_test(rcr,0x4,0x2,0x4001,ZC,0,ZO)
    srw_test(rcr,0x8002,0x2,0x2000,ZC,ZC,ZO)
    srw_test(rcr,0x8002,0x2,0x6000,ZC,ZC,ZO)
    clc
    srw_test(rcr,0x8008,0x3,0x1001,ZC,0,ZO)
    !srw_test(rcr,0x0001,0xff,0x0008,ZC,0,ZO) ! Different behavior between 8086 and 286+
    !stc
    !srw_test(rcr,0x0001,0xff,0x000C,ZC,0,ZO)

    !
    ! Multiplications & divisions
    !

    ! MUL
    mulw_test(mul,0x3,0x2,0x6,0x0,ZOC,0,ZSZAP)
    mulw_test(mul,0x100,0x42,0x4200,0x0,ZOC,0,ZSZAP)
    mulw_test(mul,0x1000,0x42,0x2000,0x4,ZOC,ZOC,ZSZAP)
    mulw_test(mul,0xffff,0xffff,0x0001,0xfffe,ZOC,ZOC,ZSZAP)
    mulw_test(mul,0x0,0x0,0x0,0x0,ZOC,0,ZSZAP)

    ! IMUL
    mulw_test(imul,0x3,0x2,0x6,0x0,ZOC,0,ZSZAP)
    mulw_test(imul,0x100,0x42,0x4200,0x0,ZOC,0,ZSZAP)
    mulw_test(imul,0x1000,0x42,0x2000,0x4,ZOC,ZOC,ZSZAP)
    mulw_test(imul,0x4000,0x2,0x8000,0x0,ZOC,ZOC,ZSZAP)
    mulw_test(imul,0xb000,0x2,0x6000,0xffff,ZOC,ZOC,ZSZAP)
    mulw_test(imul,0xffff,0x2,0xfffe,0xffff,ZOC,0,ZSZAP)
    mulw_test(imul,0xffff,0xffff,0x0001,0x0,ZOC,0,ZSZAP)
    mulw_test(imul,0x8000,0x2,0x0,0xffff,ZOC,ZOC,ZSZAP)
    mulw_test(imul,0x8000,0x8000,0x0,0x4000,ZOC,ZOC,ZSZAP)
    mulw_test(imul,0x0,0x0,0x0,0x0,ZOC,0,ZSZAP)

    ! DIV
    call #set_div_by_zero_expected
    divw_test(div,0x0,0x1,0x0,0x0bad,0x0)
    call #set_div_by_zero_unexpected
    !divw_test(div,0x0,0x2,0x0,0x0bad,0x0) ! Uncomment to test div by zero reset
    divw_test(div,0x0,0xffff,0x2,0x7fff,0x1)
    call #set_div_by_zero_expected
    divw_test(div,0xffff,0xffff,0x2,0x0bad,0x0)
    call #set_div_by_zero_unexpected
    divw_test(div,0x0100,0x0000,0x1000,0x1000,0x0)
    divw_test(div,0x0100,0x0123,0x1000,0x1000,0x123)

    ! IDIV
    call #set_div_by_zero_expected
    divw_test(idiv,0x0,0x1,0x0,0x0bad,0x0)
    call #set_div_by_zero_unexpected
    divw_test(idiv,0x0,0xffff,0x2,0x7fff,0x1)
    call #set_div_by_zero_expected
    divw_test(idiv,0xffff,0xffff,0x2,0x0,0xffff)
    call #set_div_by_zero_unexpected
    divw_test(idiv,0x0100,0x0000,0x1000,0x1000,0x0)
    divw_test(idiv,0x0100,0x0123,0x1000,0x1000,0x123)
    divw_test(idiv,0xffff,0x0,0x3,0xaaab,0xffff)
    divw_test(idiv,0x0,0x7fff,0x1,0x7fff,0)
    divw_test(idiv,0xffff,0x8001,0x1,0x8001,0)
    call #set_div_by_zero_expected
    divw_test(idiv,0x0,0x8000,0x1,0x0bad,0)
    call #set_div_by_zero_expected
    ! 0xffff8000 / 0x1 -> division error according to page 52 of
    ! the 8086 user manual. Minimum quotient is 8001 and not 8000.
    ! This is not what QEMU (and dosbox) say, so avoiding the problem for now...
    !divw_test(idiv,0xffff,0x8000,0x1,0x0bad,0)
    call #set_div_by_zero_unexpected

    ! TODO: CMP
    
skip_to_strops:
    !
    ! String operations
    !
    movsw_test(movsw_1,"MOVSW 1: ",,1,2)
    movsw_test(movsw_2,"MOVSW 2: ",rep,1,2)
    movsw_test(movsw_3,"MOVSW 3: ",rep,2,4)
    movsw_test(movsw_0,"MOVSW 0: ",rep,0,0)

    cmpsw_test(cmpsw_1,"CMPSW x x: ",,1,stropsdata,1,2,ZZP)
    cmpsw_test_rev(cmpsw_1,,1,stropsdata,1,2,ZZP)
    cmpsw_test(cmpsw_2,"REPE CMPSW x x: ",repe,5,stropsdata,0,10,ZZP)
    cmpsw_test_rev(cmpsw_2,repe,5,stropsdata,0,10,ZZP)
    cmpsw_test(cmpsw_3,"REPE CMPSW xy x: ",repe,5,stropsdata2,1,8,ZSP)
    cmpsw_test_rev(cmpsw_3,repe,5,stropsdata2,4,2,ZSAP)
    cmpsw_test(cmpsw_4,"REPE CMPSW x x with suffix: ",repe,3,stropsdata2,0,6,ZZP)
    cmpsw_test_rev(cmpsw_4,repe,3,stropsdata2,2,2,ZSAP)
    cmpsw_test(cmpsw_5,"REPE CMPSW x xy: ",repe,5,stropsdata3,0,10,ZAPC)
    cmpsw_test_rev(cmpsw_5,repe,5,stropsdata3,4,2,ZSAPC)
    cmpsw_test_rev(cmpsw_5eq,repe,4,stropsdata3eq,0,8,ZZP)

    cmpsw_test(cmpsw_6,"REPNE CMPSW x x: ",repne,4,stropsdata,3,2,ZZP)
    cmpsw_test_rev(cmpsw_6,repne,4,stropsdata,3,2,ZZP)
    cmpsw_test(cmpsw_7,"REPNE CMPSW xy x: ",repne,4,stropsdata2,3,2,ZZP)
    cmpsw_test_rev(cmpsw_7,repne,4,stropsdata2,0,8,0)
    cmpsw_test_rev(cmpsw_7neq,repne,1,stropsdata2,0,2,ZSAP)
    cmpsw_test(cmpsw_8,"REPNE CMPSW x xy: ",repne,4,stropsdata3,3,2,ZZP)
    cmpsw_test_rev(cmpsw_8,repne,4,stropsdata3eq,3,2,ZZP)
    cmpsw_test_rev(cmpsw_8eq,repne,1,stropsdata3eq,0,2,ZZP)
    call stz
    cmpsw_test_rev(cmpsw_8eq_wz,repne,1,stropsdata3eq,0,2,ZZP)
    call clz
    cmpsw_test_rev(cmpsw_8eq_woz,repne,1,stropsdata3eq,0,2,ZZP)

    lodsw_test(lodsw_1)

    scasw_test(scasw_1,"SCASW _x: ",,1,stropsdata,0x1234,1,2,ZZP)
    scasw_test_rev(scasw_1,,1,stropsdata,0x1234,1,2,ZC)
    scasw_test(scasw_2,"SCASW x: ",,1,stropsdata,0x1235,1,2,0)
    scasw_test_rev(scasw_2,,1,stropsdata,0x1235,1,2,ZPC)
    scasw_test(scasw_3,"REPNE SCASW x_x: ",repne,4,stropsdata,0x5678,2,4,ZZP)
    scasw_test_rev(scasw_3,repne,4,stropsdata,0x5678,1,6,ZZP)
    scasw_test(scasw_4,"REPNE SCASW x_: ",repne,4,stropsdata,0xBC00,0,8,ZZP)
    scasw_test_rev(scasw_4,repne,4,stropsdata,0xBC00,3,2,ZZP)
    scasw_test(scasw_5,"REPNE SCASW x _: ",repne,3,stropsdata,0xBC00,0,6,ZSAP)
    scasw_test_rev(scasw_5,repne,3,stropsdata,0xBC00,2,2,ZZP)
    scasw_test(scasw_6,"REPNE SCASW empty: ",repne,0,stropsdata,0xBC00,0,0,ZZP) ! Same flag as before (instruction not run)
    scasw_test_rev(scasw_6,repne,0,stropsdata,0xBC00,0,0,ZZP)
    scasw_test(scasw_7,"REPE SCASW _x: ",repe,4,stropsdata,0x1234,2,4,ZSAC)
    scasw_test_rev(scasw_7,repe,4,stropsdata,0x1234,3,2,ZC)
    scasw_test(scasw_8,"REPE SCASW _: ",repe,1,stropsdata,0x1234,0,2,ZZP)
    scasw_test_rev(scasw_8,repe,1,stropsdata,0x1234,0,2,ZC)
    scasw_test(scasw_9,"REPE SCASW diff: ",repe,4,stropsdata,0x1235,3,2,0)
    scasw_test_rev(scasw_9,repe,4,stropsdata,0x1235,3,2,ZPC)
    scasw_test(scasw_10,"REPE SCASW __: ",repe,3,stropseqdata,0x4242,0,6,ZZP)
    scasw_test_rev(scasw_10,repe,3,stropseqdata,0x4242,0,6,ZZP)
    scasw_test(scasw_11,"REPE SCASW __x: ",repe,4,stropseqdata,0x4242,0,8,ZP)
    scasw_test_rev(scasw_11,repe,4,stropseqdata,0x4242,0,8,ZP)

    stosw_test(stosw_1,"STOS: ",,1,2,0x4242,stropseqdata)
    stosw_test(stosw_2,"REP STOS x0: ",rep,0,0,0x4242,stropseqdata)
    stosw_test(stosw_3,"REP STOS x1: ",rep,1,2,0x4242,stropseqdata)
    stosw_test(stosw_4,"REP STOS x3: ",rep,3,6,0x4242,stropseqdata)

    !
    ! Operand access
    !
    puts(optest,,"Indirect word with 16-bit unsigned displacement: ")
    mov bx, #0x42 ! Push immediate not available on 8086 processors
    push bx
    mov bx, sp
    sub bx, #0x1002
    mov ax, [bx + 0x1002]
    mov bx, #0x42
    cmp ax, bx
    call assert_equal_value
    call success

    puts(optestb,,"Indirect byte with 16-bit unsigned displacement: ")
    mov bx, #0x0
    push bx
    mov bx, sp
    mov byte [bx], #0x42
    sub bx, #0x1002
    xor ax, ax
    mov al, [bx + 0x1002]
    xor bx, bx
    mov bl, #0x42
    cmp al, bl
    call assert_equal_value
    call success
    pop bx ! Pop 0x0

    ! LDS
    puts(lds,,"LDS: ")
    mov bx, #0x1234 ! Address
    push bx
    mov bx, #0x5678 ! Segment
    push bx
    mov bx, sp

    lds ax, [bx]
    mov cx, ds ! Save DS before restoring it
    mov dx, #DATA_SEG ! Restore DS
    mov ds, dx

    mov bx, #0x5678
    cmp ax, bx
    call assert_equal_value
    mov ax, cx
    mov bx, #0x1234
    cmp ax, bx
    call assert_equal_dsvalue
    call success
    pop bx
    pop bx

    ! Indirect addressing
    indw_idx_test(BX,SI,0x1,0x1111,0)
    indw_idx_test(BX,DI,0x2,0x2222,0)
    indw_idx_test(BP,SI,0x3,0x3333,0)
    indw_idx_test(BP,DI,0x4,0x4444,0)
    indw_test(BX,0x5,0x5555,0) ! 8i positive
    indw_test(BP,0x6,0x6666,0)
    indw_test(SI,0x7,0x7777,0)
    indw_test(DI,0x8,0x8888,0)
    indw_test(BX,-0x9,0x9999,0) ! 8i negative
    indw_test(BP,-0xa,0xaaaa,0)
    indw_test(SI,-0xb,0xbbbb,0)
    indw_test(DI,-0xc,0xcccc,0)
    indw_test(BX,0x111,0x1110,0) ! 16u
    indw_test(BP,0x222,0x2220,0)
    indw_test(SI,0x333,0x3330,0)
    indw_test(DI,0x444,0x4440,0)

    ! Indirect addressing with SS != DS
    ! To test this, change SS to a value that will advance
    ! the stack pointer by 32 bytes. 
    ! Test value are set using push/SP, so tests using BP will
    ! fail if the processor does not use SP as reference.
    puts(displ,,"Displacing SP...\\r\\n")
    mov ax, #0x07be
    mov ss, ax
    indw_idx_test(BX,SI,0x1,0x1111,0x20)
    indw_idx_test(BX,DI,0x2,0x2222,0x20)
    indw_idx_test(BP,SI,0x3,0x3333,0x0)
    indw_idx_test(BP,DI,0x4,0x4444,0x0)
    indw_test(BX,0x5,0x5555,0x20) ! 8i positive
    indw_test(BP,0x6,0x6666,0x0)
    indw_test(SI,0x7,0x7777,0x20)
    indw_test(DI,0x8,0x8888,0x20)
    indw_test(BX,-0x9,0x9999,0x20) ! 8i negative
    indw_test(BP,-0xa,0xaaaa,0x0)
    indw_test(SI,-0xb,0xbbbb,0x20)
    indw_test(DI,-0xc,0xcccc,0x20)
    indw_test(BX,0x111,0x1110,0x20) ! 16u
    indw_test(BP,0x222,0x2220,0x0)
    indw_test(SI,0x333,0x3330,0x20)
    indw_test(DI,0x444,0x4440,0x20)

    ! CMPS with segment override
    puts(segovermovs,,"CMPS with segment override: ")
    mov ax, #0x3245
    push ax
    mov si, sp
    mov di, sp
    sub di, #0x30 ! Override should not apply to DI; SS+3=DS+1=ES
    seg ss
    cmps
    je segovermovs_success
    puts(segovermovs_fail,r,"Failure")
    call _end
segovermovs_success:
    call success


    ! TODO: LEA/LDS/LES with BP, with immediate/indirect/indirect with displacement 8/16...
    ! TODO: ensure that DI used as destination for string operations uses ES
    ! TODO: XCHG
    ! TODO: LEA
    ! TODO: LOOP/LOOPZ/LOOPNZ
    ! TODO: far calls?
    ! TODO: CBW / CWD
    ! TODO: TEST
    ! TODO: segment override
    ! TODO: JBE/JNBE JLE JNLE JO JNO
    ! TODO: RETANDADDTOSP
    ! TODO: SAHF/LAHF
    ! JL/JNL
    ! DAA
    puts(spres,,"Resetting SP...\\r\\n")
    mov ax, #STACK_SEG
    mov ss, ax

    ! TODO: String operations (MOVS, LODS, CMPS...)

    jmp _end

_end:
    jmp _end

.org $167FFF
    ret ! to pad the executable to 1,4Mb

