DI_DISPL = 0x10

STR_WCNT=0x4
STR_SIZE=0x8

zero:
    .word 0
MACRO di_alloca
    ! WARNING: update if STR_SIZE changes (=0x8)

    push word zero ! Immediate addressing with PUSH is not supported on the 8086
    push word zero
    push word zero
    push word zero
    mov di, sp
    sub di, #DI_DISPL
MEND

MACRO di_alloca_rev
    ! WARNING: update if STR_SIZE changes (=0x8)
    push word zero
    mov di, sp
    sub di, #DI_DISPL
    push word zero
    push word zero
    push word zero
MEND

MACRO di_free
    add sp, #STR_SIZE
MEND

stropsdatapref:
        .word 0x0000
stropsdata:
        .word 0x1234
        .word 0x5678
        .word 0x009A
stropsdata_rev:
        .word 0xBC00
        .word 0x0000

! Shorter
stropsdata2pref:
        .word 0x0000
stropsdata2:
        .word 0x1234
        .word 0x5678
stropsdata2_rev:
        .word 0x009A
        .word 0x0000

! Longer
stropsdata3ref:
        .word 0x0000
stropsdata3:
        .word 0x1234
        .word 0x5678
        .word 0x009A
stropsdata3eq_rev:
        .word 0xBC00
stropsdata3_rev:
        .word 0xBD9B
        .word 0x0000

! Equal
stropseqdataref:
        .word 0x0000
stropseqdata:
        .word 0x4242
        .word 0x4242
stropseqdata_rev:
        .word 0x4242
        .word 0x0000

stropsnulldata:
        .word 0x0000
        .word 0x0000
        .word 0x0000
stropsnulldata_rev:
        .word 0x0000
        .word 0x0000

! run_and_test_flags(prefix,instruction,affected,expected,undefined)
MACRO run_and_test_flags
    save_flags()
    ?1
    ?2
    assert_flags(?3,?4,?5)
MEND

! check_string_content
! SI: first string
! DI: second string
! CX: count (bytes)
! Does not use CMPS as we may use this function to test it
check_string_content_func:
    ! Use BX to replace DI
    ! We should be able to use DI as it is not used with
    ! to ES in MOV instructions, but this is a test program
    ! so we should not assume that in test code
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    mov bx, di
check_string_content_start:
    cmp cx, #0
    je check_string_content_end
    push bx
    xor ax, ax
    mov al, [bx] ! AL = [DST] = Actual
    xor bx, bx
    mov bl, [si] ! BL = [SRC] = Expected
    call assert_equal_strvalue
    pop bx
    inc si
    inc bx
    dec cx
    jmp check_string_content_start
check_string_content_end:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

MACRO set_si
    mov si, #stropsdata
    push si
MEND

MACRO set_si_rev
    mov si, #stropsdata
    add si, #STR_SIZE
    sub si, #2
    push si
MEND

! Call _BEFORE_ set_si
MACRO set_di
    di_alloca()
    push di
MEND

MACRO set_di_rev
    di_alloca_rev()
    push di
MEND

! check_si(size)
MACRO check_si
    pop bx ! BX = SI_orig
    mov ax, si ! AX = SI_after
    sub ax, #?1
    call #assert_equal_sivalue
MEND

MACRO check_si_rev
    pop bx ! BX = SI_orig
    mov ax, si ! AX = SI_after
    add ax, #?1
    call #assert_equal_sivalue
MEND

! check_di(size)
MACRO check_di
    pop bx ! BX = DI_orig
    mov ax, di ! AX = DI_after
    sub ax, #?1
    call #assert_equal_divalue
MEND

MACRO check_di_rev
    pop bx ! BX = DI_orig
    mov ax, di ! AX = DI_after
    add ax, #?1
    call #assert_equal_divalue
MEND

! check_cx(expected)
MACRO check_cx
    push ax
    push bx
    mov ax, cx
    mov bx, #?1
    call #assert_equal_cxvalue
    pop bx
    pop ax
MEND

! check_cx_with_size(initial,expected_size)
! no prefix: same cx
MACRO check_cx_with_size_
    push ax
    push bx
    mov ax, cx
    mov bx, #?1
    call #assert_equal_cxvalue
    pop bx
    pop ax
MEND
! rep prefix: updated CX
MACRO check_cx_with_size_rep
    push ax
    push bx
    mov ax, cx
    mov bx, #?2 ! BX = expected = initial - expected_size / 2
    shr bx, #1
    neg bx
    add bx, #?1
    call #assert_equal_cxvalue
    pop bx
    pop ax
MEND

! check_content(size_modified)
MACRO check_content
    add di, #DI_DISPL ! Revert the displacement for DI

    ! Modified
    sub di, #?1
    sub si, #?1
    mov cx, #?1
    call #check_string_content_func

    ! Unmodified
    add di, #?1
    mov si, #stropsnulldata
    mov cx, #STR_SIZE
    sub cx, #?1 ! CX = unmodified count
    call #check_string_content_func

MEND

MACRO check_content_rev
    add di, #DI_DISPL ! Revert the displacement for DI

    ! Modified
    add si, #2 ! SI and DI were decremented after the last operation
    add di, #2 ! so revert that
    mov cx, #?1
    call #check_string_content_func

    ! Unmodified; check_string_content preserved registers
    sub di, #STR_SIZE ! Unmodified data are at the beginning of DI
    add di, #?1       ! Reposition at beginning of DI
    mov si, #stropsnulldata
    mov cx, #STR_SIZE
    sub cx, #?1 ! CX = unmodified count
    call #check_string_content_func
MEND

! movsw_test(uid,str,prefix,count,size)
MACRO movsw_test
    cld
    puts(?1,,?2)
    set_di()
    set_si()
    mov cx, #?4
    run_and_test_flags(?3,movsw,0,0,0)
    check_cx_with_size_?3(?4,?5)
    check_si(?5)
    check_di(?5)
    check_content(?5)
    di_free()
    call success
    std
    puts(?1_rev,,"... reversed: ")
    set_di_rev()
    set_si_rev()
    mov cx, #?4
    run_and_test_flags(?3,movsw,0,0,0)
    check_cx_with_size_?3(?4,?5)
    check_si_rev(?5)
    check_di_rev(?5)
    check_content_rev(?5)
    di_free()
    cld
    call success
MEND

! stosw_test(uid,str,prefix,count,size,elem,refstr)
MACRO stosw_test
    cld
    puts(?1,,?2)
    set_di()
    mov cx, #?4
    mov ax, #?6
    run_and_test_flags(?3,stosw,0,0,0)
    check_cx_with_size_?3(?4,?5)
    check_di(?5)
    mov si, #?7
    add si, #?5 ! check_content expects the SI pointer to already be incremented
    check_content(?5)
    di_free()
    call success
    std
    puts(?1_rev,,"... reversed: ")
    set_di_rev()
    mov cx, #?4
    mov ax, #?6
    run_and_test_flags(?3,stosw,0,0,0)
    check_cx_with_size_?3(?4,?5)
    check_di_rev(?5)
    mov si, #?7_rev
    sub si, #?5
    check_content_rev(?5)
    di_free()
    cld
    call success
MEND

! TODO: using check_cx_with_size_*, we should be able to factor *_test and *_test_rev

! cmpsw_test(uid,str,prefix,count,comparand,expected_cx,expected_size,expected_flags)
MACRO cmpsw_test
    cld
    puts(?1,,?2)
    mov di, #?5
    pushf
    sub di, #0x10
    popf
    push di
    set_si()
    mov cx, #?4
    run_and_test_flags(?3,cmpsw,ZOSZAPC,?8,0)
    check_cx(?6)
    check_si(?7)
    check_di(?7)
    call success
MEND

! cmpsw_test_rev(uid,prefix,count,comparand,expected_cx_rev,expected_size_rev,expected_flags_rev)
! Split because as86 does not support more than 9 arguments
MACRO cmpsw_test_rev
    std
    puts(?1_rev,,"... reversed: ")
    mov di, #?4_rev
    pushf
    sub di, #0x10
    popf
    push di
    set_si_rev()
    mov cx, #?3
    run_and_test_flags(?2,cmpsw,ZOSZAPC,?7,0)
    check_cx(?5)
    check_si_rev(?6)
    check_di_rev(?6)
    cld
    call success
MEND

! losdw_test(uid)
MACRO lodsw_test
    puts(?1,,"LODS test: ")
    set_si()
    run_and_test_flags(,lodsw,0,0,0)
    mov bx, #0x1234
    call assert_equal_value
    check_si(2)
    call success
    puts(?1_rev,,"... reversed: ")
    std
    set_si_rev()
    run_and_test_flags(,lodsw,0,0,0)
    mov bx, #0xBC00
    cld
    call assert_equal_value
    check_si_rev(2)
    call success
MEND

! scasw_test(uid,str,prefix,count,teststr,comparand,expected_cx,expected_size,expected_flags)
MACRO scasw_test
    puts(?1,,?2)
    mov di, #?5
    pushf
    sub di, #0x10
    popf
    push di
    mov cx, #?4
    mov ax, #?6
    run_and_test_flags(?3,scasw,ZOSZAPC,?9,0)
    mov bx, #?6
    call assert_equal_value
    check_cx(?7)
    check_di(?8)
    call success
MEND

! scasw_test(uid,prefix,count,teststr,comparand,expected_cx_rev,expected_size_rev,expected_flags_rev)
MACRO scasw_test_rev
    std
    puts(?1_rev,,"... reversed: ")
    mov di, #?4_rev
    pushf
    sub di, #0x10
    popf
    push di
    mov cx, #?3
    mov ax, #?5
    run_and_test_flags(?2,scasw,ZOSZAPC,?8,0)
    mov bx, #?5
    call assert_equal_value
    check_cx(?6)
    check_di_rev(?7)
    cld
    call success
MEND

clz:
    push ax

    pushf
    pop ax
    and ax, #0xcf
    push ax
    popf

    pop ax
    ret

stz:
    push ax

    pushf
    pop ax
    or ax, #0x40
    push ax
    popf

    pop ax
    ret
