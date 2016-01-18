success_str:
    .asciz "Success\r\n"

success:
    mov si, #success_str
    call gprint
    ret


! Prints 'F' is the flag is set, 'f' otherwise
! Arg #1: the flag to test
! Arg #2: the ASCII value of the flag (for display)
! AX: register that contains the flags to test
MACRO print_flag
    mov bx, #?2
    test ax, #?1
    jnz print_flag_?1_set
    add bx, #0x20 ! 'a'-'A'
print_flag_?1_set:
    push ax
    mov ax, bx
    call print_char
    pop ax
MEND

! Display the flags on screen in a readable manner
! AX: register that contains the flags to test
print_flags:
    push bx
    print_flag(ZO, 0x4f)
    print_flag(ZD, 0x44)
    print_flag(ZI, 0x49)
    print_flag(ZT, 0x54)
    print_flag(ZS, 0x53)
    print_flag(ZZ, 0x5A)
    print_flag(ZA, 0x41)
    print_flag(ZP, 0x50)
    print_flag(ZC, 0x43)
    pop bx
    ret

GET asciinums.inc
! Display the given number on screen
! BL: register that contains the number to display
print_numb:
    push ax
    push bx
    add bx, bx ! BX *= 2
    mov al, [bx + asciinums]
    call print_char
    add bx, #1
    mov al, [bx + asciinums]
    call print_char
    pop bx
    pop ax
    ret

! Display the given number on screen
! AX: register that contains the number to display
print_numw:
    push ax
    push bx
    xor bx, bx
    mov bl, ah ! BX = AX & 0xff
    call print_numb
    mov bl, al
    call print_numb
    pop bx
    pop ax
    ret

! Save flags so assert_flags() can check that unaffected
! flags are indeed unaffected
! No arguments
! M: AX
MACRO save_flags
    push ax
    pushf
    pop ax
    mov save_flags_saved, ax
    pop ax
MEND
save_flags_saved:
    .word 0

! Verify that the current CPU flags match some constraints
! Arg #1: Affected flags
! Arg #2: Flags that are expected to be set
! Arg #3: Undefined flags (will not be checked)
!
! Flags that are not affected will be checked against the 
! flags that were saved by save_flags()
! M: SI, F
MACRO assert_flags
    mov word  [affected_flags], #?1
    mov word       [set_flags], #?2
    mov word [undefined_flags], #?3
    call assert_flags_fct
MEND

affected_flags:
    .word 0
set_flags:
    .word 0
undefined_flags:
    .word 0
assert_flags_fct:
    push ax
    push bx
    push cx

    pushf
    pop bx

    ! Check that affected flags match
    mov ax, bx
    and ax, [affected_flags]
    cmp ax, [set_flags]
    mov ax, bx ! Actual
    mov bx, [set_flags] ! Expected. Now only AX contains the original flags
    call assert_equal_flags_modified

    ! Now check that unaffected flags match
    mov bx, [affected_flags] ! BX = affected flags
    or  bx, [undefined_flags] ! BX = affected and unchecked flags
    xor bx, #0xffff ! BX = unaffected flags + nonsense flags
    !and bx, #%0000111111010101 ! BX = unaffected flags; should not be needed
    mov cx, ax ! Save AX, initial actual flags
    xor ax, [save_flags_saved] ! AX = flags ^ initial flags = modified flags
    and ax, bx  ! AX = modified flags & expected unaffected flags, which should be 0
    cmp ax, #0
    mov bx, [save_flags_saved] ! Expected
    mov ax, cx ! Actual (restore CX)
    call assert_equal_flags_unmodified

    pop cx
    pop bx
    pop ax
    ret

! Helper to define a test case, automatically handling print & flag save/restore
! Arg #1: unique ID of the test
! Arg #2: string to display when running the test
! Arg #3,4,5: Affected,expected,undefined flags
! Expect the following functions to be defined:
!  run_?1: actually runs the test
!  checkval_?1: checks the value of the test
MACRO test_sugar
    pushf ! Save previous flags for when the instruction we test requires it
    jmp #t_?1
s_?1:
    .asciz ?2
t_?1:
    mov si, #s_?1
    call print
    popf ! Restore the flags
    save_flags()
    ! Run the tested instruction
    call run_?1
    pushf ! For later restore
    ! Assert value
    call checkval_?1
    popf ! Restore flags and save again
    pushf
    ! Assert flags
    assert_flags(?3,?4,?5)
    call success
    popf ! Restore actual flags for following test
    jmp end_?1
MEND


! TODO: make assert_equal_value run 'cmp' itself

! Arithmetic operations with two operands
MACRO arith_test
    test_sugar(arith_test_?1_?2_?3_?4,"Running test ?1 ?2 ?3 == ?4: ",?5,?6,?7)

run_arith_test_?1_?2_?3_?4:
    mov ax, #?2
    ?1 ax, #?3
    ret
checkval_arith_test_?1_?2_?3_?4:
    ! AX = actual
    mov bx, #?4 ! Expected
    call assert_equal_value
    ret
end_arith_test_?1_?2_?3_?4:
MEND

! Arithmetic operations with two operands
MACRO arithb_test
    test_sugar(arithb_test_?1_?2_?3_?4,"Running byte-operand test ?1 ?2 ?3 == ?4: ",?5,?6,?7)

run_arithb_test_?1_?2_?3_?4:
    mov ax, #0
    mov al, #?2
    ?1 al, #?3
    ret
checkval_arithb_test_?1_?2_?3_?4:
    ! AX = actual
    mov bx, #0
    mov bl, #?4 ! Expected
    call assert_equal_value
    ret
end_arithb_test_?1_?2_?3_?4:
MEND


MACRO mulw_test
    test_sugar(mulw_test_?1_?2_?3_?4,"Running test ?1 ?2 ?3 == ?5|?4: ",?6,?7,?8)
run_mulw_test_?1_?2_?3_?4:
    mov ax, #?2
    mov bx, #?3
    ?1 bx
    ret
checkval_mulw_test_?1_?2_?3_?4:
    ! AX = actual
    mov bx, #?4 ! Expected
    call assert_equal_value
    mov ax, dx ! Actual DX
    mov bx, #?5 ! Expected DX
    call assert_equal_dxvalue
    ret
end_mulw_test_?1_?2_?3_?4:
MEND

MACRO divw_test
    test_sugar(divw_test_?1_?2_?3_?4,"Running test ?1 ?2|?3 ?4 == ?5 rem ?6: ",0,0,ZOSZAPC)
run_divw_test_?1_?2_?3_?4:
    mov dx, #?2
    mov ax, #?3
    mov bx, #?4
    ?1 bx
    ret
checkval_divw_test_?1_?2_?3_?4:
    ! AX = actual
    mov bx, #?5 ! Expected quotient
    call assert_equal_value
    mov ax, dx ! Actual DX
    mov bx, #?6 ! Expected DX (remainder)
    call assert_equal_dxvalue
    ret
end_divw_test_?1_?2_?3_?4:
MEND

! Arithmetic operations with a single operand
MACRO ariths_test
test_sugar(ariths_test_?1_?2_?3,"Running test ?1 ?2 == ?3: ",?4,?5,?6)
run_ariths_test_?1_?2_?3:
    mov ax, #?2
    ?1 ax
    ret
checkval_ariths_test_?1_?2_?3:
    ! AX = actual
    mov bx, #?3 ! Expected
    call assert_equal_value
    ret
end_ariths_test_?1_?2_?3:
MEND


! Arithmetic operations with a single operand
MACRO arithsb_test
test_sugar(arithsb_test_?1_?2_?3,"Running test ?1 ?2 == ?3: ",?4,?5,?6)
run_arithsb_test_?1_?2_?3:
    mov ax, #0
    mov al, #?2
    ?1 al
    ret
checkval_arithsb_test_?1_?2_?3:
    ! AX = actual
    mov bx, #0
    mov bl, #?3 ! Expected
    call assert_equal_value
    ret
end_arithsb_test_?1_?2_?3:
MEND

! Indirect operands
! indw_idx_test(reg,idx_reg,off,val,spdispl)
MACRO indw_idx_test
test_sugar(indw_idx_test_?1_?2_?3_?4_?5,"Running indirect addressing test [?1+?2=?3]: ",0,0,0)
run_indw_idx_test_?1_?2_?3_?4_?5:
    mov ax, #?4
    push ax
    mov ?1, sp
    pushf
    sub ?1, #?3
    sub ?1, #?5 ! SP displacement relative to the usual reference register
    popf
    mov ?2, #?3
    mov ax, [?1+?2]
    pop dx ! Trash the pushed value, and DX seems to be a good trash bin
    ret
checkval_indw_idx_test_?1_?2_?3_?4_?5:
    mov bx, #?4
    call assert_equal_value
    ret
end_indw_idx_test_?1_?2_?3_?4_?5:
MEND

! indw_test(reg,off,val,spdispl)
MACRO indw_test
test_sugar(indw_test_?1_?3_?4,"Running indirect addressing test [?1+?2]: ",0,0,0)
run_indw_test_?1_?3_?4:
    mov ax, #?3
    push ax
    mov ?1, sp
    pushf
    sub ?1, #?2
    sub ?1, #?4 ! SP displacement relative to the usual reference register
    popf
    mov ax, [?1+?2]
    pop dx ! Trash the pushed value, and DX seems to be a good trash bin
    ret
checkval_indw_test_?1_?3_?4:
    mov bx, #?3
    call assert_equal_value
    ret
end_indw_test_?1_?3_?4:
MEND

! sr_test(instruction,val,count,result,flags*)
MACRO srw_test
test_sugar(srw_test_?1_?2_?3_?4,"Running shift-rotate test ?1 ?2 ?3 == ?4: ",?5,?6,?7)
run_srw_test_?1_?2_?3_?4:
    mov ax, #?2
    mov cl, #?3
    ?1 ax, cl
    ret
checkval_srw_test_?1_?2_?3_?4:
    mov bx, #?4
    cmp ax,bx
    call assert_equal_value
    ret
end_srw_test_?1_?2_?3_?4:
MEND

MACRO srw_test_one
    pushf
    srw_test(?1,?2,0x1,?3,?4,?5,?6)
    popf ! The carry flag can be used by RCR/RCL
    arith_test(?1,?2,0x1,?3,?4,?5,?6)
MEND

expected_str:
    .asciz "\r\n Expected: "
actual_str:
    .asciz     " Actual:   "

! Defines a 'assert_equal_?1' function which tests the 'EQUAL' flag
! Arg #1: suffix of the name of the function
! Arg #2: string to show in case of failure
! Arg #3: function to call to print expected/actual values
!          -> should take the value in AX
! Defined functions:
! AX: actual value (for printing)
! BX: expected value (for printing)
MACRO def_assert_equal
fail_?1_str:
    .asciz ?2
assert_equal_?1:
    cmp ax, bx
    push ax
    push bx
    push si
    je assert_equal_ok_?1
    ! Failure case
    mov si, #fail_?1_str
    call rprint
    mov si, #actual_str
    call print
    call ?3
    mov si, #expected_str
    call print
    mov ax, bx
    call ?3
    call _end
assert_equal_ok_?1:
    pop si
    pop bx
    pop ax
    ret
MEND

! Same, without CMP (useful for flags)
MACRO def_assert_equal_wocmp
fail_?1_str:
    .asciz ?2
assert_equal_?1:
    push ax
    push bx
    push si
    je assert_equal_ok_?1
    ! Failure case
    mov si, #fail_?1_str
    call rprint
    mov si, #actual_str
    call print
    call ?3
    mov si, #expected_str
    call print
    mov ax, bx
    call ?3
    call _end
assert_equal_ok_?1:
    pop si
    pop bx
    pop ax
    ret
MEND

def_assert_equal_wocmp(flags_modified, "Failure: modified flags do not match\\r\\n", print_flags)
def_assert_equal_wocmp(flags_unmodified, "Failure: unaffected flags were modified\\r\\n", print_flags)
def_assert_equal(value, "Failure: unexpected result\\r\\n", print_numw)
def_assert_equal(dxvalue, "Failure: unexpected result in DX\\r\\n", print_numw)
def_assert_equal(dsvalue, "Failure: unexpected result in DS\\r\\n", print_numw)
def_assert_equal(sivalue, "Failure: unexpected result in SI\\r\\n", print_numw)
def_assert_equal(divalue, "Failure: unexpected result in DI\\r\\n", print_numw)
def_assert_equal(strvalue, "Failure: unexpected result in string\\r\\n", print_numw)
def_assert_equal(cxvalue, "Failure: unexpected result in CX\\r\\n", print_numw)
