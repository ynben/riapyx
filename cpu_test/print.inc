gprint:
    push bx
    mov bx, #0xa
    call cprint_str
    pop bx
    ret

rprint:
    push bx
    mov bx, #0xc
    call cprint_str
    pop bx
    ret

print:
    push bx
    mov bx, #0x7
    call cprint_str
    pop bx
    ret

! Pass the requested color into BL
cprint_str:
    push ax
    push bx
    pushf
    cld
cprint_loop:
    lodsb
    mov ah, #$0E
    or al, al
    jz cprint_str_end
    int #0x10
    jmp cprint_loop
cprint_str_end:
    popf
    pop bx
    pop ax
    ret

! print_char: prints the char given in AL
print_char:
    push ax
    push bx
    mov ah, #0xe
    mov bx, #0x7
    int #0x10
    pop bx
    pop ax
    ret

! Helper for printing
! Arg #1: Unique ID
! Arg #2: r(ed)/g(reen)/nothing (gray)
! Arg #3: string to print
MACRO puts
    jmp ?1_cont
?1_str:
    .asciz ?3
?1_cont:
    mov si, #?1_str
    call ?2print
MEND

