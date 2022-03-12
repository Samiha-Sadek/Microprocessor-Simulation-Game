.286
Hex_Str MACRO HexNum, HexString
    Local Repeat
    Local addseven
    Local SaveString
    pusha
    pushf
    lea di, HexString
    add di, 3           ;start from the least significant byte
    mov ax, HexNum
    mov cx, 4
Repeat:
    mov dx, 0
    mov bx, 16
    Div bx              ;remainder in dx
    add dx, 30h
    cmp dx, 57
    ja addseven
    jmp SaveString
addseven:
    add dx, 7
SaveString:
    mov [di], dl
    dec di
    loop Repeat
    popf
    popa
ENDM