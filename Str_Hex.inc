.286
Str_Hex MACRO HexString, HexNum
    Local Continue
    Local Convert1
    Local Convert2
    Local Convert3
    Local Convert4
    Local Convert5
    Local subseven
    Local subseven1
    Local subseven2
    Local subseven3
    Local ThisIsTheEnd
    Local aaaaaaaa
    Local bbbbbbbbb
    Local cccccccccc
    pusha
    pushf
    mov cx, 0       ;initialize counter by 0
    Lea di, HexString
Continue:
    mov al, [di]
    cmp al, 24h     ;Test if the end of the number is reached
    je Convert1
    inc cx          ;increment counter
    inc di          ;increment di to point on the next byte
    jmp Continue

Convert1:
    dec di          ;dec di to point on the least significant byte
    mov bl, [di]
    sub bl, 30h     ;subtracr 30h to convert ascii to number
    ;Check whether the char is number or letter
    cmp bl, 10h      ;if it is a letter, bl > 10h
    ja subseven
    jmp Convert2
subseven:
    sub bl, 7          ;subtract 7 from bl to convert ascii of letter to numbers
    mov bh, 0
Convert2:
    mov HexNum, bx
    dec cx 
    

    cmp cx, 0
    jnz aaaaaaaa
    jmp ThisIsTheEnd
aaaaaaaa:
    dec di          ;move pointer to the next number
    mov bl, [di]
    sub bl, 30h     ;subtracr 30h to convert ascii to number
    ;Check whether the char is number or letter
    cmp bl, 10h      ;if it is a letter, bl > 10h
    ja subseven1
    jmp Convert3
subseven1:
    sub bl, 7          ;subtract 7 from bl to convert ascii of letter to numbers

Convert3:
    mov al, bl
    mov bl, 16
    mul bl
    add HexNum, ax
    dec cx 
    

    cmp cx, 0
    jnz bbbbbbbbb
    jmp ThisIsTheEnd
bbbbbbbbb:
    dec di          ;move pointer to the next number
    mov bl, [di]
    sub bl, 30h     ;subtracr 30h to convert ascii to number
    ;Check whether the char is number or letter
    cmp bl, 10h      ;if it is a letter, bl > 10h
    ja subseven2
    jmp Convert4
subseven2:
 sub bl, 7           ;subtract 7 from bl to convert ascii of letter to numbers

Convert4:
    mov al, bl
    mov bx, 256
    mul bx
    add HexNum, ax
    dec cx 
    

    cmp cx, 0
    jnz cccccccccc
    jmp ThisIsTheEnd
cccccccccc:
    dec di          ;move pointer to the next number
    mov bl, [di]
    sub bl, 30h     ;subtracr 30h to convert ascii to number
    ;Check whether the char is number or letter
    cmp bl, 10h      ;if it is a letter, bl > 10h
    ja subseven3
    jmp Convert5
subseven3:
     sub bl, 7           ;subtract 7 from bl to convert ascii of letter to numbers

Convert5:
    mov al, bl
    mov bx, 4096
    mul bx
    add HexNum, ax

ThisIsTheEnd:
    popf
    popa
ENDM Str_Hex