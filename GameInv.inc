.286
GameInvitation MACRO promt
    pusha
    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 47      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ah, 9
    mov dx, offset Promt
    int 21h
    popa
ENDM 