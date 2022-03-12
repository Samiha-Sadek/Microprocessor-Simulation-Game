.286
Erase MACRO spaces
    pusha
    mov ax, 0
    mov al,0
    mov  ah, 02h    ;SetCursorPosition
    mov dl, 22      ;X coordinated
    mov dh, 19      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ax, 0
    mov dx, offset spaces 
    mov ah,9h
    int 21h

    mov ax, 0
    mov al,0
    mov  ah, 02h    ;SetCursorPosition
    mov dl, 1       ;X coordinated
    mov dh, 19      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ax, 0
    mov dx, offset spaces 
    mov ah,9h
    int 21h 
    popa  
ENDM Erase 