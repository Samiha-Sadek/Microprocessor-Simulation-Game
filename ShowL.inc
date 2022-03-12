.286
ShowLevel MACRO level, promt
    Local Write1, xx
    pusha
    mov ax, 013h
    int 10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 5      ;X coordinated
    mov dh, 9      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ah, 9
    mov dx, offset promt
    int 21h

    mov  dl, 11   ;Column
    mov  dh, 9   ;Row
    mov  bh, 0    ;Display page
    mov  ah, 02h  ;SetCursorPosition
    int  10h
    
    cmp Level, 1
    je Write1
    mov  al, '2'
    jmp xx
Write1:    
    mov  al, '1'
xx:
    mov  bl, 0ACh  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    ;wait for 3 seconds
    MOV     CX, 0FH
    MOV     DX, 4240H
    MOV     AH, 86H
    INT     15H

    MOV     CX, 0FH
    MOV     DX, 4240H
    MOV     AH, 86H
    INT     15H

    MOV     CX, 0FH
    MOV     DX, 4240H
    MOV     AH, 86H
    INT     15H
    popa
ENDM