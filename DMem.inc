.286
DrawMemory MACRO Memory, column
    Local WriteMemValues
    pusha
    mov cl, 16
    mov ch, 0
    lea di, Memory
WriteMemValues:    
    mov ax, 0
    mov ah, 02     ;SetCursorPosition
    mov dl, column      ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di]
    mov  bl, 050h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, column+1     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di+1]
    mov  bl, 050h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    inc ch
    add di ,2
    dec cl
    jnz WriteMemValues
    popa
ENDM