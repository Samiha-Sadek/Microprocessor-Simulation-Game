.286
WriteRegCont MACRO regcontent, regpos
    Local WriteRegValues
    pusha
    mov cl, 4
    mov ch, 7
    lea di, regcontent
WriteRegValues:    
    mov ax, 0
    mov ah, 02     ;SetCursorPosition
    mov dl, regpos      ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di]
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, regpos+1     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di+1]
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, regpos+2     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di+2]
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, regpos+3     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di+3]
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    add ch, 2
    add di ,4
    dec cl
    jnz WriteRegValues
    popa
ENDM