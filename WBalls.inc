.286
WriteBalCont MACRO Orngball,wGryball,DGryball,Bluball,Grnball, balpos




    pusha
    mov ch, 17
    lea di, Orngball
    
    mov ax, 0
    mov ah, 02     ;SetCursorPosition
    mov dl, balpos      ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di]
    add al,30h     ;to convert from number to hexa
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h


    lea di, wGryball

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, balpos+1     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di]
    add al,30h     ;to convert from number to hexa
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    lea di, DGryball
    
    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, balpos+2     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di]
    add al,30h     ;to convert from number to hexa
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    lea di, Bluball
    
    
    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, balpos+3     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di]
    add al,30h     ;to convert from number to hexa
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h

    lea di, Grnball
    
    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, balpos+4     ;X coordinated
    mov dh, ch      ;Ycoordinated
    mov bx, 0
    int 10h
    
    mov  al, [di]
    add al,30h     ;to convert from number to hexa
    mov  bl, 055h  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h


    popa
ENDM