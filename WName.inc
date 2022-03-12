.286
WriteName MACRO Name, player
    Local playerOne
    Local Display
    Local NextChar
    Local NameEnd
pusha
    cmp player, 1
    je playerOne
    ;Player 2:
    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 22      ;X coordinated
    mov dh, 0      ;Ycoordinated
    mov bx, 0
    int 10h
    jmp Display   ;skip player1

    playerOne:
    mov ax, 0
    mov  ah, 02h    ;SetCursorPosition
    mov dl, 1       ;X coordinated
    mov dh, 0      ;Ycoordinated
    mov bx, 0
    int 10h
Display:
    lea di, Name
NextChar: 
    mov  al, [di]
    cmp al, 24h
    je NameEnd
    mov  bl, 0Ch  ;Color is red
    mov  bh, 0    ;Display page
    mov  ah, 0Eh  ;Teletype
    int  10h
    mov ax, 0
    mov  ah, 02h    ;SetCursorPosition
    inc dl       ;X coordinated
    mov dh, 0      ;Ycoordinated
    mov bx, 0
    int 10h
    inc di
    jmp NextChar

NameEnd:
popa
ENDM WriteName