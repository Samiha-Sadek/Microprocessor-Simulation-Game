.286
WriteScore MACRO Score, player
    Local playerOneScore
    Local DisplayNum
    Local NextNum
    Local ScoreEnd
    pusha
    cmp player, 1
    je playerOneScore
    ;Player 2:
    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 24      ;X coordinated
    mov dh, 2      ;Ycoordinated
    mov bx, 0
    int 10h
    jmp DisplayNum   ;skip player1

playerOneScore:
    mov ax, 0
    mov  ah, 02h    ;SetCursorPosition
    mov dl, 3       ;X coordinated
    mov dh, 2      ;Ycoordinated
    mov bx, 0
    int 10h
DisplayNum:
    lea di, Score
NextNum: 
    mov al, [di]
    cmp al, 24h
    je ScoreEnd
    mov bl, 0Eh  ;Color is red
    mov bh, 0    ;Display page
    mov ah, 0Eh  ;Teletype
    int 10h
    mov ax, 0
    mov ah, 02h    ;SetCursorPosition
    inc dl       ;X coordinated
    mov dh, 2      ;Ycoordinated
    mov bx, 0
    int 10h
    inc di
    jmp NextNum

ScoreEnd:
    popa
ENDM WriteScore