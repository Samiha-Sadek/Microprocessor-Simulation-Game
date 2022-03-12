.286
ScoreToString MACRO ScoreStr, ScoreDec
    Local Repeat1
    pusha
    lea di ,ScoreStr
    add di, 2           ;start from the least significant byte
    mov ax, ScoreDec
    mov cx, 3
Repeat1:
    mov dx, 0
    mov bx, 10
    Div bx              ;remainder in dx
    add dx, 30h
    mov [di], dl
    dec di
    loop Repeat1
    popa
ENDM ScoreToString