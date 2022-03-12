.286
ScoreToDecimal MACRO ScoreStr, ScoreDec
    local Proceed
    local Convertdec1
    local Convertdec2
    local Convertdec3
    local Invalid
    local EndOfscore
    ;ScoreStr = Score as string
    ;ScoreDec = Score in decimal
    pusha
    mov cx, 0           ;initialize counter with 0
    lea di, ScoreStr
Proceed:
    mov al, byte ptr [di]
    mov ah, 0
    cmp al, 24h
    je Convertdec1
    cmp al, 30h
    jb Invalid
    cmp al, 39h
    ja Invalid
    ;Valid:
    inc di      ;move pointer to the next byte
    inc cx       ;increment counter 
    jmp Proceed

Convertdec1:
    mov byte ptr [di], 24h
    dec di          ;dec di to point on the least significant byte
    mov bl, byte ptr [di]
    sub bl, 30h     ;subtract 30h to convert ascii to number
    mov bh, 0
    mov ScoreDec, bx
    sub cx, 1
    jnz Convertdec2
    jmp EndOfscore

Convertdec2:
    dec di
    mov al, byte ptr [di]
    sub al, 30h     ;subtracr 30h to convert ascii to number
    mov ah,0
    mov bl, 10
    mul bl
    add ScoreDec, ax
    sub cx, 1
    jnz Convertdec3
    jmp EndOfscore

Convertdec3:
    dec di
    mov al, byte ptr [di]
    sub al, 30h     ;subtracr 30h to convert ascii to number
    mov ah,0
    mov bl, 100
    mul bl
    add ScoreDec, ax
    jmp EndOfscore

Invalid:    
    lea si, ScoreDec
    mov word ptr [si], 100 

EndOfscore:
    popa   
ENDM ScoreToDecimal