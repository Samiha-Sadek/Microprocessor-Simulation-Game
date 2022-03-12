.286
BackGround MACRO
    Local draw
    Local back1
    pusha
    mov cx, 0       ;Column
    mov dx, 0     ;Row
    mov al, 04FH      ;Color
    mov ah, 0ch  
draw:
    mov cx, 0       ;Column
back1:   
    int 10h
    inc cx
    cmp cx, 320
    jnz back1
    inc dx
    cmp dx, 200
    jnz draw
    popa  
ENDM BackGround