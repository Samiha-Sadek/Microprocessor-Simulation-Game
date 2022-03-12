.286
ShowRegister MACRO X, Y
Local Columny
Local Rowx

pusha
mov cx,X ;Column
mov dx,Y ;Row   
mov al,6 ;Pixel color
mov ah,0ch ;Draw Pixel Command

mov si, 36    ;width
add si,X
mov di, 12    ;length
add di, Y 

Columny:

mov cx,X ;Column 
Rowx: int 10h
 inc cx
 cmp cx, si
 jnz Rowx
 
inc dx
cmp dx,di
jnz Columny

popa
ENDM ShowRegister