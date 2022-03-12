.286
ShowRect MACRO X, Y, L, W, colour
Local Columny
Local Rowx


;X, Y are the leftmost upper pixel

pusha
mov cx,X ;Column
mov dx,Y ;Row   
mov al,colour ;Pixel color                        
mov ah,0ch ;Draw Pixel Command

; mov si, 36    ;width
; add si,X
; mov di, 12    ;length
; add di, Y 


 
Columny:

mov cx,X ;Column 

mov bx,X
add bx,L

Rowx: 

int 10h
 inc cx
;  cmp cx, si

 cmp cx, bx
 jnz Rowx
 

 
inc dx
; cmp dx,di

mov bx, Y
add bx, W

cmp dx,bx
jnz Columny

popa
ENDM 