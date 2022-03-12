.286
ShowTring MACRO X, Y, colour
Local Columny
Local Rowx


;X,Y are the center
pusha
mov bx,0

mov bl,5  ;W/2


mov cx,X ;Column
mov dx,Y ;Row   
mov al,colour ;Pixel color                        
mov ah,0ch ;Draw Pixel Command

sub dx, bx    ;now dx is at the tip 


Columny:
mov bx,0
mov bl,5  ;W/2

add bx, dx
sub bx, Y    ; dlta y =bx = currenty (or dx) + w/2 -Y 

mov cx, X
sub cx,bx    ; cx = X - dlta y

add bx, X 
add bx,1    ;for the check

Rowx: 

int 10h
inc cx
cmp cx, bx    ;bx is x +dlta Y
jnz Rowx
 
 
inc dx
; cmp dx,di
mov bx, Y
add bx, 10
mov bl,5  ;W/2
add bx,Y ;bx is y+w/2

cmp dx,bx   

jnz Columny

popa
ENDM 