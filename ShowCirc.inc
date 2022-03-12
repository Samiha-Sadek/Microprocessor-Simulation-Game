.286
ShowCirc MACRO X, Y,R, colour
Local Columny
Local Rowx
Local checkinside
Local draw 
Local Ddraw
Local lop

;X,Y,Rd

;X, Y are the center pixel

pusha

mov cx,X ;Column
mov dx,Y ;Row   

sub cx,R
sub dx, R

mov ax,colour ;Pixel color   ;changed to ax to avoid some error                        
mov ah,0ch ;Draw Pixel Command




 
Columny:

mov cx,X
sub cx,R 

mov bx,X
add bx,R   

Rowx: 

;here check first before drawing 
pusha 
checkinside:   ;checks if it satisfies r^2= x^2 + y^2
mov ax,X
sub ax,cx

call axchk



mov si,ax     ;si = |X|

push dx   ;to preserve its current value
mul si        
pop dx

mov si,ax     ;si = X^2

mov ax,Y
sub ax,dx

call axchk


mov di,ax   ;di = |Y|

push dx   ;to preserve its current value
mul di 
pop dx

mov di,ax   ;di = Y^2

add di,si    ;di = X^2 + Y^2 

mov ax,R


push dx 
mul ax
pop dx

mov si,ax  ;si = R^2
cmp di,si
je draw
jl draw    
jnle Ddraw 

draw:
popa
int 10h

jmp lop
;don't draw
Ddraw:
popa

lop:
inc cx


cmp cx, bx   ;Bx = X+R
jnz Rowx
 

 
inc dx
; cmp dx,di

mov bx, Y
add bx, R

cmp dx,bx     ;Bx = Y+R
jnz Columny

popa
ENDM 