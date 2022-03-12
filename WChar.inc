.286
WriteChar MACRO char, col, row
pusha
mov  dl, col   ;Column
mov  dh, row  ;Row
mov  bh, 0    ;Display page
mov  ah, 02h  ;SetCursorPosition
int  10h

mov  al, char
mov  bl, 0ACh  
mov  bh, 0    ;Display page
mov  ah, 0Eh  ;Teletype
int  10h

popa 
ENDM WriteChar