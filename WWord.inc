.286
WriteWord MACRO destination, source, varstr
;;Source = Hexa number 
;;Destination = where the string (corresponding to the hexa number) is saved
    pusha
    pushf
    Hex_Str source, varstr  ;Converting the Hexa number to string and save it in a varibale string
    lea di, destination
    mov ax, WORD PTR varstr     ;saving the 2 most sig bytes of the string    
    mov [di], ax                ;save it in the destination
    mov ax, WORD PTR varstr+2   ;saving the 2 least sig bytes of the string
    mov [di+2], ax              ;save it in the destination
    popf
    popa
ENDM