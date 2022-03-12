.286
WriteByte MACRO destination, source, varstr
;;Source = Hexa number 
;;Destination = where the string (corresponding to the hexa number) is saved
    pusha
    pushf
    Hex_Str source, varstr      ;Converting the Hexa number to string and save it in a varibale string
    lea di, destination
    mov ax, WORD PTR varstr+2   ;taking the 2 least sig bytes of the string
    mov [di], ax                ;save it in the destination   
    popf
    popa
ENDM