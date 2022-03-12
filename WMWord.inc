.286
WriteMemWord MACRO Memory, source, varstr
    Local EndNotReahced
    Local WrapAroundMem
    Local EndWrite
;;Source = Hexa number to be saved in the memory as string
;;Memory = where the string is saved
    pusha
    pushf
    mov al, Memory+2
    cmp al, 0        ;Check if it's memory F
    jnz EndNotReahced
    jmp WrapAroundMem   ;The end of the memory  
EndNotReahced:    
    Hex_Str source, varstr  ;Converting the Hexa number to string and save it in a varibale string
    lea di, Memory
    mov ax, WORD PTR varstr+2   ;saving the 2 least sig bytes of the string
    mov [di], ax                ;save it in the destination
    mov ax, WORD PTR varstr     ;saving the 2 most sig bytes of the string
    mov [di+2], ax              ;save it in the destination
    jmp EndWrite
WrapAroundMem:
    Hex_Str source, varstr  ;Converting the Hexa number to string and save it in a varibale string
    lea di, Memory             
    mov ax, WORD PTR varstr+2 ;saving the 2 least sig bytes of the string (which is at memory F)
    mov [di], ax            ;save it in the destination
    mov ax, WORD PTR varstr ;saving the 2 most sig bytes of the string (which is at memory 0)
    mov [di-30], ax         ;save it in the destination 
    jmp EndWrite
EndWrite:
    popf
    popa
ENDM