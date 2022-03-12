.286
ReadMemWord MACRO Destination, Memory, varstr
    Local NotEnd
    Local WrapAround
    Local EndRead
;;Destination = Hexa number corresponding to the string copied from the memory
;;Memory = memory to read from
    pusha
    pushf
    mov al, Memory+2    
    cmp al, 0           ;Check if it's memory F
    jnz NotEnd
    jmp WrapAround      ;The end of the memory
NotEnd:
    lea si, Memory
    lea di, varstr
    add si, 2           ;To get the lower bytes first
    MOVSW               ;copy the string into a variable string
    sub si, 4           ;To get the higher bytes
    MOVSW               ;copy the string into a variable string
    Str_Hex varstr, Destination  ;convert the string to hexa number and save it in the destination
    jmp EndRead
WrapAround:
    lea si, Memory
    lea di, varstr
    sub si, 30              ;To get the lower bytes first (which is at memory 0)
    MOVSW                   ;copy the string into a variable string
    add si, 28              ;To get the higher bytes (which is at memory F)
    MOVSW                   ;copy the string into a variable string
    Str_Hex varstr, Destination     ;convert the string to hexa number and save it in the destination
EndRead:
    popf
    popa  
ENDM