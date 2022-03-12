.286
WriteMemory MACRO HexNum
    Local WriteMem
    Local NextNum
    Local MemEnd
    Local NextMem
    pusha
    mov dl, 15      ;X coordinated
    mov cx, 2
WriteMem:
    mov ax, 0
    mov ah, 02     ;SetCursorPosition
    mov dh, 0      ;Ycoordinated
    mov bx, 0
    int 10h
    lea di, HexNum
NextMem: 
    mov al, [di]
    cmp al, 24h
    je MemEnd
    mov bl, 0ACh  ;Color is red
    mov bh, 0    ;Display page
    mov ah, 0Eh  ;Teletype
    int 10h
    mov ax, 0
    mov ah, 02h    ;SetCursorPosition
    inc dh       ;X coordinated
    mov bx, 0
    int 10h
    inc di
    jmp NextMem    
MemEnd: 
    lea di, HexNum
    mov dl, 35      ;X coordinated
    loop WriteMem
    popa
ENDM WriteMemory