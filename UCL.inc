.286
ConvertToUCL MACRO string
    Local TheStart
    Local TheEnd
    Local CompareAgain
    Local ChangetoUCL
    Local NextLetter
    pusha
    lea di, string
TheStart:    
    mov al, [di]
    cmp al,24h
    je TheEnd
    cmp al, 60h
    ja CompareAgain
    jmp NextLetter
CompareAgain:
    cmp al, 7Bh
    jb ChangetoUCL
    jmp NextLetter
ChangetoUCL:
    sub al, 20h
    mov [di], al
NextLetter:
    inc di
    jmp TheStart
TheEnd:
    popa
ENDM