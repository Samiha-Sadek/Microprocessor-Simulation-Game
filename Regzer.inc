.286
Regzero MACRO Regg
    Local Repat1
    pusha
    lea di ,Regg
    mov cx, 4
Repat1:
    mov [di],byte ptr '0'
    inc di
    loop Repat1
    popa
ENDM Regzero