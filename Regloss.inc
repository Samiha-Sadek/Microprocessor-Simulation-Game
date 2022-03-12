.286
regloss MACRO reg,target,lose
Local kfara 

pusha



lea di, byte ptr reg
lea si, byte ptr target


mov cx, 4           ;length of command is 4
REPE CMPSB          ;compare with the first command in the list

cmp cx,0 
jnz kfara

;lost
mov lose,1


kfara:


popa

ENDM 