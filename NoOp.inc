.286
NoOp MACRO command, operand1, operand2, valid2Chk
 

	pusha
	pushf
	mov DI, offset command   ; The variable containing the command
	
	; fy 7alet el no operands, h-skip el kalam dh kolo, w nroo7 lel execution of command

	; skipping the command part
	; skipping some spaces
SkipSpacep0:          	    ; Skips spaces before the command
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipLettersp0     ; jumps if DI doesn't have a space
	                    ; if it doesn't have a space, then we have skipped all spaces,
        	            ; and reached the beginning of the command
	inc DI
	jmp SkipSpacep0       ; in case it didn't jump to SkipLetters, then it's a space, skip it





SkipLettersp0:        	    ; skips the command itself
        	            ; Code will reach this part when DI points to the start of the command
                	    ; We should skip all chars till we reach a space

	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jz SkipSpace2p0       ; jumps if it has a space
        	            ; if it has a space, then we have skipped all letters,
                	    ; and reached the beginning of the operands
	inc DI              
	jmp SkipLettersp0     ; in case it didn't jump to SkipSpace2, then it's a letter, skip it




SkipSpace2p0:         	    ; Skips spaces after the command, and before the operand
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipIncp0	    ; jumps if DI doesn't have a space
	                    ; if it doesn't have a space, then we have skipped all spaces,
        	            ; and reached the beginning of the command

	
	inc DI


	jmp SkipSpace2p0       ; if it's a space, skip it
SkipIncp0:


		   	     ; Having reached this part, DI now points at the beginning of the first operand
			     ; which should be $, or invalid

NoOperandp0:    		     ; handles operands of CLC - NOP

	mov bl, 36       ; ASCII of '$'
	cmp bl,[DI]

	jz nhayap0        ; if DI points to $, then it's valid
	; deft dh (3)
	mov bl, 13       ; ASCII of 'Enter'
	cmp bl,[DI]
	jz nhayap0        ; if DI points to $, then it's valid
	
	jmp InvalidOPp0   ; if it points to something else, then it's invalid
	




InvalidOPp0:  	    ; hy5osh hena lw msh valid operand
	            ; el mfrood hena n-skip el execution, w y5sr his turn, w one point

	mov valid2Chk,0     ; command is invalid, no need to continue the checks



nhayap0: 


	popf
	popa

ENDM
	
