.286
OneOp MACRO command, operand1, operand2, valid2Chk



    pusha
	pushf
	mov DI, offset command   ; The variable containing the command
	
	; fy 7alet el no operands, h-skip el kalam dh kolo, w nroo7 lel execution of command

	; skipping the command part
	; skipping some spaces
SkipSpace0P:          	    ; Skips spaces before the command
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipLetters0P     ; jumps if DI doesn't have a space
        	            ; if it doesn't have a space, then we have skipped all spaces,
                	    ; and reached the beginning of the command
	inc DI
	jmp SkipSpace0P       ; in case it didn't jump to SkipLetters, then it's a space, skip it





SkipLetters0P:     	    ; skips the command itself
	                    ; Code will reach this part when DI points to the start of the command
        	            ; We should skip all chars till we reach a space

	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jz SkipSpace20P       ; jumps if it has a space
        	            ; if it has a space, then we have skipped all letters,
                	    ; and reached the beginning of the operands
	inc DI              
	jmp SkipLetters0P     ; in case it didn't jump to SkipSpace2, then it's a letter, skip it





SkipSpace20P:         	    ; Skips spaces after the command, and before the operand
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc0P         ; jumps if DI doesn't have a space
        	            ; if it doesn't have a space, then we have skipped all spaces,
	                    ; and reached the beginning of the command


	inc DI


	jmp SkipSpace20P       ; if it's a space, skip it
SkipInc0P:


			     ; Having reached this part, DI now points at the beginning of the first operand


OneOperand0P:       	     ; handles operands of INC - DEC - MUL - DIV 

	mov bl, 65           ; ASCII of 'A'
	cmp bl,[DI]

	jnz skipJump260P
	jmp XorHorL0P          ; 1st operand is AX or AH or AL
skipJump260P:


	mov bl, 66           ; ASCII of 'B'
	cmp bl,[DI]

	jnz skipJump270P
	jmp XorHorL20P         ; 1st operand is BX or BH or BL or BP
skipJump270P:



	mov bl, 67           ; ASCII of 'C'
	cmp bl,[DI]
	jnz skipJump280P
	jmp XorHorL0P          ; 1st operand is CX or CH or CL
skipJump280P:



	mov bl, 68           ; ASCII of 'D'
	cmp bl,[DI]

	jnz skipJump290P
	jmp XorHorL30P         ; 1st operand is DX or DH or DL or DI
skipJump290P:


						 ; lw 3ada mn kol dh, yb2a msh byebda2 b A,B,C,D

	mov bl, 83           ; ASCII of 'S'
	cmp bl,[DI]

	jnz skipJump300P
	jmp SIorSP0P           ; 1st operand is SI or SP
skipJump300P:



	mov bl, 91           ; ASCII of '['
	cmp bl,[DI]

	jnz skipJump310P
	jmp SquareBracket0P    ; 1st operand is [SP],[SI],[DI],[BP]
skipJump310P:

						; lw wsl l7d hena, yb2a msh byebda2 b A,B,C,D,S,[
  						; yb2a invalid
	jmp InvalidOP0P


; ////////////////////////////////////////// The second char ///////////////////////////////////////


XorHorL0P:	            ; first letter is A,C
	inc DI              ; DI dlw2ty btshawer 3la X, H, L

	mov bl, 88          ; ASCII of 'X'
	cmp bl,[DI]

	jnz skipJump440P
	jmp WriteOperands0P   ; 1st op is AX or CX (16 bits)
skipJump440P:


	mov bl, 72          ; ASCII of 'H'
	cmp bl,[DI]

	jnz skipJumpa70P
	jmp WriteOperands0P   ; 1st op is AH or CH (8 bits)
skipJumpa70P:


	mov bl, 76          ; ASCII of 'L'
	cmp bl,[DI]

	jnz skipJumpa80P
	jmp WriteOperands0P   ; 1st op is AL or CL (8 bits)
skipJumpa80P:


			    ; lw wesel hena, yb2a bada2 b A aw C, bs msh AL,AH,AX,Cl,Ch,CX
			    ; yb2a invalid
	jmp InvalidOP0P





XorHorL20P:
	inc DI              ; DI dlw2ty btshawer 3la X, H, L, P

	mov bl, 88          ; ASCII of 'X'
	cmp bl,[DI]

	jnz skipJump450P
	jmp WriteOperands0P   ; 1st op is BX (16 bits)
skipJump450P:


	mov bl, 72          ; ASCII of 'H'
	cmp bl,[DI]

	jnz skipJumpa90P
	jmp WriteOperands0P   ; 1st op is BH (8 bits)
skipJumpa90P:


	mov bl, 76          ; ASCII of 'L'
	cmp bl,[DI]

	jnz skipJump540P
	jmp WriteOperands0P   ; 1st op is BL (8 bits)
skipJump540P:


	mov bl, 80          ; ASCII of 'P'
	cmp bl,[DI]         

	jnz skipJump460P
	jmp WriteOperands0P   ; 1st op is BP (16 bits)
skipJump460P:



			    ; lw wesel hena, yb2a bada2 b B, bs msh BL,BH,BX,BP
			    ; yb2a invalid
	jmp InvalidOP0P


XorHorL30P:
	inc DI              ; DI dlw2ty btshawer 3la X, H, L, I

	mov bl, 88          ; ASCII of 'X'
	cmp bl,[DI]


	jnz skipJump470P
	jmp WriteOperands0P   ; 1st op is DX (16 bits)
skipJump470P:


	mov bl, 72          ; ASCII of 'H'
	cmp bl,[DI]

	jnz skipJump530P
	jmp WriteOperands0P   ; 1st op is DH (8 bits)
skipJump530P:



	mov bl, 76          ; ASCII of 'L'
	cmp bl,[DI]

	jnz skipJump520P
	jmp WriteOperands0P   ; 1st op is DL (8 bits)
skipJump520P:


	mov bl, 73          ; ASCII of 'I'
	cmp bl,[DI]

	jnz skipJump480P
	jmp WriteOperands0P   ; 1st op is DI (16 bits)
skipJump480P:


						; lw wesel hena, yb2a bada2 b D, bs msh DL,DH,DX,DI
						; yb2a invalid
	jmp InvalidOP0P



SIorSP0P:
	inc DI              ; DI dlw2ty btshawer 3la I, P

	mov bl, 73          ; ASCII of 'I'
	cmp bl,[DI]

	jnz skipJump490P
	jmp WriteOperands0P   ; 1st op is SI (16 bits)
skipJump490P:


	mov bl, 80          ; ASCII of 'P'
	cmp bl,[DI]         

	jnz skipJump500P
	jmp WriteOperands0P   ; 1st op is SP (16 bits)
skipJump500P:


						; lw wesel hena, yb2a bada2 b S, bs msh SI,SP
						; yb2a invalid
	jmp InvalidOP0P




SquareBracket0P:
	inc DI              ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw shwyt spaces, aw memory

SkipSpace30P:				; Skips spaces after the [, and before the letter
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc60P		; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI
	jmp SkipSpace30P      ; if it's a space, skip it
SkipInc60P:

						; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw memory 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F

	mov bl, 83          ; ASCII of 'S'
	cmp bl,[DI]
	jz SIorSP20P          ; 1st operand is [SI] or [SP]

	mov bl, 66          ; ASCII of 'B'
	cmp bl,[DI]
	jz bracketBP0P        ; 1st operand is [BP] or [B]

	mov bl, 68          ; ASCII of 'D'
	cmp bl,[DI]

	jnz skipJump20P
	jmp bracketDI0P       ; 1st operand is [DI] or [D]
skipJump20P:

	mov bl, 48          ; ASCII of '0'
	cmp bl,[DI]
	jnz skipJump30P
	jmp checkbracketmemory0P          ; 1st operand is [0]
skipJump30P:


	mov bl, 49          ; ASCII of '1'
	cmp bl,[DI]
	jnz skipJump40P
	jmp checkbracketmemory0P          ; 1st operand is [1]
skipJump40P:


	mov bl, 50          ; ASCII of '2'
	cmp bl,[DI]
	jnz skipJump50P
	jmp checkbracketmemory0P          ; 1st operand is [2]
skipJump50P:


	mov bl, 51          ; ASCII of '3'
	cmp bl,[DI]
	jnz skipJump60P
	jmp checkbracketmemory0P          ; 1st operand is [3]
skipJump60P:

	mov bl, 52          ; ASCII of '4'
	cmp bl,[DI]

	jnz skipJump510P
	jmp checkbracketmemory0P         ; 1st operand is [4]
skipJump510P:


	mov bl, 53          ; ASCII of '5'
	cmp bl,[DI]
	jz checkbracketmemory0P         ; 1st operand is [5]

	mov bl, 54          ; ASCII of '6'
	cmp bl,[DI]
	jz checkbracketmemory0P       ; 1st operand is [6]

	mov bl, 55          ; ASCII of '7'
	cmp bl,[DI]
	jz checkbracketmemory0P          ; 1st operand is [7]

	mov bl, 56          ; ASCII of '8'
	cmp bl,[DI]
	jz checkbracketmemory0P          ; 1st operand is [8]

	mov bl, 57          ; ASCII of '9'
	cmp bl,[DI]
	jz checkbracketmemory0P          ; 1st operand is [9]

	mov bl, 65          ; ASCII of 'A'
	cmp bl,[DI]
	jz checkbracketmemory0P          ; 1st operand is [A]

	mov bl, 67          ; ASCII of 'C'
	cmp bl,[DI]
	jz checkbracketmemory0P          ; 1st operand is [C]

	mov bl, 69          ; ASCII of 'E'
	cmp bl,[DI]
	jz checkbracketmemory0P          ; 1st operand is [E]

	mov bl, 70          ; ASCII of 'F'
	cmp bl,[DI]
	jz checkbracketmemory0P          ; 1st operand is [F]

						; lw 3ada mn dol, yb2a invalid
	jmp InvalidOP0P

SIorSP20P:       
	inc DI				; DI dlw2ty btshawer 3la I, P

	mov bl, 73          ; ASCII of 'I'
	cmp bl,[DI]
	jz checkbracket0P     ; 1st op is [SI]

	mov bl, 80          ; ASCII of 'P'
	cmp bl,[DI]         
	jz checkbracket0P     ; 1st op is [SP]

						; lw msh I aw P, yb2a invalid
	jmp InvalidOP0P


bracketBP0P:      
	inc DI              ; DI dlw2ty btshawer 3la P, aw hya 5lst 5las

	mov bl, 80          ; ASCII of 'P'
	cmp bl,[DI]         
	jz checkbracket0P        ; 1st op is [BP]

	; lw msh BP, yb2a invalid, aw [B] lel memory
	; lw hya [B] lel memory, yb2a el mfrood dlw2ty yb2a bracket, aw shwyt spaces

	; h-check lw fy spaces, a3mlohom skip, b3dein el mfrood ala2y bracket, lw la yb2a invalid
SkipSpace50P:		        ; Skips spaces before the ]
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc70P		; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI
	jmp SkipSpace50P      ; if it's a space, skip it
SkipInc70P:
	; kda el DI btshawer 3la ] aw invalid

	mov bl, 93          ; ASCII of ']'
	cmp bl,[DI]         
	; modtar a3ml el jump bl shakl el mo3aq dh
	; 3lshan myenfa3sh a3ml jz far ValidOp16
	jnz skipJump0P                ; lw msh closed bracket, hy-jump lel invalid
	jmp WriteOperands0P        ; 1st op is [B]
skipJump0P:
	; lw wesel le hena, yb2a msh closed bracket, yb2a invalid

	jmp InvalidOP0P



bracketDI0P:
	inc DI              ; DI dlw2ty btshawer 3la I, aw hya 5lst 5las

	mov bl, 73          ; ASCII of 'I'
	cmp bl,[DI]
	jz checkbracket0P       ; 1st op is [DI]


	; lw msh DI, yb2a invalid, aw [D] lel memory
	; lw hya [D] lel memory, yb2a el mfrood dlw2ty yb2a bracket, aw shwyt spaces

	; h-check lw fy spaces, a3mlohom skip, b3dein el mfrood ala2y bracket, lw la yb2a invalid
SkipSpace60P:         ; Skips spaces before the ]
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc80P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI
	jmp SkipSpace60P      ; if it's a space, skip it
SkipInc80P:
	; kda el DI btshawer 3la ] aw invalid

	mov bl, 93          ; ASCII of ']'
	cmp bl,[DI]         
	; modtar a3ml el jump bl shakl el mo3aq dh
	; 3lshan myenfa3sh a3ml jz far ValidOp16
	jnz skipJump70P                ; lw closed bracket, hy-jump lel invalid
	jmp WriteOperands0P        ; 1st op is [D]
skipJump70P:
	; lw wesel le hena, yb2a msh closed bracket, yb2a invalid



	jmp InvalidOP0P




checkbracketmemory0P:

	inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace70P:         ; Skips spaces after the letter, and before the ]
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc90P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI
	jmp SkipSpace70P       ; if it's a space, skip it
SkipInc90P:


	mov bl, 93          ; ASCII of ']'
	cmp bl,[DI]         
	; modtar a3ml el jump bl shakl el mo3aq dh
	; 3lshan myenfa3sh a3ml jz far ValidOp16
	jnz skipJump90P                ; lw closed bracket, hy-jump
	jmp WriteOperands0P        ; 1st op is memory: [0],[1],...[D],[E],[F]
skipJump90P:
	; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


	; lw msh ], yb2a invalid
	jmp InvalidOP0P


checkbracket0P:       
	inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace40P:         ; Skips spaces after the [, and before the letter
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc100P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI              
	jmp SkipSpace40P       ; if it's a space, skip it
SkipInc100P:

	mov bl, 93          ; ASCII of ']'
	cmp bl,[DI]         
	; modtar a3ml el jump bl shakl el mo3aq dh
	; 3lshan myenfa3sh a3ml jz far ValidOp16
	jnz skipJump80P                ; lw closed bracket, hy-jump
	jmp WriteOperands0P        ; 1st op is [SI],[DI],[SP],[BP]
skipJump80P:
	; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


	; lw msh ], yb2a invalid
	jmp InvalidOP0P





	; ////////////////////////////////////////// The operand has been handleed by this point ///////////////////////////////////////



	; 3ayez fl 2a5er b3d ma at2kd en kolo valid, adeelo string lel operand (mn 8eir spaces gowah)


WriteOperands0P:      ; dh b3d ma et2kd en kolo valid, b-save el operands fy variable b2a 3lshan tron yst3mlhom


	; dlw2ty DI byshawer 3la 2a5er correct character fl operand, el mfrood 3lshan tb2a valid
	; lazem ala2y b3deeh $, aw spaces b3dein $, lw la yb2a invalid l2no mzwd 7roof 8lt b3d el command
	inc DI          ; kda DI btshawer 3la awl 7aga b3d el 1st operand,
					; el hya ya 2ema $, ya 2ema spaces b3deeha $
	; skipping some spaces
SkipSpaceaftervmcheck0P:          ; Skips spaces after the immediate value
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz Skipspaceincvmcheck0P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,
						; and reached the beginning of the command
	inc DI
	jmp SkipSpaceaftervmcheck0P       ; in case it didn't jump to SkipLetters, then it's a space, skip it
Skipspaceincvmcheck0P:


	; checking if the end of the 2nd operand has been reached
	mov bl,36           ; ASCII of '$' 
	cmp bl,[DI]

	jz skipJump40vmcheck0P
	; deft dh (3)
	mov bl,13           ; ASCII of 'Enter' 
	cmp bl,[DI]

	jz skipJump40vmcheck0P
	jmp InvalidOP0P       ; hyb2a invalid lw mfeesh $ wala enter fl 2a5er
skipJump40vmcheck0P:



	mov SI, offset operand1   ; el variable elly hktb fyh el operands

	mov DI, offset command   ; el variable elly shayel el command

	; skipping the command part
	; skipping some spaces
SkipSpaceop0P:          ; Skips spaces before the command
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipLettersop0P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,
						; and reached the beginning of the command
	inc DI
	jmp SkipSpaceop0P       ; in case it didn't jump to SkipLettersop, then it's a space, skip it



SkipLettersop0P:        ; skips the command itself
						; Code will reach this part when DI points to the start of the command
						; We should skip all chars till we reach a space

	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jz SkipSpace2op0P       ; jumps if it has a space
						; if it has a space, then we have skipped all letters,
						; and reached the beginning of the operands
	inc DI              
	jmp SkipLettersop0P     ; in case it didn't jump to SkipSpace2, then it's a letter, skip it



SkipSpace2op0P:         ; Skips spaces after the command, and before the operand
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc40P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI
	jmp SkipSpace2op0P       ; if it's a space, skip it
SkipInc40P:



	; Having reached this part, DI now points at the beginning of the first operand
	; lw howa immediate value, momkn yb2a bade2 b shwyt zeros, fa h3mlha skip


	;3ayez azwd hena eny asheel el zeros elly fl awl [DONE]

	; skipping some zeros
	;mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero
						 ; mloosh lazma hena, l2n kda w kda el bta3 valid f3ln

Skipzeroszeroro0P:          ; Skips zeros at the beginning of 1st operand
	mov bl, 48         ; ASCII of zero
	cmp bl,[DI]         ; checks if the DI currently has a zero
	jnz startOfImmediateValuezeroro0P     ; jumps if DI doesn't have a zero
						; if it doesn't have a zero, then we have skipped all zeros,
						; and reached the beginning of the immediate value
	;mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
	inc DI
	jmp Skipzeroszeroro0P       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it


startOfImmediateValuezeroro0P:
	; lama awsal hena, ana kda 3mlt skip le kol el zeros, w dlw2ty ya 2ema wa2ef 3la space aw comma
	; ya 2ema wa2ef 3la char tany 3ady

	; lw wa2ef 3la spaces, yb2a h3mlha skip, wl mfrood ala2y b3deeha comma
	; yb2a kda el value kanet 0, wana 3mltlha skip, fa hktb zerohaya fy operand1, w yb2a kda 5las
	; aroo7 3la operand2 b2a


SkipSpace2op2zeroro0P:         ; Skips spaces after the 1st operand
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc5zeroro0P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI
	jmp SkipSpace2op2zeroro0P       ; if it's a space, skip it
SkipInc5zeroro0P:

	; lama awsl hena, ana kda 3mlt skip lel space, w wa2ef 3la comma, ya 2ema howa
	; kan asln md5l 7aga 8eir el zero

	mov bl, 44          ; ASCII of ','
	cmp bl,[DI]
	jnz LoadSecondOpzeroro0P         ; lw la2eit comma, yb2a howa kan kateb zero wana 3mltlha skip, fa
							; ha7ot el zero fy operand1, w aroo7 le operand2 3la tool


	mov ch,'0'            ; ba5od el 0
	mov [SI],ch         ; a7oto fl SI
	;inc SI              ; mlhash lazma 7ata n3mlha inc
	jmp LoadSecondOp0P     ; baroo7 ashoof el 2nd operand b2a

LoadSecondOpzeroro0P:








	; Having reached this part, DI now points at the beginning of the first operand
	dec DI              ; b3mlo dec hena 3lshan azwdo awl ma a5osh el loop
WriteFirstOp0P:
	inc DI
	; ha5od ay char a7oto fy Operand1, 2la lw el character dh space, h3mlo skip
	; lw el character dh $ , yb2a el operand 5eles
SkipSpace2op20P:         ; Skips spaces after the 1st operand
	mov bl, 32          ; ASCII of space
	cmp bl,[DI]         ; checks if the DI currently has a space
	jnz SkipInc50P     ; jumps if DI doesn't have a space
						; if it doesn't have a space, then we have skipped all spaces,

	inc DI
	jmp SkipSpace2op20P       ; if it's a space, skip it
SkipInc50P:

	mov bl, 36          ; ASCII of '$'
	cmp bl,[DI]
	jz LoadSecondOp0P         ; lw wslt lel $, yb2a el operand 5eles
	; deft dh (3)
	mov bl, 13       ; ASCII of 'Enter'
	cmp bl,[DI]

	jz LoadSecondOp0P         ; lw wslt lel enter, yb2a el operand 5eles
	; lw wslt lel 7eta dyh, yb2a dh la space wala comma, yb2a dh char fl 1st operand
	; fa hktbo fl SI b2a
	mov ch,[DI]         ; ba5od el value el gowa el DI
	mov [SI],ch         ; a7otaha fl SI
	inc SI
	jmp WriteFirstOp0P    ; byroo7 yekteb el letter el b3do


LoadSecondOp0P:
	jmp nhaya0P


InvalidOP0P:  	    ; hy5osh hena lw msh valid operand
	            ; el mfrood hena n-skip el execution, w y5sr his turn, w one point

	mov valid2Chk,0     ; command is invalid, no need to continue the checks

nhaya0P: 
    
	popf
	popa






ENDM
	
