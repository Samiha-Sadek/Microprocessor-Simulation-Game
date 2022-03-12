.286
TwoOpF MACRO command, operand1, operand2, valid2Chk

pusha
	pushf
	
	mov DI, offset command   ; The variable containing the command
	
	; fy 7alet el no operands, h-skip el kalam dh kolo, w nroo7 lel execution of command



	
    ; skipping the command part
    ; skipping some spaces
SkipSpace:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipLetters     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpace       ; in case it didn't jump to SkipLetters, then it's a space, skip it





SkipLetters:        ; skips the command itself
                        ; Code will reach this part when DI points to the start of the command
                        ; We should skip all chars till we reach a space

    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jz SkipSpace2       ; jumps if it has a space
                        ; if it has a space, then we have skipped all letters,
                        ; and reached the beginning of the operands
    inc DI              
    jmp SkipLetters     ; in case it didn't jump to SkipSpace2, then it's a letter, skip it





SkipSpace2:         ; Skips spaces after the command, and before the operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command

    ; pushf               ; 3lshan a7tafez bl value of zero flag abl el inc DI
    inc DI
    ; popf                ; 3lshan arg3o tany

    jmp SkipSpace2       ; if it's a space, skip it
SkipInc:


    ; Having reached this part, DI now points at the beginning of the first operand

    ; In case of commands having 2 operands: 
    ; ADD - ADC- SUB - SBB - MOV - XOR - AND - OR   ; dol lyhom nafs el sheroot (fl valid operands y3ny)
    ; SHR - SHL - ROR                               ; dol lyhom nafs el sheroot (fl valid operands y3ny)

TwoOperandsFirstType:       ; handles operands of ADD - ADC- SUB - SBB - MOV - XOR - AND - OR

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]

    jnz skipJump26
    jmp XorHorL          ; 1st operand is AX or AH or AL
skipJump26:


    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]

    jnz skipJump27
    jmp XorHorL2          ; 1st operand is BX or BH or BL or BP
skipJump27:



    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jnz skipJump28
    jmp XorHorL          ; 1st operand is CX or CH or CL
skipJump28:



    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump29
    jmp XorHorL3         ; 1st operand is DX or DH or DL or DI
skipJump29:


    ; lw 3ada mn kol dh, yb2a msh byebda2 b A,B,C,D

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]

    jnz skipJump30
    jmp SIorSP          ; 1st operand is SI or SP
skipJump30:



    mov bl, 91          ; ASCII of '['
    cmp bl,[DI]

    jnz skipJump31
    jmp SquareBracket          ; 1st operand is [SP],[SI],[DI],[BP]
skipJump31:

    ; lw wsl l7d hena, yb2a msh byebda2 b A,B,C,D,S,[
    ; yb2a invalid
    jmp InvalidOP





    ; ////////////////////////////////////////// habos 3la tany letter b2a ///////////////////////////////////////


XorHorL:            ; first letter is A,C
    inc DI              ; DI dlw2ty btshawer 3la X, H, L

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump44
    jmp ValidOp16       ; 1st op is AX or CX (16 bits)
skipJump44:


    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]

    jnz skipJumpa7
    jmp ValidOp8        ; 1st op is AH or CH (8 bits)
skipJumpa7:


    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJumpa8
    jmp ValidOp8        ; 1st op is AL or CL (8 bits)
skipJumpa8:


    ; lw wesel hena, yb2a bada2 b A aw C, bs msh AL,AH,AX,Cl,Ch,CX
    ; yb2a invalid
    jmp InvalidOP





XorHorL2:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, P

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump45
    jmp ValidOp16       ; 1st op is BX (16 bits)
skipJump45:


    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]

    jnz skipJumpa9
    jmp ValidOp8        ; 1st op is BH (8 bits)
skipJumpa9:


    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJump54
    jmp ValidOp8        ; 1st op is BL (8 bits)
skipJump54:


    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         

    jnz skipJump46
    jmp ValidOp16        ; 1st op is BP (16 bits)
skipJump46:



    ; lw wesel hena, yb2a bada2 b B, bs msh BL,BH,BX,BP
    ; yb2a invalid
    jmp InvalidOP


XorHorL3:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, I

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]


    jnz skipJump47
    jmp ValidOp16       ; 1st op is DX (16 bits)
skipJump47:


    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]

    jnz skipJump53
    jmp ValidOp8        ; 1st op is DH (8 bits)
skipJump53:



    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJump52
    jmp ValidOp8        ; 1st op is DL (8 bits)
skipJump52:


    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]

    jnz skipJump48
    jmp ValidOp16        ; 1st op is DI (16 bits)
skipJump48:


    ; lw wesel hena, yb2a bada2 b D, bs msh DL,DH,DX,DI
    ; yb2a invalid
    jmp InvalidOP



SIorSP:
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]

    jnz skipJump49
    jmp ValidOp16       ; 1st op is SI (16 bits)
skipJump49:


    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         

    jnz skipJump50
    jmp ValidOp16        ; 1st op is SP (16 bits)
skipJump50:


    ; lw wesel hena, yb2a bada2 b S, bs msh SI,SP
    ; yb2a invalid
    jmp InvalidOP




SquareBracket:
    inc DI              ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw shwyt spaces, aw memory

SkipSpace3:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc6     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace3       ; if it's a space, skip it
SkipInc6:

    ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw memory 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]
    jz SIorSP2          ; 1st operand is [SI] or [SP]

    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]
    jz bracketBP          ; 1st operand is [BP] or [B]

    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump2
    jmp bracketDI          ; 1st operand is [DI] or [D]
skipJump2:

    mov bl, 48          ; ASCII of '0'
    cmp bl,[DI]
    jnz skipJump3
    jmp checkbracketmemory          ; 1st operand is [0]
skipJump3:


    mov bl, 49          ; ASCII of '1'
    cmp bl,[DI]
    jnz skipJump4
    jmp checkbracketmemory          ; 1st operand is [1]
skipJump4:


    mov bl, 50          ; ASCII of '2'
    cmp bl,[DI]
    jnz skipJump5
    jmp checkbracketmemory          ; 1st operand is [2]
skipJump5:


    mov bl, 51          ; ASCII of '3'
    cmp bl,[DI]
    jnz skipJump6
    jmp checkbracketmemory          ; 1st operand is [3]
skipJump6:

    mov bl, 52          ; ASCII of '4'
    cmp bl,[DI]

    jnz skipJump51
    jmp checkbracketmemory         ; 1st operand is [4]
skipJump51:


    mov bl, 53          ; ASCII of '5'
    cmp bl,[DI]
    jz checkbracketmemory         ; 1st operand is [5]

    mov bl, 54          ; ASCII of '6'
    cmp bl,[DI]
    jz checkbracketmemory       ; 1st operand is [6]

    mov bl, 55          ; ASCII of '7'
    cmp bl,[DI]
    jz checkbracketmemory          ; 1st operand is [7]

    mov bl, 56          ; ASCII of '8'
    cmp bl,[DI]
    jz checkbracketmemory          ; 1st operand is [8]

    mov bl, 57          ; ASCII of '9'
    cmp bl,[DI]
    jz checkbracketmemory          ; 1st operand is [9]

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]
    jz checkbracketmemory          ; 1st operand is [A]

    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jz checkbracketmemory          ; 1st operand is [C]

    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]
    jz checkbracketmemory          ; 1st operand is [E]

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]
    jz checkbracketmemory          ; 1st operand is [F]

    ; lw 3ada mn dol, yb2a invalid
    jmp InvalidOP

SIorSP2:       
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracket       ; 1st op is [SI]

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracket        ; 1st op is [SP]

    ; lw msh I aw P, yb2a invalid
    jmp InvalidOP


bracketBP:      
    inc DI              ; DI dlw2ty btshawer 3la P, aw hya 5lst 5las

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracket        ; 1st op is [BP]

    ; lw msh BP, yb2a invalid, aw [B] lel memory
    ; lw hya [B] lel memory, yb2a el mfrood dlw2ty yb2a bracket, aw shwyt spaces

    ; h-check lw fy spaces, a3mlohom skip, b3dein el mfrood ala2y bracket, lw la yb2a invalid
SkipSpace5:         ; Skips spaces before the ]
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc7     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace5      ; if it's a space, skip it
SkipInc7:
    ; kda el DI btshawer 3la ] aw invalid

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump                ; lw closed bracket, hy-jump lel invalid
    jmp Validmemory        ; 1st op is [B]
skipJump:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid

    jmp InvalidOP



bracketDI:
    inc DI              ; DI dlw2ty btshawer 3la I, aw hya 5lst 5las

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracket       ; 1st op is [DI]


    ; lw msh DI, yb2a invalid, aw [D] lel memory
    ; lw hya [D] lel memory, yb2a el mfrood dlw2ty yb2a bracket, aw shwyt spaces

    ; h-check lw fy spaces, a3mlohom skip, b3dein el mfrood ala2y bracket, lw la yb2a invalid
SkipSpace6:         ; Skips spaces before the ]
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc8     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace6      ; if it's a space, skip it
SkipInc8:
    ; kda el DI btshawer 3la ] aw invalid

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump7                ; lw closed bracket, hy-jump lel invalid
    jmp Validmemory        ; 1st op is [D]
skipJump7:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid



    jmp InvalidOP




checkbracketmemory:

    inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace7:         ; Skips spaces after the letter, and before the ]
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc9     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace7       ; if it's a space, skip it
SkipInc9:


    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump9                ; lw closed bracket, hy-jump
    jmp Validmemory        ; 1st op is memory: [0],[1],...[D],[E],[F]
skipJump9:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


    ; lw msh ], yb2a invalid
    jmp InvalidOP


checkbracket:       
    inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace4:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc10     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI              
    jmp SkipSpace4       ; if it's a space, skip it
SkipInc10:

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump8                ; lw closed bracket, hy-jump
    jmp ValidOp16        ; 1st op is [SI],[DI],[SP],[BP]
skipJump8:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


    ; lw msh ], yb2a invalid
    jmp InvalidOP



    ; ////////////////////////////////////////// kda 5lst el 2nd operand ///////////////////////////////////////





    ; 3ayez fl 2a5er b3d ma at2kd en kolo valid, adeelo 2 string, wa7ed lel 1st operand, w wa7ed lel 2nd operand (mn 8eir spaces gowahom)


ValidOp16:  ; hy5osh hena lw el 1st operand AX,CX,BX,DX, SI,DI,SP,BP, [SI],[DI],[SP],[BP]
    ;h-check 3l size-mismatch m3 el 2nd operand b2a
    mov ch,9            ; dh el flag bta3 el zero (hyban ta7t aktr)
    inc DI
    ; el mfrood DI dlw2ty btshawer 3la space, aw comma, lw la yb2a invalid

    ; skipping some spaces
SkipSpacesecond:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz skipComma     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecond       ; in case it didn't jump to SkipComma, then it's a space, skip it

skipComma:
    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]

    jz skipJump32
    jmp InvalidOP       ; if the spaces aren't followed with a comma, then it's invalid
skipJump32:

    inc DI              ; lw wesel hena, yb2a el operand el awlany tmam l7d el comma, hbtdy fl tany b2a
                        ; hshoof lw fy spaces el awl


    ; skipping some spaces
SkipSpacesecond2:          ; Skips spaces before the 2nd operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz startOfSecondOperand     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecond2       ; in case it didn't jump to SkipComma, then it's a space, skip it


startOfSecondOperand:



    ;DI now points to the beginning of the 2nd operand

    ; habos el awl, lw la2eit DI btshawer 3la $, yb2a howa mkatabsh 7aga fl 2nd operand
    ; yb2a invalid

    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump33
    jmp InvalidOP    ; baroo7 aktb b2a el 2 operands
skipJump33:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump33EN
    jmp InvalidOP    ; baroo7 aktb b2a el 2 operands
skipJump33EN:


    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]

    jnz skipJump34
    jmp XorHorLsecond          ; 2nd operand is AX or AH or AL
skipJump34:



    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]

    jnz skipJump35
    jmp XorHorL2second          ; 2nd operand is BX or BH or BL or BP
skipJump35:


    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]

    jnz skipJump36
    jmp XorHorLsecond          ; 2nd operand is CX or CH or CL
skipJump36:


    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump37
    jmp XorHorL3second         ; 2nd operand is DX or DH or DL or DI
skipJump37:


    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]

    jnz skipJump37E
    jmp InvalidOP         ; Invalid as it starts with E
skipJump37E:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]

    jnz skipJump37F
    jmp InvalidOP         ; Invalid as it starts with F
skipJump37F:



    ; lw 3ada mn kol dh, yb2a msh byebda2 b A,B,C,D

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]

    jnz skipJump38
    jmp SIorSPsecond          ; 1st operand is SI or SP
skipJump38:


    mov bl, 91          ; ASCII of '['
    cmp bl,[DI]


    jnz skipJump39
    jmp SquareBracketsecond          ; 1st operand is [SP],[SI],[DI],[BP]
skipJump39:

    ; lw wsl l7d hena, yb2a msh byebda2 b A,B,C,D,S,[
    ; yb2a invalid aw memory aw immediate
    ; lw memory, hyb2a invalid bsbb el size mismatch, bs dh fl goz2 bta3 el [
    ; lw immediate b2a: lazem ybda2 b 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F (arkam hexa)


    ; m7tag a-handle 7war eno lw bade b zeros kteer
    ; skipping some zeros
    mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero

Skipzeros:          ; Skips zeros at the beginning of 2nd operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValue     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp Skipzeros       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it



startOfImmediateValue:      ; lama awsl hena, ana kda tanesht kol el zeros elly fl awl, w ebtadeit fl value nafsaha

    ;bs el fekra en el value momkn tb2a zero asln, fana kda 3mltlha skip, lazem a-handle el 7eta dyh
    ; 3mltlo handle bl flag el zwdto (ch=9 aw ch=7)

    dec DI              ; b3mlo decrement hena, 3lshan azwdo awl ma a5osh el loop
    mov cl,5            ; 2a5ry 3lshan dh 16 bits, 2a5ry a7ot 4 arkam, lw 7at 5, yb2a size mismatch
Checkimmediate:
    inc DI              ; b-check kol mra, el immediate value = kam
    dec cl              ; lw dh wesel eno b2a =0, yb2a invalid, l2no kda da5al 5 arkam

    mov bl, 48          ; ASCII of '0'
    cmp bl,[DI]
    jnz skipJump10
    jmp Checkimmediate          ; 2nd operand is 0
skipJump10:


    mov bl, 49          ; ASCII of '1'
    cmp bl,[DI]
    jnz skipJump11
    jmp Checkimmediate          ; 1st operand is 1
skipJump11:


    mov bl, 50          ; ASCII of '2'
    cmp bl,[DI]
    jnz skipJump12
    jmp Checkimmediate          ; 1st operand is 2
skipJump12:


    mov bl, 51          ; ASCII of '3'
    cmp bl,[DI]
    jnz skipJump13
    jmp Checkimmediate          ; 1st operand is 3
skipJump13:

    mov bl, 52          ; ASCII of '4'
    cmp bl,[DI]
    jnz skipJump14
    jmp Checkimmediate          ; 1st operand is 4
skipJump14:

    mov bl, 53          ; ASCII of '5'
    cmp bl,[DI]
    jnz skipJump15
    jmp Checkimmediate          ; 1st operand is 5
skipJump15:

    mov bl, 54          ; ASCII of '6'
    cmp bl,[DI]
    jnz skipJump16
    jmp Checkimmediate          ; 1st operand is 6
skipJump16:

    mov bl, 55          ; ASCII of '7'
    cmp bl,[DI]
    jnz skipJump17
    jmp Checkimmediate          ; 1st operand is 7
skipJump17:

    mov bl, 56          ; ASCII of '8'
    cmp bl,[DI]
    jnz skipJump18
    jmp Checkimmediate          ; 1st operand is 8
skipJump18:

    mov bl, 57          ; ASCII of '9'
    cmp bl,[DI]
    jnz skipJump19
    jmp Checkimmediate          ; 1st operand is 9
skipJump19:

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]
    jnz skipJump20
    jmp Checkimmediate          ; 1st operand is A
skipJump20:

    mov bl, 67          ; ASCII of 'B'
    cmp bl,[DI]
    jnz skipJump21
    jmp Checkimmediate          ; 1st operand is B
skipJump21:

    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jnz skipJump22
    jmp Checkimmediate          ; 1st operand is C
skipJump22:

    mov bl, 67          ; ASCII of 'D'
    cmp bl,[DI]
    jnz skipJump23
    jmp Checkimmediate          ; 1st operand is D
skipJump23:

    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]
    jnz skipJump24
    jmp Checkimmediate          ; 1st operand is E
skipJump24:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]
    jnz skipJump25
    jmp Checkimmediate          ; 1st operand is F
skipJump25:


    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump40EN
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40EN:


    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch
    ; aw 3amel spaces b3d el arkam

    ;h-handle el spaces el awl
    ; skipping some spaces
SkipSpaceafter:          ; Skips spaces after the immediate value
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz Skipspaceinc     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceafter       ; in case it didn't jump to SkipLetters, then it's a space, skip it
Skipspaceinc:

    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40again
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40again:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump40againEN
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40againEN:

    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch

    mov bl,7
    cmp ch,bl           ; lw zero, yb2a valid w howa kan mda5al immediate value=0, wana 3mltlha skip

    jnz skipJump41
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump41:



    jmp InvalidOP






XorHorLsecond:            ; first letter is A,C
    inc DI              ; DI dlw2ty btshawer 3la X, H, L

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump42
    jmp WriteOperands       ; 2nd op is AX or CX (16 bits), fa haroo7 aktbo
skipJump42:


    ; lw wesel hena, yb2a bada2 b A aw C, bs msh AX,CX
    ; yb2a invalid
    jmp InvalidOP





XorHorL2second:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, P

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump43
    jmp WriteOperands      ; 2nd op is BX (16 bits)
skipJump43:



    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         

    jnz skipJumpa1
    jmp WriteOperands        ; 1st op is BP (16 bits)
skipJumpa1:


    ; lw wesel hena, yb2a bada2 b B, bs msh BX,BP
    ; yb2a invalid
    jmp InvalidOP


XorHorL3second:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, I

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJumpa2
    jmp WriteOperands      ; 1st op is DX (16 bits)
skipJumpa2:




    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]

    jnz skipJumpa3
    jmp WriteOperands      ; 1st op is DI (16 bits)
skipJumpa3:


    ; lw wesel hena, yb2a bada2 b D, bs msh DX,DI
    ; yb2a invalid
    jmp InvalidOP



SIorSPsecond:
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]

    jnz skipJumpa4
    jmp WriteOperands       ; 1st op is SI (16 bits)
skipJumpa4:


    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         

    jnz skipJumpa5
    jmp WriteOperands     ; 1st op is SP (16 bits)
skipJumpa5:

    ; lw wesel hena, yb2a bada2 b S, bs msh SI,SP
    ; yb2a invalid
    jmp InvalidOP




SquareBracketsecond:
    inc DI              ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw shwyt spaces, aw memory

SkipSpace3second:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace3second       ; if it's a space, skip it
SkipInc2:
    ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw memory 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]
    jz SIorSP2second          ; 1st operand is [SI] or [SP]

    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]
    jz bracketBPsecond          ; 1st operand is [BP] or [B]

    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump2second
    jmp bracketDIsecond         ; 1st operand is [DI] or [D]
skipJump2second:




    ; lw 3ada mn dol, yb2a invalid
    jmp InvalidOP

SIorSP2second:       
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketsecond       ; 1st op is [SI]

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketsecond        ; 1st op is [SP]

    ; lw msh I aw P, yb2a invalid
    jmp InvalidOP


bracketBPsecond:      
    inc DI              ; DI dlw2ty btshawer 3la P, aw hya 5lst 5las

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketsecond        ; 1st op is [BP]

    ; lw msh BP, yb2a invalid, aw [B] lel memory, bs el memory invalid brdo
    jmp InvalidOP


bracketDIsecond:
    inc DI              ; DI dlw2ty btshawer 3la I, aw hya 5lst 5las

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketsecond       ; 1st op is [DI]

    ; lw msh DI, yb2a invalid, aw [D] lel memory, bs el memory invalid brdo
    jmp InvalidOP



checkbracketsecond:       
    inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace4second:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc3     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI             
    jmp SkipSpace4second       ; if it's a space, skip it

SkipInc3:

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump8second                ; lw closed bracket, hy-jump
    jmp WriteOperands        ; 1st op is [SI],[DI],[SP],[BP]
skipJump8second:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


    ; lw msh ], yb2a invalid
    jmp InvalidOP








ValidOp8:   ; hy5osh hena lw el 1st operand AH,AL,BH,BL,CH,CL,DH,DL
    ;h-check 3l size-mismatch m3 el 2nd operand b2a
    ; 8b means 8-bit
    mov ch,9            ; dh el flag bta3 el zero (hyban ta7t aktr)
    inc DI
    ; el mfrood DI dlw2ty btshawer 3la space, aw comma, lw la yb2a invalid

    ; skipping some spaces
SkipSpacesecond8b:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz skipComma8b     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecond8b       ; in case it didn't jump to SkipComma, then it's a space, skip it

skipComma8b:
    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]

    jz skipJump328b
    jmp InvalidOP       ; if the spaces aren't followed with a comma, then it's invalid
skipJump328b:

    inc DI              ; lw wesel hena, yb2a el operand el awlany tmam l7d el comma, hbtdy fl tany b2a
                        ; hshoof lw fy spaces el awl


    ; skipping some spaces
SkipSpacesecond28b:          ; Skips spaces before the 2nd operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz startOfSecondOperand8b     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecond28b       ; in case it didn't jump to SkipComma, then it's a space, skip it


startOfSecondOperand8b:

    ;DI now points to the beginning of the 2nd operand

    ; habos el awl, lw la2eit DI btshawer 3la $, yb2a howa mkatabsh 7aga fl 2nd operand
    ; yb2a invalid

    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump338b
    jmp InvalidOP    ; baroo7 aktb b2a el 2 operands
skipJump338b:

    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump338bEN
    jmp InvalidOP    ; baroo7 aktb b2a el 2 operands
skipJump338bEN:


    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]

    jnz skipJump348b
    jmp XorHorLsecond8b          ; 2nd operand is AX or AH or AL
skipJump348b:



    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]

    jnz skipJump358b
    jmp XorHorLsecond8b          ; 2nd operand is BX or BH or BL
skipJump358b:


    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]

    jnz skipJump368b
    jmp XorHorLsecond8b          ; 2nd operand is CX or CH or CL
skipJump368b:


    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump378b
    jmp XorHorLsecond8b         ; 2nd operand is DX or DH or DL
skipJump378b:


    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]

    jnz skipJump37E2
    jmp InvalidOP         ; Invalid as it starts with E
skipJump37E2:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]

    jnz skipJump372F
    jmp InvalidOP         ; Invalid as it starts with F
skipJump372F:

    ; lw 3ada mn kol dh, yb2a msh byebda2 b A,B,C,D
    ; yb2a ya 2ema bracket, ya immediate, ya invalid

    mov bl, 91          ; ASCII of '['
    cmp bl,[DI]


    jnz skipJump398b
    jmp SquareBracketsecond8b          ; 1st operand is [SP],[SI],[DI],[BP]
skipJump398b:

    ; lw wsl l7d hena, yb2a msh byebda2 b A,B,C,D,[
    ; yb2a invalid aw S aw immediate
    ; lw S, hyb2a invalid bsbb el size mismatch
    ; lw immediate b2a: lazem ybda2 b 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F (arkam hexa)


    ; m7tag a-handle 7war eno lw bade b zeros kteer
    ; skipping some zeros
    mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero

Skipzeros8b:          ; Skips zeros at the beginning of 2nd operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValue8b     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp Skipzeros8b       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it



startOfImmediateValue8b:      ; lama awsl hena, ana kda tanesht kol el zeros elly fl awl, w ebtadeit fl value nafsaha

    ;bs el fekra en el value momkn tb2a zero asln, fana kda 3mltlha skip, lazem a-handle el 7eta dyh
    ; 3mltlo handle bl flag el zwdto (ch=9 aw ch=7)

    dec DI              ; b3mlo decrement hena, 3lshan azwdo awl ma a5osh el loop
    mov cl,3            ; 2a5ry 3lshan dh 8 bits, 2a5ry a7ot 2 arkam, lw 7at 3, yb2a size mismatch
Checkimmediate8b:
    inc DI              ; b-check kol mra, el immediate value = kam
    dec cl              ; lw dh wesel eno b2a =0, yb2a invalid, l2no kda da5al 2 arkam

    mov bl, 48          ; ASCII of '0'
    cmp bl,[DI]
    jnz skipJump108b
    jmp Checkimmediate8b          ; 2nd operand is 0
skipJump108b:


    mov bl, 49          ; ASCII of '1'
    cmp bl,[DI]
    jnz skipJump118b
    jmp Checkimmediate8b          ; 1st operand is 1
skipJump118b:


    mov bl, 50          ; ASCII of '2'
    cmp bl,[DI]
    jnz skipJump128b
    jmp Checkimmediate8b          ; 1st operand is 2
skipJump128b:


    mov bl, 51          ; ASCII of '3'
    cmp bl,[DI]
    jnz skipJump138b
    jmp Checkimmediate8b          ; 1st operand is 3
skipJump138b:

    mov bl, 52          ; ASCII of '4'
    cmp bl,[DI]
    jnz skipJump148b
    jmp Checkimmediate8b          ; 1st operand is 4
skipJump148b:

    mov bl, 53          ; ASCII of '5'
    cmp bl,[DI]
    jnz skipJump158b
    jmp Checkimmediate8b          ; 1st operand is 5
skipJump158b:

    mov bl, 54          ; ASCII of '6'
    cmp bl,[DI]
    jnz skipJump168b
    jmp Checkimmediate8b          ; 1st operand is 6
skipJump168b:

    mov bl, 55          ; ASCII of '7'
    cmp bl,[DI]
    jnz skipJump178b
    jmp Checkimmediate8b          ; 1st operand is 7
skipJump178b:

    mov bl, 56          ; ASCII of '8'
    cmp bl,[DI]
    jnz skipJump188b
    jmp Checkimmediate8b          ; 1st operand is 8
skipJump188b:

    mov bl, 57          ; ASCII of '9'
    cmp bl,[DI]
    jnz skipJump198b
    jmp Checkimmediate8b          ; 1st operand is 9
skipJump198b:

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]
    jnz skipJump208b
    jmp Checkimmediate8b          ; 1st operand is A
skipJump208b:

    mov bl, 67          ; ASCII of 'B'
    cmp bl,[DI]
    jnz skipJump218b
    jmp Checkimmediate8b          ; 1st operand is B
skipJump218b:

    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jnz skipJump228b
    jmp Checkimmediate8b          ; 1st operand is C
skipJump228b:

    mov bl, 67          ; ASCII of 'D'
    cmp bl,[DI]
    jnz skipJump238b
    jmp Checkimmediate8b          ; 1st operand is D
skipJump238b:

    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]
    jnz skipJump248b
    jmp Checkimmediate8b          ; 1st operand is E
skipJump248b:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]
    jnz skipJump258b
    jmp Checkimmediate8b          ; 1st operand is F
skipJump258b:


    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump408b
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump408b:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump408bEN
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump408bEN:

    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch
    ; aw 3amel spaces b3d el arkam

    ;h-handle el spaces el awl
    ; skipping some spaces
SkipSpaceafter8b:          ; Skips spaces after the immediate value
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz Skipspaceinc8b     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceafter8b       ; in case it didn't jump to SkipLetters, then it's a space, skip it
Skipspaceinc8b:

    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40again8b
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40again8b:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump40again8bEN
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40again8bEN:
    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch

    mov bl,7
    cmp ch,bl           ; lw zero, yb2a valid w howa kan mda5al immediate value=0, wana 3mltlha skip

    jnz skipJump418b
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump418b:



    jmp InvalidOP



XorHorLsecond8b:            ; first letter is A,C,B,C
                                ;hazawed hena el B,C brdo, badal XorHorLsecond2 w 3     ; done 5las
    inc DI              ; DI dlw2ty btshawer 3la X, H, L

    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]

    jnz skipJump428b2
    jmp WriteOperands   ; 2nd op is AH,BH,CH,DH (8 bits), fa haroo7 aktbo
skipJump428b2:

    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJump428b
    jmp WriteOperands          ; 2nd op is AL,BL,CL,DL (8 bits), fa haroo7 aktbo     
skipJump428b:


    ; lw wesel hena, yb2a bada2 b A,B,C,D, bs mgash b3deehom H,L
    ; yb2a invalid
    jmp InvalidOP


SquareBracketsecond8b:
    inc DI              ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw shwyt spaces, aw memory

SkipSpace3second8b:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc28b     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace3second8b       ; if it's a space, skip it
SkipInc28b:
    ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw memory 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]

    jnz skipJumpa6
    jmp SIorSP2second8b          ; 1st operand is [SI] or [SP]
skipJumpa6:

    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]

    jnz skipJumpa10
    jmp bracketBPsecond8b          ; 1st operand is [BP] or [B]
skipJumpa10:


    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump2second8b
    jmp bracketDIsecond8b         ; 1st operand is [DI] or [D]
skipJump2second8b:




    ; lw 3ada mn dol, yb2a invalid, aw memory

    mov bl, 48          ; ASCII of '0'
    cmp bl,[DI]
    jnz skipJump38b
    jmp checkbracketsecond8b          ; 1st operand is [0]
skipJump38b:


    mov bl, 49          ; ASCII of '1'
    cmp bl,[DI]
    jnz skipJump48b
    jmp checkbracketsecond8b          ; 1st operand is [1]
skipJump48b:


    mov bl, 50          ; ASCII of '2'
    cmp bl,[DI]
    jnz skipJump58b
    jmp checkbracketsecond8b          ; 1st operand is [2]
skipJump58b:


    mov bl, 51          ; ASCII of '3'
    cmp bl,[DI]
    jnz skipJump68b
    jmp checkbracketsecond8b          ; 1st operand is [3]
skipJump68b:

    mov bl, 52          ; ASCII of '4'
    cmp bl,[DI]

    jnz skipJump518b
    jmp checkbracketsecond8b         ; 1st operand is [4]
skipJump518b:


    mov bl, 53          ; ASCII of '5'
    cmp bl,[DI]
    jz checkbracketsecond8b         ; 1st operand is [5]

    mov bl, 54          ; ASCII of '6'
    cmp bl,[DI]
    jz checkbracketsecond8b       ; 1st operand is [6]

    mov bl, 55          ; ASCII of '7'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [7]

    mov bl, 56          ; ASCII of '8'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [8]

    mov bl, 57          ; ASCII of '9'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [9]

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [A]

    ; dyh obsolete hena
    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [B]

    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [C]

    ; dyh obsolete hena
    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [D]

    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [E]

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]
    jz checkbracketsecond8b          ; 1st operand is [F]

    ; lw 3ada mn dol, yb2a invalid
    jmp InvalidOP





    jmp InvalidOP

SIorSP2second8b:       
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketsecond8b       ; 1st op is [SI]

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketsecond8b        ; 1st op is [SP]

    ; lw msh I aw P, yb2a invalid
    jmp InvalidOP


bracketBPsecond8b:      
    inc DI              ; DI dlw2ty btshawer 3la P, aw hya 5lst 5las

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketsecond8b        ; 1st op is [BP]

    ; lw msh BP, yb2a invalid, aw [B] lel memory

    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jz checkbracketsecond8b        ; 1st op is [B     ]

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]        
    jz checkbracketsecond8b        ; 1st op is [B]
    ; lw wesel hena, yb2a howa bada2 b B, bs msh [B] wala [BP] bs, yb2a invalid

    jmp InvalidOP


bracketDIsecond8b:
    inc DI              ; DI dlw2ty btshawer 3la I, aw hya 5lst 5las

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketsecond8b       ; 1st op is [DI]

    ; lw msh DI, yb2a invalid, aw [D] lel memory


    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jz checkbracketsecond8b        ; 1st op is [D     ]

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]        
    jz checkbracketsecond8b        ; 1st op is [D]
    ; lw wesel hena, yb2a howa bada2 b B, bs msh [D] wala [DI] bs, yb2a invalid


    jmp InvalidOP


checkbracketsecond8b:       
    inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace4second8b:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc38b     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI             
    jmp SkipSpace4second8b       ; if it's a space, skip it

SkipInc38b:

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump8second8b                ; lw closed bracket, hy-jump
    jmp WriteOperands        ; 1st op is [SI],[DI],[SP],[BP]
skipJump8second8b:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


    ; lw msh ], yb2a invalid
    jmp InvalidOP




Validmemory: ; hy5osh hena lw el 1st operand memory: [0],[1],...,[D],[E],[F]
    ; vm y3ny valid memory



    mov ch,9            ; dh el flag bta3 el zero (hyban ta7t aktr)
    inc DI
    ; el mfrood DI dlw2ty btshawer 3la space, aw comma, lw la yb2a invalid

    ; skipping some spaces
SkipSpacesecondvm:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz skipCommavm     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecondvm       ; in case it didn't jump to SkipComma, then it's a space, skip it

skipCommavm:
    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]

    jz skipJump32vm
    jmp InvalidOP       ; if the spaces aren't followed with a comma, then it's invalid
skipJump32vm:

    inc DI              ; lw wesel hena, yb2a el operand el awlany tmam l7d el comma, hbtdy fl tany b2a
                        ; hshoof lw fy spaces el awl


    ; skipping some spaces
SkipSpacesecond2vm:          ; Skips spaces before the 2nd operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz startOfSecondOperandvm     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecond2vm       ; in case it didn't jump to SkipComma, then it's a space, skip it


startOfSecondOperandvm:



    ;DI now points to the beginning of the 2nd operand

    ; habos el awl, lw la2eit DI btshawer 3la $, yb2a howa mkatabsh 7aga fl 2nd operand
    ; yb2a invalid

    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump33vm
    jmp InvalidOP    ; baroo7 a2olo eno 5sr point
skipJump33vm:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump33vmEN
    jmp InvalidOP    ; baroo7 a2olo eno 5sr point
skipJump33vmEN:


    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]

    jnz skipJump34vm
    jmp XorHorLsecondvm          ; 2nd operand is AX or AH or AL
skipJump34vm:



    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]

    jnz skipJump35vm
    jmp XorHorL2secondvm          ; 2nd operand is BX or BH or BL or BP
skipJump35vm:


    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]

    jnz skipJump36vm
    jmp XorHorLsecondvm          ; 2nd operand is CX or CH or CL
skipJump36vm:


    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump37vm
    jmp XorHorL3secondvm         ; 2nd operand is DX or DH or DL or DI
skipJump37vm:

    ; lw 3ada mn kol dh, yb2a msh byebda2 b A,B,C,D


    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]

    jnz skipJump37E3
    jmp InvalidOP         ; Invalid as it starts with E
skipJump37E3:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]

    jnz skipJump37F3
    jmp InvalidOP         ; Invalid as it starts with F
skipJump37F3:


    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]

    jnz skipJump38vm
    jmp SIorSPsecondvm          ; 1st operand is SI or SP
skipJump38vm:


    mov bl, 91          ; ASCII of '['
    cmp bl,[DI]


    jnz skipJump39vm
    jmp SquareBracketsecondvm          ; 1st operand is [SP],[SI],[DI],[BP]
skipJump39vm:

    ; lw wsl l7d hena, yb2a msh byebda2 b A,B,C,D,S,[
    ; yb2a invalid aw memory aw immediate
    ; lw memory, hyb2a invalid bsbb el size mismatch, bs dh fl goz2 bta3 el [
    ; lw immediate b2a: lazem ybda2 b 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F (arkam hexa)


    ; m7tag a-handle 7war eno lw bade b zeros kteer
    ; skipping some zeros
    mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero

Skipzerosvm:          ; Skips zeros at the beginning of 2nd operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValuevm     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp Skipzerosvm       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it



startOfImmediateValuevm:      ; lama awsl hena, ana kda tanesht kol el zeros elly fl awl, w ebtadeit fl value nafsaha

    ;bs el fekra en el value momkn tb2a zero asln, fana kda 3mltlha skip, lazem a-handle el 7eta dyh
    ; 3mltlo handle bl flag el zwdto (ch=9 aw ch=7)

    dec DI              ; b3mlo decrement hena, 3lshan azwdo awl ma a5osh el loop
    mov cl,5            ; 2a5ry 3lshan dh 16 bits, 2a5ry a7ot 4 arkam, lw 7at 5, yb2a size mismatch
                        ; fl memory, lw 3ayez t5leeha byte bs, 5ly mov cl,3 badal 5 (et2kd brdo mn ValidOp8)
Checkimmediatevm:
    inc DI              ; b-check kol mra, el immediate value = kam
    dec cl              ; lw dh wesel eno b2a =0, yb2a invalid, l2no kda da5al 5 arkam

    mov bl, 48          ; ASCII of '0'
    cmp bl,[DI]
    jnz skipJump10vm
    jmp Checkimmediatevm          ; 2nd operand is 0
skipJump10vm:


    mov bl, 49          ; ASCII of '1'
    cmp bl,[DI]
    jnz skipJump11vm
    jmp Checkimmediatevm          ; 1st operand is 1
skipJump11vm:


    mov bl, 50          ; ASCII of '2'
    cmp bl,[DI]
    jnz skipJump12vm
    jmp Checkimmediatevm          ; 1st operand is 2
skipJump12vm:


    mov bl, 51          ; ASCII of '3'
    cmp bl,[DI]
    jnz skipJump13vm
    jmp Checkimmediatevm          ; 1st operand is 3
skipJump13vm:

    mov bl, 52          ; ASCII of '4'
    cmp bl,[DI]
    jnz skipJump14vm
    jmp Checkimmediatevm          ; 1st operand is 4
skipJump14vm:

    mov bl, 53          ; ASCII of '5'
    cmp bl,[DI]
    jnz skipJump15vm
    jmp Checkimmediatevm          ; 1st operand is 5
skipJump15vm:

    mov bl, 54          ; ASCII of '6'
    cmp bl,[DI]
    jnz skipJump16vm
    jmp Checkimmediatevm          ; 1st operand is 6
skipJump16vm:

    mov bl, 55          ; ASCII of '7'
    cmp bl,[DI]
    jnz skipJump17vm
    jmp Checkimmediatevm          ; 1st operand is 7
skipJump17vm:

    mov bl, 56          ; ASCII of '8'
    cmp bl,[DI]
    jnz skipJump18vm
    jmp Checkimmediatevm          ; 1st operand is 8
skipJump18vm:

    mov bl, 57          ; ASCII of '9'
    cmp bl,[DI]
    jnz skipJump19vm
    jmp Checkimmediatevm          ; 1st operand is 9
skipJump19vm:

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]
    jnz skipJump20vm
    jmp Checkimmediatevm          ; 1st operand is A
skipJump20vm:

    mov bl, 67          ; ASCII of 'B'
    cmp bl,[DI]
    jnz skipJump21vm
    jmp Checkimmediatevm          ; 1st operand is B
skipJump21vm:

    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jnz skipJump22vm
    jmp Checkimmediatevm          ; 1st operand is C
skipJump22vm:

    mov bl, 67          ; ASCII of 'D'
    cmp bl,[DI]
    jnz skipJump23vm
    jmp Checkimmediatevm          ; 1st operand is D
skipJump23vm:

    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]
    jnz skipJump24vm
    jmp Checkimmediatevm          ; 1st operand is E
skipJump24vm:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]
    jnz skipJump25vm
    jmp Checkimmediatevm          ; 1st operand is F
skipJump25vm:


    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40vm
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40vm:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump40vmEN
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40vmEN:


    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch
    ; aw 3amel spaces b3d el arkam

    ;h-handle el spaces el awl
    ; skipping some spaces
SkipSpaceaftervm:          ; Skips spaces after the immediate value
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz Skipspaceincvm     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceaftervm       ; in case it didn't jump to SkipLetters, then it's a space, skip it
Skipspaceincvm:

    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40againvm
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40againvm:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump40againvmEN
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump40againvmEN:

    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch

    mov bl,7
    cmp ch,bl           ; lw zero, yb2a valid w howa kan mda5al immediate value=0, wana 3mltlha skip

    jnz skipJump41vm
    jmp WriteOperands    ; baroo7 aktb b2a el 2 operands
skipJump41vm:



    jmp InvalidOP




SquareBracketsecondvm:
    inc DI              ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw shwyt spaces, aw memory

SkipSpace3secondvm:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc2vm     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace3secondvm       ; if it's a space, skip it
SkipInc2vm:
    ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw memory 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]
    jz SIorSP2secondvm          ; 1st operand is [SI] or [SP]

    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]
    jz bracketBPsecondvm          ; 1st operand is [BP] or [B]

    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump2secondvm
    jmp bracketDIsecondvm         ; 1st operand is [DI] or [D]
skipJump2secondvm:




    ; lw 3ada mn dol, yb2a invalid
    jmp InvalidOP

SIorSP2secondvm:       
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketsecondvm       ; 1st op is [SI]

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketsecondvm        ; 1st op is [SP]

    ; lw msh I aw P, yb2a invalid
    jmp InvalidOP


bracketBPsecondvm:      
    inc DI              ; DI dlw2ty btshawer 3la P, aw hya 5lst 5las

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketsecondvm        ; 1st op is [BP]

    ; lw msh BP, yb2a invalid, aw [B] lel memory, bs el memory invalid brdo
    jmp InvalidOP


bracketDIsecondvm:
    inc DI              ; DI dlw2ty btshawer 3la I, aw hya 5lst 5las

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketsecondvm       ; 1st op is [DI]

    ; lw msh DI, yb2a invalid, aw [D] lel memory, bs el memory invalid brdo
    jmp InvalidOP


checkbracketsecondvm:       
    inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace4secondvm:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc3vm     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI             
    jmp SkipSpace4secondvm       ; if it's a space, skip it

SkipInc3vm:

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump8secondvm                ; lw closed bracket, hy-jump
    jmp WriteOperands        ; 1st op is [SI],[DI],[SP],[BP]
skipJump8secondvm:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


    ; lw msh ], yb2a invalid
    jmp InvalidOP





XorHorLsecondvm:            ; first letter is A,C
    inc DI              ; DI dlw2ty btshawer 3la X, H, L

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump42vm
    jmp WriteOperands       ; 2nd op is AX or CX (16 bits), fa haroo7 aktbo
skipJump42vm:



    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]
    jz WriteOperands        ; 1st op is AH or CH (8 bits)

    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]
    jz WriteOperands        ; 1st op is AL or CL (8 bits)


    ; lw wesel hena, yb2a bada2 b A aw C, bs msh AX,CX
    ; yb2a invalid
    jmp InvalidOP



XorHorL2secondvm:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, P

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump43vm
    jmp WriteOperands      ; 2nd op is BX (16 bits)
skipJump43vm:


    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]
    jz WriteOperands        ; 1st op is BH (8 bits)

    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJump54vm
    jmp WriteOperands        ; 1st op is BL (8 bits)
skipJump54vm:


    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz WriteOperands        ; 1st op is BP (16 bits)


    ; lw wesel hena, yb2a bada2 b B, bs msh BX,BP
    ; yb2a invalid
    jmp InvalidOP


XorHorL3secondvm:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, I

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]
    jz WriteOperands      ; 1st op is DX (16 bits)



    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]

    jnz skipJump53vm
    jmp WriteOperands        ; 1st op is DH (8 bits)
skipJump53vm:



    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJump52vm
    jmp WriteOperands        ; 1st op is DL (8 bits)
skipJump52vm:



    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz WriteOperands      ; 1st op is DI (16 bits)

    ; lw wesel hena, yb2a bada2 b D, bs msh DX,DI
    ; yb2a invalid
    jmp InvalidOP



SIorSPsecondvm:
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz WriteOperands       ; 1st op is SI (16 bits)

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz WriteOperands     ; 1st op is SP (16 bits)

    ; lw wesel hena, yb2a bada2 b S, bs msh SI,SP
    ; yb2a invalid
    jmp InvalidOP









WriteOperands:      ; dh b3d ma et2kd en kolo valid, b-save el operands fy variable b2a 3lshan tron yst3mlhom

    ;3ayez a-handle 7war eno myektebsh el zeros el zyada elly fl awl, wala el 7roof elly b3d el 2nd operands 
    ; el 7roof handled kda 5las
    ; wl zero kaman

    ; dlw2ty DI byshawer 3la 2a5er correct character fl 2nd operand, el mfrood 3lshan tb2a valid
    ; lazem ala2y b3deeh $, aw spaces b3dein $, lw la yb2a invalid l2no mzwd 7roof 8lt b3d el command
    inc DI          ; kda DI btshawer 3la awl 7aga b3d el 2nd operand
    ; skipping some spaces
SkipSpaceaftervmcheck:          ; Skips spaces after the immediate value
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz Skipspaceincvmcheck     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceaftervmcheck       ; in case it didn't jump to SkipLetters, then it's a space, skip it
Skipspaceincvmcheck:


    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jz skipJump40vmcheck
    
    ; deft dh (3)
    mov bl,13           ; ASCII of 'enter' 
    cmp bl,[DI]

    jz skipJump40vmcheck


    jmp InvalidOP       ; hyb2a invalid lw mfeesh $ fl 2a5er
skipJump40vmcheck:
    


    mov SI, offset operand1   ; el variable elly hktb fyh el operands

    mov DI, offset command   ; el variable elly shayel el command

    ; skipping the command part
    ; skipping some spaces
SkipSpaceop:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipLettersop     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceop       ; in case it didn't jump to SkipLettersop, then it's a space, skip it



SkipLettersop:        ; skips the command itself
                        ; Code will reach this part when DI points to the start of the command
                        ; We should skip all chars till we reach a space

    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jz SkipSpace2op       ; jumps if it has a space
                        ; if it has a space, then we have skipped all letters,
                        ; and reached the beginning of the operands
    inc DI              
    jmp SkipLettersop     ; in case it didn't jump to SkipSpace2, then it's a letter, skip it



SkipSpace2op:         ; Skips spaces after the command, and before the operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc4     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op       ; if it's a space, skip it
SkipInc4:



    ; Having reached this part, DI now points at the beginning of the first operand
    ; lw howa immediate value, momkn yb2a bade2 b shwyt zeros, fa h3mlha skip


    ;3ayez azwd hena eny asheel el zeros elly fl awl [DONE]

    ; skipping some zeros
    ;mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero
                         ; mloosh lazma hena, l2n kda w kda el bta3 valid f3ln

Skipzeroszeroro:          ; Skips zeros at the beginning of 1st operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValuezeroro     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    ;mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp Skipzeroszeroro       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it


startOfImmediateValuezeroro:
    ; lama awsal hena, ana kda 3mlt skip le kol el zeros, w dlw2ty ya 2ema wa2ef 3la space aw comma
    ; ya 2ema wa2ef 3la char tany 3ady

    ; lw wa2ef 3la spaces, yb2a h3mlha skip, wl mfrood ala2y b3deeha comma
    ; yb2a kda el value kanet 0, wana 3mltlha skip, fa hktb zerohaya fy operand1, w yb2a kda 5las
    ; aroo7 3la operand2 b2a


SkipSpace2op2zeroro:         ; Skips spaces after the 1st operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5zeroro     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2zeroro       ; if it's a space, skip it
SkipInc5zeroro:

    ; lama awsl hena, ana kda 3mlt skip lel space, w wa2ef 3la comma, ya 2ema howa
    ; kan asln md5l 7aga 8eir el zero

    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]
    jnz LoadSecondOpzeroro         ; lw la2eit comma, yb2a howa kan kateb zero wana 3mltlha skip, fa
                            ; ha7ot el zero fy operand1, w aroo7 le operand2 3la tool

    ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    mov ch,'0'            ; ba5od el 0
    mov [SI],ch         ; a7oto fl SI
    ;inc SI              ; mlhash lazma 7ata n3mlha inc
    jmp LoadSecondOp     ; baroo7 ashoof el 2nd operand b2a

LoadSecondOpzeroro:








    ; Having reached this part, DI now points at the beginning of the first operand
    dec DI              ; b3mlo dec hena 3lshan azwdo awl ma a5osh el loop
WriteFirstOp:
    inc DI
    ; ha5od ay char a7oto fy Operand1, 2la lw el character dh space, h3mlo skip
    ; lw el character dh comma, yb2a operand 1 5eles

SkipSpace2op2:         ; Skips spaces after the 1st operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2       ; if it's a space, skip it
SkipInc5:

    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]
    jz LoadSecondOp         ; lw wslt lel comma, yb2a el 1st operand 5eles, 5osh 3l 2nd b2a

    ; lw wslt lel 7eta dyh, yb2a dh la space wala comma, yb2a dh char fl 1st operand
    ; fa hktbo fl SI b2a
    mov ch,[DI]         ; ba5od el value el gowa el DI
    mov [SI],ch         ; a7otaha fl SI
    inc SI
    jmp WriteFirstOp    ; byroo7 yekteb el letter el b3do


LoadSecondOp:
    mov SI, offset operand2   ; el variable elly hktb fyh el operands


    ; dlw2ty el [DI] btshawer 3la comma, fa e3ml inc DI
    inc DI          ; kda ya 2ema bshawer 3la spaces, fa h3mlha skip, ya 2ema bdayet el 2nd operand


SkipSpace2op2zeroroze:         ; Skips spaces after the comma
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5zeroroze     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2zeroroze       ; if it's a space, skip it
SkipInc5zeroroze:

    ; lama awsl hena, ana kda 3mlt skip lel space, w wa2ef 3la bdayet el 2nd operand
    ; hbtdy a-skip zeros b2a

    ; skipping some zeros
    ;mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero
                         ; mloosh lazma hena, l2n kda w kda el bta3 valid f3ln

Skipzeroszeroroze:          ; Skips zeros at the beginning of 2nd operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValuezeroroze     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    ;mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp Skipzeroszeroroze       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it


startOfImmediateValuezeroroze:
    ; lama awsal hena, ana kda 3mlt skip le kol el zeros, w dlw2ty ya 2ema wa2ef 3la space aw $
    ; ya 2ema wa2ef 3la char tany 3ady

    ; lw wa2ef 3la spaces, yb2a h3mlha skip, wl mfrood ala2y b3deeha $
    ; yb2a kda el value kanet 0, wana 3mltlha skip, fa hktb zerohaya fy operand2, w yb2a kda 5las


SkipSpace2op2zeroroze2:         ; Skips spaces after the 2nd operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5zeroroze2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2zeroroze2       ; if it's a space, skip it
SkipInc5zeroroze2:

    ; lama awsl hena, ana kda 3mlt skip lel space, w wa2ef 3la comma, ya 2ema howa
    ; kan asln md5l 7aga 8eir el zero


    mov bl, 36          ; ASCII of '$'
    cmp bl,[DI]
    jnz LoadSecondOpzeroroze         ; lw la2eit $, yb2a howa kan kateb zero wana 3mltlha skip, fa
                            ; ha7ot el zero fy operand2, w ab2a kda 5lst


    ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    mov ch,'0'            ; ba5od el 0
    mov [SI],ch         ; a7oto fl SI
    ;inc SI              ; mlhash lazma 7ata n3mlha inc
    jmp nhaya

LoadSecondOpzeroroze:




; deft dh (7)
     mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]
    jnz LoadSecondOpzerorozeEN         ; lw la2eit $, yb2a howa kan kateb zero wana 3mltlha skip, fa
                            ; ha7ot el zero fy operand2, w ab2a kda 5lst

    ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
    mov ch,'0'            ; ba5od el 0
    mov [SI],ch         ; a7oto fl SI
    ;inc SI              ; mlhash lazma 7ata n3mlha inc
    jmp nhaya
    
LoadSecondOpzerorozeEN:




    ; DI dlw2ty byshawer 3la bdayet el 2nd operand, ya 2ema spaces fa h3mlha skip 2odam
    ; bs ana awl ma ba5osh fl WriteSecondOp, bzawed el DI, fa m7tag a2alelo hena wa7da
    dec DI









WriteSecondOp:       ; habtedy aktb el 2nd operand b2a

    ; dlw2ty el [DI] btshawer 3la comma, fa e3ml inc DI
    inc DI          ; kda ya 2ema bshawer 3la spaces, fa h3mlha skip, ya 2ema bdayet el 2nd operand


    ; ha5od ay char a7oto fy Operand1, 2la lw el character dh space, h3mlo skip
    ; lw el character dh comma, yb2a operand 1 5eles

SkipSpace2op3:         ; Skips spaces after the comma
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5op     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces

    inc DI
    jmp SkipSpace2op3       ; if it's a space, skip it
SkipInc5op:


    ;3ayez azwd hena eny asheel el zeros elly fl awl [DONE]


    mov bl, 36          ; ASCII of '$'
    cmp bl,[DI]

    jnz skipJump55
    jmp nhaya         ; lw wslt lel $, yb2a el 2nd operand 5eles, etla3 b2a 5las
skipJump55:
    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump55EN
    jmp nhaya         ; lw wslt lel $, yb2a el 2nd operand 5eles, etla3 b2a 5las
skipJump55EN:
    

    ; lw wslt lel 7eta dyh, yb2a dh la space wala $, yb2a dh char fl 2nd operand
    ; fa hktbo fl SI b2a
    mov ch,[DI]         ; ba5od el value el gowa el DI
    mov [SI],ch         ; a7otaha fl SI
    inc SI
    jmp WriteSecondOp    ; byroo7 yekteb el letter el b3do








InvalidOP:  	    ; hy5osh hena lw msh valid operand
	            ; el mfrood hena n-skip el execution, w y5sr his turn, w one point
    mov valid2Chk,0     ; command is invalid, no need to continue the checks




nhaya: ;eb2a sheel 7etet el test dyh

    ; mov ax, 0
    ; mov  ah, 02     ;SetCursorPosition
    ; mov dl, 0      ;X coordinated
    ; mov dh, 0      ;Ycoordinated
    ; mov bx, 0
    ; int 10h

    ; mov ah, 9
    ; mov dx, offset operand1
    ; int 21h
    
    
	popf
	popa
	

ENDM
	
