.286
TwoOpS MACRO command, operand1, operand2, valid2Chk


    pusha
	pushf
	mov DI, offset command   ; The variable containing the command
	
	; fy 7alet el no operands, h-skip el kalam dh kolo, w nroo7 lel execution of command


    
    ; skipping the command part
    ; skipping some spaces
SkipSpaceOP2:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipLettersOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceOP2       ; in case it didn't jump to SkipLetters, then it's a space, skip it





SkipLettersOP2:        ; skips the command itself
                        ; Code will reach this part when DI points to the start of the command
                        ; We should skip all chars till we reach a space

    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jz SkipSpace2OP2S       ; jumps if it has a space
                        ; if it has a space, then we have skipped all letters,
                        ; and reached the beginning of the operands
    inc DI              
    jmp SkipLettersOP2     ; in case it didn't jump to SkipSpace2, then it's a letter, skip it





SkipSpace2OP2S:         ; Skips spaces after the command, and before the operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipIncOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command

    ; pushf               ; 3lshan a7tafez bl value of zero flag abl el inc DI
    inc DI
    ; popf                ; 3lshan arg3o tany

    jmp SkipSpace2OP2S       ; if it's a space, skip it
SkipIncOP2:


    ; Having reached this part, DI now points at the beginning of the first operand

    ; In case of commands having 2 operands: 
    ; ADD - ADC- SUB - SBB - MOV - XOR - AND - OR   ; dol lyhom nafs el sheroot (fl valid operands y3ny)
    ; SHR - SHL - ROR                               ; dol lyhom nafs el sheroot (fl valid operands y3ny)


TwoOperandsSecondTypeOP2:       ; handles operands of SHR - SHL - ROR
    ; ST y3ny Second Type

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]

    jnz skipJump26STOP2
    jmp XorHorLSTOP2          ; 1st operand is AX or AH or AL
skipJump26STOP2:


    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]

    jnz skipJump27STOP2
    jmp XorHorL2STOP2          ; 1st operand is BX or BH or BL or BP
skipJump27STOP2:



    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jnz skipJump28STOP2
    jmp XorHorLSTOP2          ; 1st operand is CX or CH or CL
skipJump28STOP2:



    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump29STOP2
    jmp XorHorL3STOP2         ; 1st operand is DX or DH or DL or DI
skipJump29STOP2:


    ; lw 3ada mn kol dh, yb2a msh byebda2 b A,B,C,D

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]

    jnz skipJump30STOP2
    jmp SIorSPSTOP2          ; 1st operand is SI or SP
skipJump30STOP2:



    mov bl, 91          ; ASCII of '['
    cmp bl,[DI]

    jnz skipJump31STOP2
    jmp SquareBracketSTOP2          ; 1st operand is [SP],[SI],[DI],[BP]
skipJump31STOP2:

    ; lw wsl l7d hena, yb2a msh byebda2 b A,B,C,D,S,[
    ; yb2a invalid
    jmp InvalidOPOP2




XorHorLSTOP2:            ; first letter is A,C
    inc DI              ; DI dlw2ty btshawer 3la X, H, L

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump44STOP2
    jmp ValidateSecondOP2       ; 1st op is AX or CX (16 bits)
skipJump44STOP2:


    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]
    jnz skipJump44ST1OP2
    jmp ValidateSecondOP2        ; 1st op is AH or CH (8 bits)
skipJump44ST1OP2:

    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]
    jnz skipJump44ST2OP2
    jmp ValidateSecondOP2        ; 1st op is AL or CL (8 bits)
skipJump44ST2OP2:

    ; lw wesel hena, yb2a bada2 b A aw C, bs msh AL,AH,AX,Cl,Ch,CX
    ; yb2a invalid
    jmp InvalidOPOP2





XorHorL2STOP2:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, P

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]

    jnz skipJump45STOP2
    jmp ValidateSecondOP2       ; 1st op is BX (16 bits)
skipJump45STOP2:


    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]
    jnz skipJump44ST3OP2
    jmp ValidateSecondOP2        ; 1st op is BH (8 bits)
skipJump44ST3OP2:

    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJump54STOP2
    jmp ValidateSecondOP2        ; 1st op is BL (8 bits)
skipJump54STOP2:


    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         

    jnz skipJump46STOP2
    jmp ValidateSecondOP2        ; 1st op is BP (16 bits)
skipJump46STOP2:



    ; lw wesel hena, yb2a bada2 b B, bs msh BL,BH,BX,BP
    ; yb2a invalid
    jmp InvalidOPOP2


XorHorL3STOP2:
    inc DI              ; DI dlw2ty btshawer 3la X, H, L, I

    mov bl, 88          ; ASCII of 'X'
    cmp bl,[DI]


    jnz skipJump47STOP2
    jmp ValidateSecondOP2       ; 1st op is DX (16 bits)
skipJump47STOP2:


    mov bl, 72          ; ASCII of 'H'
    cmp bl,[DI]

    jnz skipJump53STOP2
    jmp ValidateSecondOP2        ; 1st op is DH (8 bits)
skipJump53STOP2:



    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jnz skipJump52STOP2
    jmp ValidateSecondOP2        ; 1st op is DL (8 bits)
skipJump52STOP2:


    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]

    jnz skipJump48STOP2
    jmp ValidateSecondOP2        ; 1st op is DI (16 bits)
skipJump48STOP2:


    ; lw wesel hena, yb2a bada2 b D, bs msh DL,DH,DX,DI
    ; yb2a invalid
    jmp InvalidOPOP2



SIorSPSTOP2:
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]

    jnz skipJump49STOP2
    jmp ValidateSecondOP2       ; 1st op is SI (16 bits)
skipJump49STOP2:


    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         

    jnz skipJump50STOP2
    jmp ValidateSecondOP2        ; 1st op is SP (16 bits)
skipJump50STOP2:


    ; lw wesel hena, yb2a bada2 b S, bs msh SI,SP
    ; yb2a invalid
    jmp InvalidOPOP2




SquareBracketSTOP2:
    inc DI              ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw shwyt spaces, aw memory

SkipSpace3STOP2:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc6STOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace3STOP2       ; if it's a space, skip it
SkipInc6STOP2:

    ; DI dlw2ty btshawer 3la SI,DI,SP,BP, aw memory 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F

    mov bl, 83          ; ASCII of 'S'
    cmp bl,[DI]
    jz SIorSP2STOP2          ; 1st operand is [SI] or [SP]

    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]
    jz bracketBPSTOP2          ; 1st operand is [BP] or [B]

    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump2STOP2
    jmp bracketDISTOP2          ; 1st operand is [DI] or [D]
skipJump2STOP2:

    mov bl, 48          ; ASCII of '0'
    cmp bl,[DI]
    jnz skipJump3STOP2
    jmp checkbracketmemorySTOP2          ; 1st operand is [0]
skipJump3STOP2:


    mov bl, 49          ; ASCII of '1'
    cmp bl,[DI]
    jnz skipJump4STOP2
    jmp checkbracketmemorySTOP2          ; 1st operand is [1]
skipJump4STOP2:


    mov bl, 50          ; ASCII of '2'
    cmp bl,[DI]
    jnz skipJump5STOP2
    jmp checkbracketmemorySTOP2          ; 1st operand is [2]
skipJump5STOP2:


    mov bl, 51          ; ASCII of '3'
    cmp bl,[DI]
    jnz skipJump6STOP2
    jmp checkbracketmemorySTOP2          ; 1st operand is [3]
skipJump6STOP2:

    mov bl, 52          ; ASCII of '4'
    cmp bl,[DI]

    jnz skipJump51STOP2
    jmp checkbracketmemorySTOP2         ; 1st operand is [4]
skipJump51STOP2:


    mov bl, 53          ; ASCII of '5'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2         ; 1st operand is [5]

    mov bl, 54          ; ASCII of '6'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2       ; 1st operand is [6]

    mov bl, 55          ; ASCII of '7'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2          ; 1st operand is [7]

    mov bl, 56          ; ASCII of '8'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2          ; 1st operand is [8]

    mov bl, 57          ; ASCII of '9'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2          ; 1st operand is [9]

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2          ; 1st operand is [A]

    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2          ; 1st operand is [C]

    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2          ; 1st operand is [E]

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]
    jz checkbracketmemorySTOP2          ; 1st operand is [F]

    ; lw 3ada mn dol, yb2a invalid
    jmp InvalidOPOP2

SIorSP2STOP2:       
    inc DI              ; DI dlw2ty btshawer 3la I, P

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketSTOP2       ; 1st op is [SI]

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketSTOP2        ; 1st op is [SP]

    ; lw msh I aw P, yb2a invalid
    jmp InvalidOPOP2


bracketBPSTOP2:      
    inc DI              ; DI dlw2ty btshawer 3la P, aw hya 5lst 5las

    mov bl, 80          ; ASCII of 'P'
    cmp bl,[DI]         
    jz checkbracketSTOP2        ; 1st op is [BP]

    ; lw msh BP, yb2a invalid, aw [B] lel memory
    ; lw hya [B] lel memory, yb2a el mfrood dlw2ty yb2a bracket, aw shwyt spaces

    ; h-check lw fy spaces, a3mlohom skip, b3dein el mfrood ala2y bracket, lw la yb2a invalid
SkipSpace5STOP2:         ; Skips spaces before the ]
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc7STOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace5STOP2      ; if it's a space, skip it
SkipInc7STOP2:
    ; kda el DI btshawer 3la ] aw invalid

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJumpSTOP2                ; lw closed bracket, hy-jump lel invalid
    jmp ValidateSecondOP2        ; 1st op is [B]
skipJumpSTOP2:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid

    jmp InvalidOPOP2



bracketDISTOP2:
    inc DI              ; DI dlw2ty btshawer 3la I, aw hya 5lst 5las

    mov bl, 73          ; ASCII of 'I'
    cmp bl,[DI]
    jz checkbracketSTOP2       ; 1st op is [DI]


    ; lw msh DI, yb2a invalid, aw [D] lel memory
    ; lw hya [D] lel memory, yb2a el mfrood dlw2ty yb2a bracket, aw shwyt spaces

    ; h-check lw fy spaces, a3mlohom skip, b3dein el mfrood ala2y bracket, lw la yb2a invalid
SkipSpace6STOP2:         ; Skips spaces before the ]
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc8STOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace6STOP2      ; if it's a space, skip it
SkipInc8STOP2:
    ; kda el DI btshawer 3la ] aw invalid

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump7STOP2                ; lw closed bracket, hy-jump lel invalid
    jmp ValidateSecondOP2        ; 1st op is [D]
skipJump7STOP2:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid



    jmp InvalidOPOP2




checkbracketmemorySTOP2:

    inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace7STOP2:         ; Skips spaces after the letter, and before the ]
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc9STOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace7STOP2       ; if it's a space, skip it
SkipInc9STOP2:


    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump9STOP2                ; lw closed bracket, hy-jump
    jmp ValidateSecondOP2        ; 1st op is memory: [0],[1],...[D],[E],[F]
skipJump9STOP2:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


    ; lw msh ], yb2a invalid
    jmp InvalidOPOP2


checkbracketSTOP2:       
    inc DI              ; DI dlw2ty btshawer 3la ] aw shwyt spaces

SkipSpace4STOP2:         ; Skips spaces after the [, and before the letter
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc10STOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI              
    jmp SkipSpace4STOP2       ; if it's a space, skip it
SkipInc10STOP2:

    mov bl, 93          ; ASCII of ']'
    cmp bl,[DI]         
    ; modtar a3ml el jump bl shakl el mo3aq dh
    ; 3lshan myenfa3sh a3ml jz far ValidOp16
    jnz skipJump8STOP2                ; lw closed bracket, hy-jump
    jmp ValidateSecondOP2        ; 1st op is [SI],[DI],[SP],[BP]
skipJump8STOP2:
    ; lw wesel le hena, yb2a msh closed bracket, yb2a invalid


    ; lw msh ], yb2a invalid
    jmp InvalidOPOP2






    ; h5osh fl 2nd operand b2a

ValidateSecondOP2:
    ;VS means validate second

    ; 2nd operand is either CL, or a byte worth of 
    ; immediate values ranging from 00 to 0FF


    mov ch,9            ; dh el flag bta3 el zero (hyban ta7t aktr)
    inc DI
    ; el mfrood DI dlw2ty btshawer 3la space, aw comma, lw la yb2a invalid

    ; skipping some spaces
SkipSpacesecondVSOP2:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz skipCommaVSOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecondVSOP2       ; in case it didn't jump to SkipComma, then it's a space, skip it

skipCommaVSOP2:
    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]

    jz skipJump32VSOP2
    jmp InvalidOPOP2       ; if the spaces aren't followed with a comma, then it's invalid
skipJump32VSOP2:

    inc DI              ; lw wesel hena, yb2a el operand el awlany tmam l7d el comma, hbtdy fl tany b2a
                        ; hshoof lw fy spaces el awl


    ; skipping some spaces
SkipSpacesecond2VSOP2:          ; Skips spaces before the 2nd operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz startOfSecondOperandVSOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the 2nd operand
    inc DI
    jmp SkipSpacesecond2VSOP2       ; in case it didn't jump to SkipComma, then it's a space, skip it


startOfSecondOperandVSOP2:



    ;DI now points to the beginning of the 2nd operand

    ; habos el awl, lw la2eit DI btshawer 3la $, yb2a howa mkatabsh 7aga fl 2nd operand
    ; yb2a invalid

    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump33VSOP2
    
    jmp InvalidOPOP2    ; baroo7 aktb b2a el 2 operands
skipJump33VSOP2:
    ; deft dh (5)
    mov bl,13           ; ASCII of 'Enter' 
    cmp bl,[DI]

    jnz skipJump33VSOP2EN
    
    jmp InvalidOPOP2    ; baroo7 aktb b2a el 2 operands
skipJump33VSOP2EN:

    ; dlw2ty DI btshawer 3la bdayet el 2nd operand

    mov bl, 67          ; ASCII of 'C'
    cmp bl,[DI]
    jnz skipJump28VSOP2
    jmp XorHorLVSOP2          ; 1st operand is CL
skipJump28VSOP2:

    ; lw msh C, habos lw howa value

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]

    jnz skipJump37EAOP2
    jmp InvalidOPOP2         ; Invalid as it starts with A not 0A
skipJump37EAOP2:

    mov bl, 66          ; ASCII of 'B'
    cmp bl,[DI]

    jnz skipJump37FBOP2
    jmp InvalidOPOP2         ; Invalid as it starts with B not 0B
skipJump37FBOP2:



    mov bl, 68          ; ASCII of 'D'
    cmp bl,[DI]

    jnz skipJump37EDOP2
    jmp InvalidOPOP2         ; Invalid as it starts with D not 0D
skipJump37EDOP2:



    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]

    jnz skipJump37EEOP2
    jmp InvalidOPOP2         ; Invalid as it starts with E not 0E
skipJump37EEOP2:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]

    jnz skipJump37FFOP2
    jmp InvalidOPOP2         ; Invalid as it starts with F not 0F
skipJump37FFOP2:


    ; skipping some zeros
    mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero

SkipzerosVSOP2:          ; Skips zeros at the beginning of 2nd operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValueVSOP2     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp SkipzerosVSOP2       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it



startOfImmediateValueVSOP2:      ; lama awsl hena, ana kda tanesht kol el zeros elly fl awl, w ebtadeit fl value nafsaha

    ;bs el fekra en el value momkn tb2a zero asln, fana kda 3mltlha skip, lazem a-handle el 7eta dyh
    ; 3mltlo handle bl flag el zwdto (ch=9 aw ch=7)

    dec DI              ; b3mlo decrement hena, 3lshan azwdo awl ma a5osh el loop
    mov cl,5            ; 2a5ry 3lshan dh 16 bits, 2a5ry a7ot 4 arkam, lw 7at 5, yb2a size mismatch
CheckimmediateVSOP2:
    inc DI              ; b-check kol mra, el immediate value = kam
    dec cl              ; lw dh wesel eno b2a =0, yb2a invalid, l2no kda da5al 5 arkam

    mov bl, 48          ; ASCII of '0'
    cmp bl,[DI]
    jnz skipJump10VSOP2
    jmp CheckimmediateVSOP2          ; 2nd operand is 0
skipJump10VSOP2:


    mov bl, 49          ; ASCII of '1'
    cmp bl,[DI]
    jnz skipJump11VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 1
skipJump11VSOP2:


    mov bl, 50          ; ASCII of '2'
    cmp bl,[DI]
    jnz skipJump12VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 2
skipJump12VSOP2:


    mov bl, 51          ; ASCII of '3'
    cmp bl,[DI]
    jnz skipJump13VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 3
skipJump13VSOP2:

    mov bl, 52          ; ASCII of '4'
    cmp bl,[DI]
    jnz skipJump14VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 4
skipJump14VSOP2:

    mov bl, 53          ; ASCII of '5'
    cmp bl,[DI]
    jnz skipJump15VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 5
skipJump15VSOP2:

    mov bl, 54          ; ASCII of '6'
    cmp bl,[DI]
    jnz skipJump16VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 6
skipJump16VSOP2:

    mov bl, 55          ; ASCII of '7'
    cmp bl,[DI]
    jnz skipJump17VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 7
skipJump17VSOP2:

    mov bl, 56          ; ASCII of '8'
    cmp bl,[DI]
    jnz skipJump18VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 8
skipJump18VSOP2:

    mov bl, 57          ; ASCII of '9'
    cmp bl,[DI]
    jnz skipJump19VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is 9
skipJump19VSOP2:

    mov bl, 65          ; ASCII of 'A'
    cmp bl,[DI]
    jnz skipJump20VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is A
skipJump20VSOP2:

    mov bl, 67          ; ASCII of 'B'
    cmp bl,[DI]
    jnz skipJump21VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is B
skipJump21VSOP2:

    mov bl, 67          ; ASCII of 'C'      ; dyh msh hywsalaha, 8eir lw kan bady b shwyt zeros fl awl
    cmp bl,[DI]
    jnz skipJump22VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is C
skipJump22VSOP2:

    mov bl, 67          ; ASCII of 'D'
    cmp bl,[DI]
    jnz skipJump23VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is D
skipJump23VSOP2:

    mov bl, 69          ; ASCII of 'E'
    cmp bl,[DI]
    jnz skipJump24VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is E
skipJump24VSOP2:

    mov bl, 70          ; ASCII of 'F'
    cmp bl,[DI]
    jnz skipJump25VSOP2
    jmp CheckimmediateVSOP2          ; 1st operand is F
skipJump25VSOP2:


    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40VSOP2
    jmp WriteOperandsOP2    ; baroo7 aktb b2a el 2 operands
skipJump40VSOP2:
    ; deft dh (5)
    mov bl,13           ; ASCII of 'Enter' 
    cmp bl,[DI]

   jnz skipJump40VSOP2EN
    jmp WriteOperandsOP2    ; baroo7 aktb b2a el 2 operands
skipJump40VSOP2EN:

    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch
    ; aw 3amel spaces b3d el arkam

    ;h-handle el spaces el awl
    ; skipping some spaces
SkipSpaceafterVSOP2:          ; Skips spaces after the immediate value
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipspaceincVSOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceafterVSOP2       ; in case it didn't jump to SkipLetters, then it's a space, skip it
SkipspaceincVSOP2:

    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40againVSOP2
    jmp WriteOperandsOP2    ; baroo7 aktb b2a el 2 operands
skipJump40againVSOP2:
    ; deft dh (5)
    mov bl,13           ; ASCII of 'Enter' 
    cmp bl,[DI]

    jnz skipJump40againVSOP2EN
    jmp WriteOperandsOP2    ; baroo7 aktb b2a el 2 operands
skipJump40againVSOP2EN:




    ; lw wesel hena, yb2a ya 2ema invalid, ya 2ema kan immediate value
    ; kan 7atet esfar, wana 3mltlha skip. lw kda, yb2a ch=7, fa h-check bl ch

    mov bl,7
    cmp ch,bl           ; lw zero, yb2a valid w howa kan mda5al immediate value=0, wana 3mltlha skip

    jnz skipJump41VSOP2
    jmp WriteOperandsOP2    ; baroo7 aktb b2a el 2 operands
skipJump41VSOP2:

    ; lw wesel hena, yb2a invalid
    jmp InvalidOPOP2




    ; lw kan ebtda b C 3la tool b2a, yb2a howa ya 2ema immediate value 5lst 3la kda
    ; ya 2ema register CL, fa dlw2ty habos howa L wala la

XorHorLVSOP2:            ; first letter is C
    inc DI              ; DI dlw2ty btshawer 3la L, aw shwyt spaces, aw $

    mov bl, 76          ; ASCII of 'L'
    cmp bl,[DI]

    jz skipJumpa8VSOP2
    jmp InvalidOPOP2        ; 2nd op is not CL, yb2a invalid
skipJumpa8VSOP2:

    ;lw hena 5aletha badal invalidOP, 5aletha WriteOperands. htozbot; la nvm 8lt


    ; lw wesel hena, yb2a howa dlw2ty CL, el gy b2a hyb2a
    ; ya 2ema spaces, ya 2ema 5eles, ya 2ema invalid
    inc DI  ; kda bshawer 3la el ba3d el CL

SkipSpace2op3VSOP2:         ; Skips spaces after the CL
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5opVSOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces

    inc DI
    jmp SkipSpace2op3VSOP2       ; if it's a space, skip it
SkipInc5opVSOP2:

    ; kda 3mlt skip lel spaces, el mfrood yb2a 5eles dlw2ty

    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jnz skipJump40vmVSOP2
    jmp WriteOperandsOP2    ; baroo7 aktb b2a el 2 operands
skipJump40vmVSOP2:
    ; deft dh (5)
    mov bl,13           ; ASCII of 'Enter' 
    cmp bl,[DI]

    jnz skipJump40vmVSOP2EN
    jmp WriteOperandsOP2    ; baroo7 aktb b2a el 2 operands
skipJump40vmVSOP2EN:



    ; lw wesel le hena, yb2a msh bady bl C, w msh immediate
    ; yb2a invalid
    jmp InvalidOPOP2



WriteOperandsOP2:      ; dh b3d ma et2kd en kolo valid, b-save el operands fy variable b2a 3lshan tron yst3mlhom

    ;3ayez a-handle 7war eno myektebsh el zeros el zyada elly fl awl, wala el 7roof elly b3d el 2nd operands 
    ; el 7roof handled kda 5las
    ; wl zero kaman

    ; dlw2ty DI byshawer 3la 2a5er correct character fl 2nd operand, el mfrood 3lshan tb2a valid
    ; lazem ala2y b3deeh $, aw spaces b3dein $, lw la yb2a invalid l2no mzwd 7roof 8lt b3d el command
    inc DI          ; kda DI btshawer 3la awl 7aga b3d el 2nd operand
    ; skipping some spaces
SkipSpaceaftervmcheckOP2:          ; Skips spaces after the immediate value
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipspaceincvmcheckOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceaftervmcheckOP2       ; in case it didn't jump to SkipLetters, then it's a space, skip it
SkipspaceincvmcheckOP2:


    ; checking if the end of the 2nd operand has been reached
    mov bl,36           ; ASCII of '$' 
    cmp bl,[DI]

    jz skipJump40vmcheckOP2
    ; deft dh (3)
    mov bl,13           ; ASCII of 'enter' 
    cmp bl,[DI]

    jz skipJump40vmcheckOP2
    jmp InvalidOPOP2       ; hyb2a invalid lw mfeesh $ fl 2a5er
skipJump40vmcheckOP2:



    mov SI, offset operand1   ; el variable elly hktb fyh el operands

    mov DI, offset command   ; el variable elly shayel el command

    ; skipping the command part
    ; skipping some spaces
SkipSpaceopOP2:          ; Skips spaces before the command
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipLettersopOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,
                        ; and reached the beginning of the command
    inc DI
    jmp SkipSpaceopOP2       ; in case it didn't jump to SkipLettersop, then it's a space, skip it



SkipLettersopOP2:        ; skips the command itself
                        ; Code will reach this part when DI points to the start of the command
                        ; We should skip all chars till we reach a space

    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jz SkipSpace2opOP2       ; jumps if it has a space
                        ; if it has a space, then we have skipped all letters,
                        ; and reached the beginning of the operands
    inc DI              
    jmp SkipLettersopOP2     ; in case it didn't jump to SkipSpace2, then it's a letter, skip it



SkipSpace2opOP2:         ; Skips spaces after the command, and before the operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc4OP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2opOP2       ; if it's a space, skip it
SkipInc4OP2:



    ; Having reached this part, DI now points at the beginning of the first operand
    ; lw howa immediate value, momkn yb2a bade2 b shwyt zeros, fa h3mlha skip


    ;3ayez azwd hena eny asheel el zeros elly fl awl [DONE]

    ; skipping some zeros
    ;mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero
                         ; mloosh lazma hena, l2n kda w kda el bta3 valid f3ln

SkipzeroszeroroOP2:          ; Skips zeros at the beginning of 1st operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValuezeroroOP2     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    ;mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp SkipzeroszeroroOP2       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it


startOfImmediateValuezeroroOP2:
    ; lama awsal hena, ana kda 3mlt skip le kol el zeros, w dlw2ty ya 2ema wa2ef 3la space aw comma
    ; ya 2ema wa2ef 3la char tany 3ady

    ; lw wa2ef 3la spaces, yb2a h3mlha skip, wl mfrood ala2y b3deeha comma
    ; yb2a kda el value kanet 0, wana 3mltlha skip, fa hktb zerohaya fy operand1, w yb2a kda 5las
    ; aroo7 3la operand2 b2a


SkipSpace2op2zeroroOP2:         ; Skips spaces after the 1st operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5zeroroOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2zeroroOP2       ; if it's a space, skip it
SkipInc5zeroroOP2:

    ; lama awsl hena, ana kda 3mlt skip lel space, w wa2ef 3la comma, ya 2ema howa
    ; kan asln md5l 7aga 8eir el zero

    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]
    jnz LoadSecondOpzeroroOP2         ; lw la2eit comma, yb2a howa kan kateb zero wana 3mltlha skip, fa
                            ; ha7ot el zero fy operand1, w aroo7 le operand2 3la tool


    mov ch,'0'            ; ba5od el 0
    mov [SI],ch         ; a7oto fl SI
    ;inc SI              ; mlhash lazma 7ata n3mlha inc
    jmp LoadSecondOpOP2     ; baroo7 ashoof el 2nd operand b2a

LoadSecondOpzeroroOP2:








    ; Having reached this part, DI now points at the beginning of the first operand
    dec DI              ; b3mlo dec hena 3lshan azwdo awl ma a5osh el loop
WriteFirstOpOP2:
    inc DI
    ; ha5od ay char a7oto fy Operand1, 2la lw el character dh space, h3mlo skip
    ; lw el character dh comma, yb2a operand 1 5eles

SkipSpace2op2OP2:         ; Skips spaces after the 1st operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5OP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2OP2       ; if it's a space, skip it
SkipInc5OP2:

    mov bl, 44          ; ASCII of ','
    cmp bl,[DI]
    jz LoadSecondOpOP2         ; lw wslt lel comma, yb2a el 1st operand 5eles, 5osh 3l 2nd b2a

    ; lw wslt lel 7eta dyh, yb2a dh la space wala comma, yb2a dh char fl 1st operand
    ; fa hktbo fl SI b2a
    mov ch,[DI]         ; ba5od el value el gowa el DI
    mov [SI],ch         ; a7otaha fl SI
    inc SI
    jmp WriteFirstOpOP2    ; byroo7 yekteb el letter el b3do


LoadSecondOpOP2:
    mov SI, offset operand2   ; el variable elly hktb fyh el operands


    ; dlw2ty el [DI] btshawer 3la comma, fa e3ml inc DI
    inc DI          ; kda ya 2ema bshawer 3la spaces, fa h3mlha skip, ya 2ema bdayet el 2nd operand


SkipSpace2op2zerorozeOP2:         ; Skips spaces after the comma
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5zerorozeOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2zerorozeOP2       ; if it's a space, skip it
SkipInc5zerorozeOP2:

    ; lama awsl hena, ana kda 3mlt skip lel space, w wa2ef 3la bdayet el 2nd operand
    ; hbtdy a-skip zeros b2a

    ; skipping some zeros
    ;mov ch,9            ; ba7ot ay value fl ch hena, w h8yrha lw 3mlt skip le 3la el a2al 1 zero
                         ; mloosh lazma hena, l2n kda w kda el bta3 valid f3ln

SkipzeroszerorozeOP2:          ; Skips zeros at the beginning of 2nd operand
    mov bl, 48         ; ASCII of zero
    cmp bl,[DI]         ; checks if the DI currently has a zero
    jnz startOfImmediateValuezerorozeOP2     ; jumps if DI doesn't have a zero
                        ; if it doesn't have a zero, then we have skipped all zeros,
                        ; and reached the beginning of the immediate value
    ;mov ch,7            ; 7atet hena value tanya, lw el ch=7 dlw2ty, dh kda m3nah eny 3mlt skip le 3l a2al 1 zero                    
    inc DI
    jmp SkipzeroszerorozeOP2       ; in case it didn't jump to startOfImmediateValue, then it's a zero, skip it


startOfImmediateValuezerorozeOP2:
    ; lama awsal hena, ana kda 3mlt skip le kol el zeros, w dlw2ty ya 2ema wa2ef 3la space aw $
    ; ya 2ema wa2ef 3la char tany 3ady

    ; lw wa2ef 3la spaces, yb2a h3mlha skip, wl mfrood ala2y b3deeha $
    ; yb2a kda el value kanet 0, wana 3mltlha skip, fa hktb zerohaya fy operand2, w yb2a kda 5las


SkipSpace2op2zeroroze2OP2:         ; Skips spaces after the 2nd operand
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5zeroroze2OP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces,

    inc DI
    jmp SkipSpace2op2zeroroze2OP2       ; if it's a space, skip it
SkipInc5zeroroze2OP2:

    ; lama awsl hena, ana kda 3mlt skip lel space, w wa2ef 3la comma, ya 2ema howa
    ; kan asln md5l 7aga 8eir el zero


    mov bl, 36          ; ASCII of '$'
    cmp bl,[DI]
    jnz LoadSecondOpzerorozeOP2         ; lw la2eit $, yb2a howa kan kateb zero wana 3mltlha skip, fa
                            ; ha7ot el zero fy operand2, w ab2a kda 5lst


    mov ch,'0'            ; ba5od el 0
    mov [SI],ch         ; a7oto fl SI
    ;inc SI              ; mlhash lazma 7ata n3mlha inc
    jmp nhayaOP2

LoadSecondOpzerorozeOP2:
    
    ; deft dh (7)
     mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]
    jnz LoadSecondOpzerorozeOP2EN         ; lw la2eit $, yb2a howa kan kateb zero wana 3mltlha skip, fa
                            ; ha7ot el zero fy operand2, w ab2a kda 5lst


    mov ch,'0'            ; ba5od el 0
    mov [SI],ch         ; a7oto fl SI
    ;inc SI              ; mlhash lazma 7ata n3mlha inc
    jmp nhayaOP2
    
LoadSecondOpzerorozeOP2EN:
    
    
    
    ; DI dlw2ty byshawer 3la bdayet el 2nd operand, ya 2ema spaces fa h3mlha skip 2odam
    ; bs ana awl ma ba5osh fl WriteSecondOp, bzawed el DI, fa m7tag a2alelo hena wa7da
    dec DI









WriteSecondOpOP2:       ; habtedy aktb el 2nd operand b2a

    ; dlw2ty el [DI] btshawer 3la comma, fa e3ml inc DI
    inc DI          ; kda ya 2ema bshawer 3la spaces, fa h3mlha skip, ya 2ema bdayet el 2nd operand


    ; ha5od ay char a7oto fy Operand1, 2la lw el character dh space, h3mlo skip
    ; lw el character dh comma, yb2a operand 1 5eles

SkipSpace2op3OP2:         ; Skips spaces after the comma
    mov bl, 32          ; ASCII of space
    cmp bl,[DI]         ; checks if the DI currently has a space
    jnz SkipInc5opOP2     ; jumps if DI doesn't have a space
                        ; if it doesn't have a space, then we have skipped all spaces

    inc DI
    jmp SkipSpace2op3OP2       ; if it's a space, skip it
SkipInc5opOP2:


    ;3ayez azwd hena eny asheel el zeros elly fl awl [DONE]


    mov bl, 36          ; ASCII of '$'
    cmp bl,[DI]

    jnz skipJump55OP2
    jmp nhayaOP2         ; lw wslt lel $, yb2a el 2nd operand 5eles, etla3 b2a 5las
skipJump55OP2:

    ; deft dh (5)
    mov bl, 13          ; ASCII of 'enter'
    cmp bl,[DI]

    jnz skipJump55OP2EN
    jmp nhayaOP2         ; lw wslt lel $, yb2a el 2nd operand 5eles, etla3 b2a 5las
skipJump55OP2EN:

    ; lw wslt lel 7eta dyh, yb2a dh la space wala $, yb2a dh char fl 2nd operand
    ; fa hktbo fl SI b2a
    mov ch,[DI]         ; ba5od el value el gowa el DI
    mov [SI],ch         ; a7otaha fl SI
    inc SI
    jmp WriteSecondOpOP2    ; byroo7 yekteb el letter el b3do







InvalidOPOP2:  	    ; hy5osh hena lw msh valid operand
	            ; el mfrood hena n-skip el execution, w y5sr his turn, w one point

    mov valid2Chk,0     ; command is invalid, no need to continue the checks


nhayaOP2: 
    
    
	popf
	popa



ENDM
	
