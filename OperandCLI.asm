.model compact
.386
.stack 64
.data
SRC db '[SI]' 
SRCend db '$' ;source terminator
lenSRC db ?   ;source length
DEST db 'CX'
DESTend db '$' ;destination terminator
lenDEST db ?   ;destination length
WhichPlayer db 2
AddressTypeSRC db 0 ; 1: immediate, 2: Register, 3: Direct, 4: Register Indirect
AddressTypeDEST db 0 ; 1: immediate, 2: Register, 3: Direct, 4: Register Indirect
MemoryTypeSRC db ? ; 1: 8-bits 2: 16-Bits
MemoryTypeDEST db ? ; 1: 8-bits 2: 16-Bits
AddressingMode db ? ; 1: immediate, 2: Register, 3: Direct, 4: Register Indirect
Points1 dw 100
Points2 dw 200
;Processor1 data
ax1 label byte ;First player AX register
al1 db 30h,30h
ah1 db 30h,30h

bx1 label byte ;First player BX register
bl1 db 30h,30h
bh1 db 30h,30h

cx1 label byte ;First player CX register
cl1 db 30h,30h
ch1 db 30h,30h

dx1 label byte ;First player DX register
dl1 db 30h,30h
dh1 db 30h,30h

si1 db 30h,30h,30h,30h ;First player SI register
di1 db 30h,30h,30h,30h ;First player DI register
sp1 db 30h,30h,30h,30h ;First player SP register
bp1 db 30h,30h,30h,30h ;First player BP register

ImmediateValue1 dw ?

mem1 db 32 DUP(30h) ;First player memory

;Processor2 data
ax2 label byte ;Second Player AX register
al2 db 30h,30h
ah2 db 30h,30h

bx2 label byte ;Second Player BX register
bl2 db 30h,30h
bh2 db 30h,30h

cx2 label byte ;Second Player CX register
cl2 db 30h,30h
ch2 db 30h,30h

dx2 label byte ;Second Player DX register
dl2 db 30h,30h
dh2 db 30h,30h

si2 db 30h,30h,30h,30h ;Second Player SI register
di2 db 30h,30h,30h,30h ;Second Player DI register
sp2 db 30h,30h,30h,30h ;Second Player SP register
bp2 db 30h,30h,30h,30h ;Second Player BP register

ImmediateValue2 dw ?

mem2 db 32 DUP(30h) ;Second player memory

.code
main proc far
mov ax,@data
mov ds,ax
mov es,ax
mov ax,0
mov bx,offset cx1
mov di,offset SRC
mov cl,0Ah
mov al,'$'
REPNE SCASB
mov dl,10
sub dl,cl
sub dl,1
mov lenSRC,dl
mov di,offset DEST
mov cl,0Ah
mov al,'$'
REPNE SCASB
mov dl,10
sub dl,cl
sub dl,1
mov lenDEST,dl
mov si,offset SRC
mov cl,2h
mov bh,[si] ;handles the first iteration of below loop
cmp bh,5Bh
jz Capcheck
cmp bh,5Dh
jz Capcheck
cmp bh,61h
jl Capcheck
sub bh,20h
mov [si],bh
jmp Capcheck
Capcheck: ;make all characters Capital
dec cl
jz endCapcheck ;check if string length is reached
inc si
mov bh,[si] ;take next letter from offset
cmp bh,5Bh ;compare to '[' if it is a square bracket skip this letter
jz Capcheck
cmp bh,5Dh ;compare to ']' if it is a square bracket skip this letter
jz Capcheck
cmp bh,61h 
jl Capcheck ;compare the char to 61h -> 'a' if it is less than 'a' skip it and move on to the next char
sub bh,20h ;convert small letter to capital letter
mov [si],bh ;update offset
jmp Capcheck ;return to start of loop
endCapcheck:
mov si,offset DEST
mov cl,lenDEST
mov bh,[si]
cmp bh,5Bh ;handles the first iteration of below loop
jz CapcheckDEST
cmp bh,5Dh
jz CapcheckDEST
cmp bh,61h
jl CapcheckDEST
sub bh,20h
jmp CapcheckDEST
CapcheckDEST: ;make all characters Capital
dec cl
jz endCapcheckDEST ;check if string length is reached
inc si
mov bh,[si] ;take next letter from offset
cmp bh,5Bh ;compare to '[' if it is a square bracket skip this letter
jz CapcheckDEST
cmp bh,5Dh ;compare to ']' if it is a square bracket skip this letter
jz CapcheckDEST
cmp bh,61h 
jl CapcheckDEST ;compare the char to 61h -> 'a' if it is less than 'a' skip it and move on to the next char
sub bh,20h ;convert small letter to capital letter
mov [si],bh ;update offset
jmp CapcheckDEST ;return to start of loop
endCapcheckDEST:
call SourceAddressStringParser
call DestinationAddressStringParser
endprog:hlt
main endp

SourceAddressStringParser proc near
push dx
push cx
push bx
push ax
cmp SRC,'[' ;check if SRC is an direct or an indirect memory
jnz ISregSRC
cmp SRC+2,']' ;check if direct memory ---> direct memory is defined as [x] where x is on range of 0 to F
jnz IsInDirectMemorySRC
mov cx,0
MemoryAssignmentSRC:
mov cl,SRC+1
cmp cl,29h
jle AddressErrorSRC
cmp cl,39h
jg StartfromAMemory
sub cl,30h
jmp Player1MemorySRC 
StartfromAMemory:
cmp cl,40h
jle AddressErrorSRC
cmp cl,47h
jge AddressErrorSRC
sub cl,37h
Player1MemorySRC:
cmp WhichPlayer,1
jnz Player2DirectMemorySRC
mov si,offset mem2
add si,cx
cmp AddressTypeSRC,0
jnz endAddressCheckerSRC
mov AddressTypeSRC,3
jmp endAddressCheckerSRC
Player2DirectMemorySRC:
mov si,offset mem1
add si,cx
cmp AddressTypeSRC,0
jnz endAddressCheckerSRC
mov AddressTypeSRC,3
jmp endAddressCheckerSRC
IsInDirectMemorySRC: ;Use address inside each of those variables as offset to be delivered by si
cmp SRC+1,'B'
jnz IsSISRC
cmp SRC+2,'X'
jnz AddressErrorSRC
cmp WhichPlayer,1
jnz Player2BXIndirectSRC
mov cl,bl2+1
mov SRC+1,cl
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
Player2BXIndirectSRC:
mov cl,bl1+1
mov SRC+1,cl
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
IsSISRC:
cmp SRC+1,'S'
jnz IsDISRC
cmp SRC+2,'I'
jnz AddressErrorSRC
cmp WhichPlayer,1
jnz Player2SIIndirectSRC
mov cl,si2+3
mov SRC+1,cl
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
Player2SIIndirectSRC:
mov cl,si1+3
mov SRC+1,cl
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
IsDISRC:
cmp SRC+1,'D'
jnz AddressErrorSRC
cmp SRC+2,'I'
jnz AddressErrorSRC
cmp WhichPlayer,1
jnz Player2DIIndirectSRC
mov cl,di2+3
mov SRC+1,cl
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
Player2DIIndirectSRC:
mov cl,di1+3
mov SRC+1,cl
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
ISregSRC:         ;SRC is a register
cmp SRC,'A'
jnz IfitisBSRC
cmp SRC+1,'X' ;check if it is Register AX
jnz IsItAHSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2AXSRC
mov si,offset ax2 ;put offset of variable ax2 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2AXSRC: ;player 2 is writing command
mov si,offset ax1 ;put offset of variable ax1 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItAHSRC:
cmp SRC+1,'H' ;check if it is Register AH
jnz IsItALSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2AHSRC
mov si,offset ah2 ;put offset of variable ah2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2AHSRC: ;player 2 is writing command
mov si,offset ah1 ;put offset of variable ah1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItALSRC:
cmp SRC+1,'L' ;check if it is Register AL
jnz AddressErrorSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2ALSRC
mov si,offset al2 ;put offset of variable al2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2ALSRC: ;player 2 is writing command
mov si,offset al1 ;put offset of variable al1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IfitisBSRC:
cmp SRC,'B'
jnz IfitisCSRC
cmp SRC+1,'X' ;check if it is Register BX
jnz IsItBHSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2BXSRC
mov si,offset bx2 ;put offset of variable bx2 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2BXSRC: ;player 2 is writing command
mov si,offset bx1 ;put offset of variable bx1 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItBHSRC:
cmp SRC+1,'H' ;check if it is Register BH
jnz IsItBLSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2BHSRC
mov si,offset bh2 ;put offset of variable bh2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2BHSRC: ;player 2 is writing command
mov si,offset bh1 ;put offset of variable bh1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItBLSRC:
cmp SRC+1,'L' ;check if it is Register BL
jnz AddressErrorSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2BLSRC
mov si,offset bl2 ;put offset of variable bl2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2BLSRC: ;player 2 is writing command
mov si,offset bl1 ;put offset of variable bl1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IfitisCSRC:
cmp SRC,'C'
jnz IfitisDSRC
cmp SRC+1,'X' ;check if it is Register CX
jnz IsItCHSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2CXSRC
mov si,offset cx2 ;put offset of variable cx2 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2CXSRC: ;player 2 is writing command
mov si,offset cx1 ;put offset of variable cx1 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItCHSRC:
cmp SRC+1,'H' ;check if it is Register CH
jnz IsItCLSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2CHSRC
mov si,offset ch2 ;put offset of variable ch2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2CHSRC: ;player 2 is writing command
mov si,offset ch1 ;put offset of variable ch1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItCLSRC:
cmp SRC+1,'L' ;check if it is Register CL
jnz AddressErrorSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2CLSRC
mov si,offset cl2 ;put offset of variable cl2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2CLSRC: ;player 2 is writing command
mov si,offset cl1 ;put offset of variable cl1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IfitisDSRC:
cmp SRC,'D'
jnz IsImmediateSRC
cmp SRC+1,'X' ;check if it is Register DX
jnz IsItDHSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2DXSRC
mov si,offset dx2 ;put offset of variable dx2 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2DXSRC: ;player 2 is writing command
mov si,offset dx1 ;put offset of variable dx1 into si and treat si as the source
mov MemoryTypeSRC,2 ;source is 16-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItDHSRC:
cmp SRC+1,'H' ;check if it is Register DH
jnz IsItDLSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2DHSRC
mov si,offset dh2 ;put offset of variable dh2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2DHSRC: ;player 2 is writing command
mov si,offset dh1 ;put offset of variable dh1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsItDLSRC:
cmp SRC+1,'L' ;check if it is Register DL
jnz IsImmediateSRC
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2DLSRC
mov si,offset dl2 ;put offset of variable dl2 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
Player2DLSRC: ;player 2 is writing command
mov si,offset dl1 ;put offset of variable dl1 into si and treat si as the source
mov MemoryTypeSRC,1 ;source is 8-bits
mov AddressTypeSRC,2
jmp endAddressCheckerSRC ;move to end of program
IsImmediateSRC: ;check if source is an immediate value
mov cx,0
mov cl,lenSRC   ;use length of Source to loop on it
    mov si,offset SRC
     StringToHexSRC: ;convert String to Hex
     mov al,[si]
     cmp al,29h
     jle AddressErrorSRC
     cmp al,39h
     jg StartfromAImmediate
     sub al,30h
     mov [si],al
     jmp IncrementStringToHexSRC
     StartfromAImmediate:
     cmp al,40h
     jle AddressErrorSRC
     cmp al,47h
     jge AddressErrorSRC
     sub al,37h
     mov [si],al
     IncrementStringToHexSRC: 
     inc si
     loop StringToHexSRC
     mov si,offset SRC ;move Source to immediate value
     mov ax,0
     mov bx,1000h      ;add sepaereate array values on each other to produce full number (1,0,4,5----->1045)
     mov al,[si]
     mul bx
     add cx,ax
     inc si
     mov ax,0
     mov bx,100h
     mov al,[si]
     mul bx
     add cx,ax
     inc si
     mov ax,0
     mov bx,10h
     mov al,[si]
     mul bx
     add cx,ax
     inc si
     mov ax,0
     mov bx,01h
     mov al,[si]
     mul bx
     add cx,ax
     cmp ch,0h
     jnz SixteenbitImmediateSRC
     mov MemoryTypeSRC,1 ;Immediate value is 8 bits
     jmp Player1ImmediateSRC
     SixteenbitImmediateSRC:
     mov MemoryTypeSRC,2 ;immeduate value is 16 bits
     Player1ImmediateSRC:
     cmp WhichPlayer,1 ;assign Immediate value to immediate 2 and send its offset with si
     jnz Player2ImmediateSRC
     mov ImmediateValue2,cx
     mov si,offset ImmediateValue2
     mov AddressTypeSRC,1
     jmp endAddressCheckerSRC
     Player2ImmediateSRC:
     mov ImmediateValue1,cx ;assign Immediate value to immediate 1 and send its offset with si
     mov si,offset ImmediateValue1
     mov AddressTypeSRC,1
     jmp endAddressCheckerSRC
AddressErrorSRC:
cmp WhichPlayer,1 ;check who made the error and -1 his/her points
jnz Player2errSRC
sub Points1,1
mov AddressTypeSRC,0
jmp endAddressCheckerSRC
Player2errSRC:
sub Points2,1
mov AddressTypeSRC,0
endAddressCheckerSRC:
pop ax
pop bx
pop cx
pop dx
ret
SourceAddressStringParser endp 


DestinationAddressStringParser proc near
push dx
push cx
push bx
push ax
cmp DEST,'[' ;check if DEST is an direct or an indirect memory
jnz ISregDEST
cmp DEST+2,']' ;check if direct memory ---> direct memory is defined as [x] where x is on range of 0 to F
jnz IsInDirectMemoryDEST
mov cx,0
MemoryAssignmentDEST:
mov cl,DEST+1
cmp cl,29h
jle AddressErrorDEST
cmp cl,39h
jg StartfromAMemoryDEST
sub cl,30h
jmp Player1MemoryDEST 
StartfromAMemoryDEST:
cmp cl,40h
jle AddressErrorDEST
cmp cl,47h
jge AddressErrorDEST
sub cl,37h
Player1MemoryDEST:
cmp WhichPlayer,1
jnz Player2DirectMemoryDEST
mov di,offset mem2
add di,cx
cmp AddressTypeDEST,0
jnz endAddressCheckerDEST
mov AddressTypeDEST,3
jmp endAddressCheckerDEST
Player2DirectMemoryDEST:
mov di,offset mem1
add di,cx
cmp AddressTypeDEST,0
jnz endAddressCheckerDEST
mov AddressTypeDEST,3
jmp endAddressCheckerDEST
IsInDirectMemoryDEST: ;Use address inside each of those variables as offset to be delivered by si
cmp DEST+1,'B'
jnz IsSIDEST
cmp DEST+2,'X'
jnz AddressErrorDEST
cmp WhichPlayer,1
jnz Player2BXIndirectDEST
mov cl,bl2+1
mov DEST+1,cl
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
Player2BXIndirectDEST:
mov cl,bl1+1
mov DEST+1,cl
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
IsSIDEST:
cmp DEST+1,'S'
jnz IsDIDEST
cmp DEST+2,'I'
jnz AddressErrorDEST
cmp WhichPlayer,1
jnz Player2SIIndirectDEST
mov cl,si2+3
mov DEST+1,cl
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
Player2SIIndirectDEST:
mov cl,si1+3
mov DEST+1,cl
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
IsDIDEST:
cmp DEST+1,'D'
jnz AddressErrorDEST
cmp DEST+2,'I'
jnz AddressErrorDEST
cmp WhichPlayer,1
jnz Player2DIIndirectDEST
mov cl,di2+3
mov DEST+1,cl
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
Player2DIIndirectDEST:
mov cl,di1+3
mov DEST+1,cl
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
ISregDEST:         ;Dest is a register
cmp DEST,'A'
jnz IfitisBDEST
cmp DEST+1,'X' ;check if it is Register AX
jnz IsItAHDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2AXDEST
mov di,offset ax2 ;put offset of variable ax2 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2AXDEST: ;player 2 is writing command
mov di,offset ax1 ;put offset of variable ax1 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItAHDEST:
cmp DEST+1,'H' ;check if it is Register AH
jnz IsItALDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2AHDEST
mov di,offset ah2 ;put offset of variable ah2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2AHDEST: ;player 2 is writing command
mov di,offset ah1 ;put offset of variable ah1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItALDEST:
cmp DEST+1,'L' ;check if it is Register AL
jnz AddressErrorDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2ALDEST
mov di,offset al2 ;put offset of variable al2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2ALDEST: ;player 2 is writing command
mov di,offset al1 ;put offset of variable al1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IfitisBDEST:
cmp DEST,'B'
jnz IfitisCDEST
cmp DEST+1,'X' ;check if it is Register BX
jnz IsItBHDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2BXDEST
mov di,offset bx2 ;put offset of variable bx2 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2BXDEST: ;player 2 is writing command
mov di,offset bx1 ;put offset of variable bx1 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItBHDEST:
cmp DEST+1,'H' ;check if it is Register BH
jnz IsItBLDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2BHDEST
mov di,offset bh2 ;put offset of variable bh2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2BHDEST: ;player 2 is writing command
mov di,offset bh1 ;put offset of variable bh1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItBLDEST:
cmp DEST+1,'L' ;check if it is Register BL
jnz AddressErrorDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2BLDEST
mov di,offset bl2 ;put offset of variable bl2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2BLDEST: ;player 2 is writing command
mov di,offset bl1 ;put offset of variable bl1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IfitisCDEST:
cmp DEST,'C'
jnz IfitisDDEST
cmp DEST+1,'X' ;check if it is Register CX
jnz IsItCHDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2CXDEST
mov di,offset cx2 ;put offset of variable cx2 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2CXDEST: ;player 2 is writing command
mov di,offset cx1 ;put offset of variable cx1 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItCHDEST:
cmp DEST+1,'H' ;check if it is Register CH
jnz IsItCLDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2CHDEST
mov di,offset ch2 ;put offset of variable ch2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2CHDEST: ;player 2 is writing command
mov di,offset ch1 ;put offset of variable ch1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItCLDEST:
cmp DEST+1,'L' ;check if it is Register CL
jnz AddressErrorDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2CLDEST
mov di,offset cl2 ;put offset of variable cl2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2CLDEST: ;player 2 is writing command
mov di,offset cl1 ;put offset of variable cl1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IfitisDDEST:
cmp DEST,'D'
jnz AddressErrorDEST
cmp DEST+1,'X' ;check if it is Register DX
jnz IsItDHDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2DXDEST
mov di,offset dx2 ;put offset of variable dx2 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2DXDEST: ;player 2 is writing command
mov di,offset dx1 ;put offset of variable dx1 into di and treat di as the source
mov MemoryTypeDEST,2 ;source is 16-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItDHDEST:
cmp DEST+1,'H' ;check if it is Register DH
jnz IsItDLDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2DHDEST
mov di,offset dh2 ;put offset of variable dh2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2DHDEST: ;player 2 is writing command
mov di,offset dh1 ;put offset of variable dh1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
IsItDLDEST:
cmp DEST+1,'L' ;check if it is Register DL
jnz AddressErrorDEST
cmp WhichPlayer,1 ;check if player1 is entering this command
jnz Player2DLDEST
mov di,offset dl2 ;put offset of variable dl2 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program
Player2DLDEST: ;player 2 is writing command
mov di,offset dl1 ;put offset of variable dl1 into di and treat di as the source
mov MemoryTypeDEST,1 ;source is 8-bits
mov AddressTypeDEST,2
jmp endAddressCheckerDEST ;move to end of program

AddressErrorDEST:
cmp WhichPlayer,1 ;check who made the error and -1 his/her points
jnz Player2errDEST
sub Points1,1
mov AddressTypeDEST,0
jmp endAddressCheckerDEST
Player2errDEST:
sub Points2,1
mov AddressTypeDEST,0
endAddressCheckerDEST:
pop ax
pop bx
pop cx
pop dx
ret
DestinationAddressStringParser endp

AddressingmodeDesignator proc near

cmp AddressTypeDEST,2
jnz DestIsMemory
cmp AddressTypeSRC,1
jnz SRCIsRegisterwithDestRegister
mov AddressingMode,1
jmp endAddressingMode
SRCIsRegisterwithDestRegister:
cmp AddressTypeSRC,2
jnz SRCIsMemorywithDestRegister
mov AddressingMode,2
jmp endAddressingMode
SRCIsMemorywithDestRegister:
cmp AddressTypeSRC,3
jnz SRCisIndirectMemorywithDestRegister
mov AddressingMode,3
jmp endAddressingMode
SRCisIndirectMemorywithDestRegister:
cmp AddressTypeSRC,4
jnz endAddressingMode ;error mn abl el function deeh msh lazem nen2so marteen
mov AddressingMode,4
jmp endAddressingMode
DestIsMemory:
cmp AddressTypeDEST,3
jnz DestIsIndirectMemory
cmp AddressTypeSRC,1
jnz SRCIsRegisterwithDestMemory
mov AddressingMode,3
jmp endAddressingMode
SRCIsRegisterwithDestMemory:
cmp AddressTypeSRC,2
jnz SRCIsMemorywithDestMemory
mov AddressingMode,3
jmp endAddressingMode
SRCIsMemorywithDestMemory:
cmp AddressTypeSRC,3
jnz SRCisIndirectMemorywithDestMemory
mov AddressingMode,0
jmp AddressModeError
SRCisIndirectMemorywithDestMemory:
cmp AddressTypeSRC,4
jnz endAddressingMode ;error mn abl el function deeh msh lazem nen2so marteen
mov AddressingMode,0
jmp AddressModeError
DestIsIndirectMemory:
cmp AddressTypeDEST,4
jnz endAddressingMode
cmp AddressTypeSRC,1
jnz SRCIsRegisterwithDestInMemory
mov AddressingMode,4
jmp endAddressingMode
SRCIsRegisterwithDestInMemory:
cmp AddressTypeSRC,2
jnz SRCIsMemorywithDestInMemory
mov AddressingMode,4
jmp endAddressingMode
SRCIsMemorywithDestInMemory:
cmp AddressTypeSRC,3
jnz SRCisIndirectMemorywithDestInMemory
mov AddressingMode,0
jmp AddressModeError
SRCisIndirectMemorywithDestInMemory:
mov AddressingMode,0
jmp AddressModeError


AddressModeError:
cmp WhichPlayer,1 ;check who made the error and -1 his/her points
jnz Player2errMode
sub Points1,1
jmp endAddressingMode
Player2errMode:
sub Points2,1
endAddressingMode:
ret
AddressingmodeDesignator endp
end main