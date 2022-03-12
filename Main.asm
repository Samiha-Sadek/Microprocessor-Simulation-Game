.286
.386
include Erase.inc
include ShowR.inc
include WChar.inc
include WName.inc
include WScore.inc
include WMemory.inc
include Str_Hex.inc
include Hex_Str.inc
include BG.inc
include UCL.inc
include DMem.inc
include WReg.inc
include WByte.inc
include WWord.inc
include RByte.inc
include RWord.inc
include WMWord.inc
include RMWord.inc
include ShowCirc.inc
include ShwTrg.inc
include ShwRct.inc
include WBalls.inc
include Scr_Dec.inc
include Scr_Str.inc
include NoOp.inc
include OneOp.inc
include TwoOpF.inc
include TwoOpS.inc
include Regzer.inc
include Regloss.inc
include ChatInv.inc
include GameInv.inc
include ShowL.inc

.model Large
.stack
.data
xxxx db 0
db '0000'
operand2 db 15 DUP('$')
SRCend db '$' ;source terminator
lenSRC db ?   ;source length
db '0000'
operand1 db 15 DUP('$')
DESTend db '$' ;destination terminator
lenDEST db ?   ;destination length
AddressTypeSRC db 0 ; 1: immediate, 2: Register, 3: Direct, 4: Register Indirect
AddressTypeDEST db 0 ; 1: immediate, 2: Register, 3: Direct, 4: Register Indirect
MemoryTypeSRC db ? ; 1: 8-bits 2: 16-Bits
MemoryTypeDEST db ? ; 1: 8-bits 2: 16-Bits
AddressingMode db ? ; 1: immediate, 2: Register, 3: Direct, 4: Register Indirect

SourceOffset dw ?
DestinationOffset dw ?


ImmediateValue1 dw ?

ImmediateValue2 dw ?

ImmediateValueStr db '$$$$', '$'

Target db '105E' ,'$'
Level db 1
player db 1          ;1 for player 1 and 2 for player 2
DummyPlayerNum db 1
buffer db 16,?
command db 17 dup('$')
CommandList db 'ADD','ADC','SUB','SBB','DIV','MUL','MOV','NOP','INC','DEC','XOR','AND','SHR','SHL','CLC','ROR'
ORCommandstr db 'OR'
spaces db '                ', '$'
opcode db 5 dup('$')


spaces2 db '                                                                                                             ', '$'
dummyyyyy db 0
dummyyyyy2 db 0


dummy dw ?

CommandNo db 1
disp db 41h
disp1 db 0
disp2 db 1
;;;;; Player Names:
buffer2 db 16,?
Name1 db 16 dup('$')
buffer3 db 16,?
Name2 db 16 dup('$')
;;;;; Player Scores:
buffer4 db 4,?
Score1 db 4 dup('$')
buffer5 db 4,?
Score2 db 4 dup('$')
Score1InDec dw ?
Score2InDec dw ?

HexNumbers db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F', '$'
operandsize db 1
SourceMem db 0              ;1 = Source is memory, 0 = source is register
DestMem db 0              ;1 = Dest is memory, 0 = Dest is register
carry1 db 0
carry2 db 0
carry db 0          ;Intermediate carry for both MP
db '000'
VarHexstr1 db '$$$$','$'
db '000'
VarHexstr2 db '$$$$','$'
db '000'
VarHexstr3 db '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$','$'

VarHexNum dw ?
VarHexNum1 dw ?
VarHexNum2 dw ?

;;;;;;;;;;;;;;;;;;;;Promts
Promt1 db 'Enter Your Name: ', '$'
Promt2 db 'Enter Initial Points: ', '$'
Promt3 db 'Press F1 to start chatting ', '$'
Promt4 db 'Press F2 to start the game', '$'
Promt5 db 'Press ESC to Exit', '$'
Promt6 db 'Game Invitation', '$'
Promt7 db 'Chat Invitation', '$'
Promt8 db 'Level', '$'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
col db 0
column1 db 0 
row1 db 13
row2 db 1
messageoffset dw ?   
message1 db 42 dup('$')
dummystr db 42 dup('$')    
displacment1 dw 0
displacment2 dw 0
pagenumber db 0
inputnumber db 0
Inputsize db 0  
SentName db 16 dup('$')
ReceivedName db 16 dup('$')
Sending db 0
Receiving db 0
EscapePressed db 0
F1PressedSender db 0
F1PressedReceiver db 0
F2PressedSender db 0
F2PressedReceiver db 0
ChatModuleStart db 0    
GameModuleStart db 0
ISentTheInvitation db 0
CurrentPlayer db ?
RegisterOffset dw ?
counter dw 0
RegisterDisp dw 0
SentPoints db 4 dup('$')
REceivedPoints db 4 dup('$')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; For Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
msg db 'Incorrect','$'      ;;Temporary
HexNum db 'ABCD','$'           ;;Temporary
HexNum2 db '54','$'            ;;Temporary
HexNum3 db '122','$'          ;;Temporary
HexNum4 db '$$$','$'            ;;Temporary
x dw 50
y dw 60
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

col1 db 16, 17
col2 db 36, 37
reg1 db 2,3,4,5
reg2 db 10,11,12,13
reg3 db 22,23,24,25
reg4 db 30,31,32,33

balls1 db 3,5,7,9,11
balls2 db 23,25,27,29,31

;;player1 balls
Ob1 db 0
WGb1 db 0
DGb1 db 0
Bb1  db 0
Gb1 db 0
;;player2 balls
Ob2 db 0
WGb2 db 0
DGb2 db 0
Bb2  db 0
Gb2 db 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Processor1 data
ax1 label byte
ah1 db 30h,30h
al1 db 30h,30h

bx1 label byte
bh1 db 30h,30h
bl1 db 30h,30h

cx1 label byte
ch1 db 30h,30h
cl1 db 30h,30h

dx1 label byte
dh1 db 30h,30h
dl1 db 30h,30h

si1 db 30h,30h,30h,30h
di1 db 30h,30h,30h,30h
sp1 db 30h,30h,30h,30h
bp1 db 30h,30h,30h,30h

mem1 db 32 DUP(30h), 0


;Processor2 data
ax2 label byte
ah2 db 30h,30h
al2 db 30h,30h

bx2 label byte
bh2 db 30h,30h
bl2 db 30h,30h

cx2 label byte
ch2 db 30h,30h
cl2 db 30h,30h

dx2 label byte
dh2 db 30h,30h
dl2 db 30h,30h

; si2 db 30h,30h,30h,30h
si2 db 30h,30h,30h,30h
di2 db 30h,30h,30h,30h
sp2 db 30h,30h,30h,30h
bp2 db 30h,30h,30h,30h

mem2 db 32 DUP(30h), 0

modee db 3     ; 1 for chatting, 2 for minigame , 3 for playing the game

;;;;;;;;;;;;vars to check if a powerup has been used before ;;;;;;;;;;;;;;;
player1PU1 db 0
player1PU2 db 0
player1PU3 db 0
player1PU4 db 0
player1PU15 db 0

player1PU5 db 0

player2PU1 db 0
player2PU2 db 0
player2PU3 db 0
player2PU4 db 0
player2PU15 db 0


player2PU5 db 0


ExecutePowerUp1 db 0    ;0 = Do not execute, 1 = Execute
ExecutePowerUp2 db 0    ;0 = Do not execute, 1 = Execute
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;minigame
p1position dw 80, 170

p2position dw 236, 170


bulltes1 dw  20 dup (0)
bulletsno1 dw 0

bulltes2 dw  20 dup (0)
bulletsno2 dw 0

shapeC dw 20,40
shexist dw 1
ShpClr dw 6    ;can be either 6,7,8,9,10
Trnsnmbr dw 0    ;Current nubmer of turns

TrnsBMG  dw 2    ;turns before mingame ;ranges form 2 to 12  ;default is 1

valid2Chk db 1      ; 0 = Invalid, 1 = Valid. if it's invalid, skip the rest of the checks

bufferL db 3,?
LevelStr db 3 dup('$')

PromtL db 'Enter the Level of the Game:   ', '$'







ppnn1   db 'Player1:','$'
ppnn2   db 'Player2:','$'

Promtvalax db 'Enter the Value of Ax:   ', '$'
Promtvalbx db 'Enter the Value of Bx:   ', '$'
Promtvalcx db 'Enter the Value of Cx:   ', '$'
Promtvaldx db 'Enter the Value of Dx:   ', '$'
Promtvalsi db 'Enter the Value of SI:   ', '$'
Promtvaldi db 'Enter the Value of DI:   ', '$'
Promtvalbp db 'Enter the Value of SP:   ', '$'
Promtvalsp db 'Enter the Value of BP:   ', '$'


PromtvalMP db 'Choose which microprocessor:   ', '$'


bufferrregs db 6,?
rreggs db 6 dup('$')




buffermmp db 6,?
mmmp db 6 dup('$')

PromtF db 'Enter Forbidden key:   ', '$'

bufferF1 db 16,?
ForbiddenChar1 db '$$'       ; A variable to store the forbidden char
bufferF2 db 16,?
ForbiddenChar2 db '$$'       ; A variable to store the forbidden char




PromtLs1 db 'Player 1 Lost ', '$'
PromtLs2 db 'Player 2 Lost', '$'

;THE VALUE WHICH MAKES THE PLAYER LOSE 
Promttarg db 'Enter New target:   ', '$'

buffertarg db 16,?
dummytargetlol db '105E' ,'$'
targetlol db '105E' ,'$'
; 8albn el target lazem yefdal 4 7roof, eb2a garab kda

valTarLol db 0      ; Used to make sure the target isn't already in a register
lose1 db 0
lose2 db 0

pplayer db 0   ;placeholder for player while choosing the microprocessor




hasrc  db 0    ;0 for not rec, 1 for rec

chr db '$'


commandcol1 db 1
commandcol2 db 22



SentForbChar db 2 dup('$')
ReceivedForbChar db 2 dup('$')


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.code




RecClr proc far

pusha


Reccll: ;Recieve the clr
    
    ;Check that Data is Ready
    mov dx , 3FDH		; Line Status Register
    in al , dx 
    test al , 1
    jz Reccll                                 ;Not Ready
 ;If Ready read the VALUE in Receive data register
      	mov dx , 03F8H
        in al , dx
        
        cmp al, 6      ; it will be either 1 or 2
        je ReccllEnd
        cmp al,7
        je ReccllEnd

        cmp al, 8     
        je ReccllEnd
        cmp al,9
        je ReccllEnd

        cmp al, 10     
        je ReccllEnd
        

        jmp Reccll  
ReccllEnd:
    mov ah,0
    mov word ptr  ShpClr ,ax

popa
ret

RecClr ENDP


;sends whatever is in bl
;The proc waits for the other side to be free before sending
;we might need a version that doesn't keep checking 
sendbayte proc far
    pusha


    mov dx , 3FDH		; Line Status Register
    AGAINsnd:  In al , dx 			;Read Line Status
            test al , 00100000b
            JZ AGAINsnd                               ;Not empty

    ;If empty put the VALUE in Transmit data register
            mov dx , 3F8H		; Transmit data register
            ;7ot el value beta3 el byte ely 3ayez teb3atha fil al 
            mov al,bl
            out dx , al


    popa
    ret

sendbayte ENDP


;recieves in a variable called chr
; if not recieved, hasrc will be 0
recbayte proc far
    pusha

    mov dx , 3FDH		; Line Status Register

    CHKrr:    in al , dx 
            test al , 1
            JZ skrec                                   ;Not Ready
    ;If Ready read the VALUE in Receive data register
            mov hasrc,1
            mov dx , 03F8H
            in al , dx
            mov chr,al
            jmp endrc
    skrec:
        mov hasrc,0 
    endrc:

    popa
ret

recbayte ENDP


rndfreq proc
;randomize the freq of shooter
push ax
push cx
mov ah,00h
int 1Ah     ; CX:DX now hold number of clock ticks since midnight

mov ax,dx
mov cx,11

mov dx,0
div cx      ;dx contains the remainder

pop ax
pop cx

push bx
push di

lea bx, TrnsBMG
mov di,[bx]   ;di has the value
add di, dx
mov [bx],di
pop bx
pop di

ret

rndfreq ENDP

axchk proc far

cmp ax,0
jl axisneg
jnl axispos

axisneg:
mov bx,-1

push dx
mul bx
pop dx

axispos:
ret

axchk ENDP

wtFrVR proc far
;wait for VR
pusha
mov dx, 3dah

 ;Wait for bit 3 to be zero (not in VR).
 ;We want to detect a 0->1 transition.
waitForEnd:
 in al, dx
 test al, 08h
jnz waitForEnd

 ;Wait for bit 3 to be one (in VR)
waitForNew:
 in al, dx
 test al, 08h
jz waitForNew


popa
ret

wtFrVR ENDP

rndclr proc  far
;randomize the colour of the object
pusha
mov ah,00h
int 1Ah     ; CX:DX now hold number of clock ticks since midnight

mov ax,dx
mov cx,5

mov dx,0
div cx      ;dx contains the remainder



lea bx, ShpClr
mov di,[bx]   ;di has the value
add di, dx
mov [bx],di


mov al, CurrentPlayer
cmp player, al
jne Receiveclr


sndclr:
mov bx, word ptr ShpClr
call sendbayte
jmp rcend


Receiveclr:
call RecClr  ; will keep looping 



rcend:


popa

ret

rndclr ENDP

FinBul1 proc far
;move bullets1
pusha


lea bx, bulltes1   ;for comparison
sub bx,2   ;just for checking

; mov cx ,ax

lea cx, bulltes1
add cx,40


; loop over all the bullets and move them too
sub cx,2  ;because we need the y of the bullet not the x of next bullet

finbults1:


mov di,cx
mov di, [di]


sub cx,2

mov si, cx
mov si, [si]



;moving the bullet

ShowRect si,di,2,2,0   ;show black square

mov si,0


push bx
mov bx,cx
add bx,2
mov [bx],si
sub bx,2
mov [bx],si
pop bx





sub cx,2



cmp cx,bx
jnz finbults1

mov bulletsno1,0
popa

ret



FinBul1 ENDP

FinBul2 proc far
;move bullets2
pusha


lea bx, bulltes2   ;for comparison
sub bx,2   ;just for checking

; mov cx ,ax

lea cx, bulltes2
add cx,40


; loop over all the bullets and move them too
sub cx,2  ;because we need the y of the bullet not the x of next bullet

finbults2:


mov di,cx
mov di, [di]


sub cx,2

mov si, cx
mov si, [si]



;moving the bullet

ShowRect si,di,2,2,0   ;show black square

mov si,0


push bx
mov bx,cx
add bx,2
mov [bx],si
sub bx,2
mov [bx],si
pop bx





sub cx,2



cmp cx,bx
jnz finbults2

mov bulletsno2,0
popa

ret



FinBul2 ENDP



chkCol1 proc far
;check colided
pusha
;di,si are the bullets coordinates

lea bx, shexist
mov ax, 0

mov bx,[bx]
cmp ax,bx

; jz far ptr skchk1
jnz sj1

jmp  skchk1

sj1:  ;skip jump1


mov ax,si
mov bx,di

lea di, shapeC
mov si, [di]
add di,2
mov di, [di]

;check on y
add di,5

cmp bx,di

; jg skchk1   ;jump greater
jbe sj2
jmp skchk1
sj2:

sub di,10

cmp bx, di

; jb skchk1    ;jump below
jge sj3
jmp skchk1
sj3:


add di,5   ;back to original value

;check on x
add si,5

cmp ax,si

; jg skchk1
jbe sj4
jmp skchk1
sj4:

sub si,10

cmp ax, si


; jb skchk1    ;jump below
jge sj5
jmp skchk1
sj5:

;;;;;;;;;;;;;;;;;;;;;;;;;Collided;;;;;;;

add si,5 ;back to original value

ShowCirc si,di,5,0   ;draw a black circle

;for now

add si,158

ShowCirc si,di,5,0   ;draw a black circle

;now we set the shape to not exist
call FinBul1
call FinBul2

lea di, shexist
mov si,0
mov [di],si


;send to the other side that the player1 collided

push bx
mov bl,10  ; 10 is for p1 winning the minigame
call sendbayte
pop bx


mov si, ShpClr
sub si,5
add Score1InDec,si
ScoreToString Score1, Score1InDec


add si,5
;update the number of balls

;6,7,8,9,10

cmp si,6
jz addOr1

cmp si,7
jz addWG1

cmp si,8
jz addDG1

cmp si,9
jz addB1

jmp addG1



addOr1:
add Ob1,1
jmp skAB

addWG1:
add WGb1,1
jmp skAB

addDG1:
add DGb1,1
jmp skAB

addB1:
add Bb1,1
jmp skAB

addG1:
add Gb1,1


skAB:

mov ShpClr,6
mov modee,3    ;back to playing the main game


mov TrnsBMG, 2    ;default number
mov Trnsnmbr,0

CALL rndfreq    ;pick a random number of turns before starting the minigame


skchk1:   ;skip check





popa

ret

chkCol1 ENDP


chkCol2 proc far


pusha
;di,si are the bullets coordinates

lea bx, shexist
mov ax, 0

mov bx,[bx]
cmp ax,bx


; jz skchk2
jnz skj6
jmp skchk2
skj6:

mov ax,si
mov bx,di

lea di, shapeC
mov si, [di]
add di,2
mov di, [di]

;check on y
add di,5

cmp bx,di

; jg skchk2
jbe skj7
jmp skchk2
skj7:

sub di,10

cmp bx, di

; jb skchk2    ;jump below
jge skj8
jmp skchk2
skj8:


add di,5   ;back to original value

;check on x
add si,5

add si, 158 ; for player 2 side


cmp ax,si

; jg skchk2
jbe skj9
jmp skchk2
skj9:


sub si,10

cmp ax, si

; jb skchk2    ;jump below
jge skjA
jmp skchk2
skjA:


;;;;;;;;;;;;;;;;;;;;;;;;;;;Collided
add si,5 ;back to original value

ShowCirc si,di,5,0   ;draw a black circle

;for now

sub si,158

ShowCirc si,di,5,0   ;draw a black circle

;we need to finish the bullets
call FinBul1
call FinBul2

;now we set the shape to not exist

lea di, shexist
mov si,0
mov [di],si


;send to the other side that the player2 collided

push bx
mov bl,11  ; 11 is for p2 winning the minigame
call sendbayte
pop bx


mov si, ShpClr
sub si,5
add Score2InDec,si
ScoreToString Score2, Score2InDec

add si,5
;update the number of balls

;6,7,8,9,10

cmp si,6
jz addOr2

cmp si,7
jz addWG2

cmp si,8
jz addDG2

cmp si,9
jz addB2

jmp addG2





addOr2:
add Ob2,1
jmp skAB1

addWG2:
add WGb2,1
jmp skAB1

addDG2:
add DGb2,1
jmp skAB1

addB2:
add Bb2,1
jmp skAB1

addG2:
add Gb2,1



skAB1:

mov ShpClr,6
mov modee,3    ;back to playing the main game

mov TrnsBMG, 2   ;default number
mov Trnsnmbr,0



call rndfreq    ;pick a random number of turns before starting the minigame

skchk2:   ;skip check

popa

ret

chkCol2 ENDP





mo proc far
;move object
;EXTRN shapeC
;EXTRN shexist

pusha

lea bx, shexist
mov bx, [bx]
AND bx,1

; jz skmo ;skip move object
jnz skj4
jmp skmo
skj4:

lea di, shapeC
mov si, [di]
add di,2
mov di, [di]

call wtFrVR
ShowCirc  si,di,4,0 ;draw a black circle

add si, 158   ; to move to players 2 side

ShowCirc  si,di,4,0 ;draw a black circle

sub si, 158   ; to move to players 2 side



;hena momken ah w momken man3ml4 randomization

;for now it'll be moving in the x direction

;if at the beginning of the screen

mov ax, 6
cmp ax,si

; jnc rsi
jc skj5
jmp rsi
skj5:


skr:  ;skipreset

sub si,1


ShowCirc  si,di,4,ShpClr

add si,158
ShowCirc  si,di,4,ShpClr
sub si,158



mov ax,si

lea di, shapeC
mov [di],ax







skmo:


popa
ret

rsi:   ;reset si

; mov si,154   ; =158-4
mov si,115   ; =158-4

jmp skr

mo ENDP

spwbult1 proc far
;p1 shooting

;check on bullet1 and 2
push di
lea di, bulletsno1
mov bl , [di]

pop di

mov bh , 9      ;it starts counting from zero up there

cmp bl,bh   ; 10 is the max number of bullets

; jz movee
jnz skj1
jmp movee
skj1:


push di

lea di, p1position
mov si,[di]
add di,2
mov di,[di]

sub di,7    ;=5 +2

ShowRect si,di,2,2,6

;increase bullet number
push di
lea di, bulletsno1

mov bx,[di]
add bx,1
mov [di],bx
pop di
;back to the y value

;update the position
pusha
dec bx ;back to the old number of bullets
mov ax,bx
mov bx,4     ; *2 for x,y  and *2 for words instead of bytes
mul bx

lea bx, bulltes1
add bx,ax
add bx,2 ;update the y value first
mov [bx],di
sub bx,2
mov [bx],si

popa

pop di ;back to the original di



ret

spwbult1 ENDP

spwbult2 proc far
;p2 shooting

;check on bullet1 and 2
push di
lea di, bulletsno2
mov bl , [di]

pop di

mov bh , 9      ;it starts counting from zero up there

cmp bl,bh   ; 10 is the max number of bullets

; jz movee
jnz skmj     ;skip move jump 34an bas ele bys2l
jmp movee
skmj:


push di

lea di, p2position
mov si,[di]
add di,2
mov di,[di]

sub di,7    ;=5 +2

ShowRect si,di,2,2,7

;increase bullet number
push di
lea di, bulletsno2

mov bx,[di]
add bx,1
mov [di],bx
pop di
;back to the y value

;update the position
pusha
dec bx ;back to the old number of bullets
mov ax,bx
mov bx,4     ; *2 for x,y  and *2 for words instead of bytes
mul bx

lea bx, bulltes2
add bx,ax
add bx,2 ;update the y value first
mov [bx],di
sub bx,2
mov [bx],si

popa

pop di ;back to the original di



ret

spwbult2 ENDP

mb1 proc
;move bullets1

pusha


lea bx, bulltes1   ;for comparison
sub bx,2   ;just for checking

; mov cx ,ax

lea cx, bulltes1
add cx,40


; loop over all the bullets and move them too
sub cx,2  ;because we need the y of the bullet not the x of next bullet

movebults1:


mov di,cx
mov di, [di]


sub cx,2

mov si, cx
mov si, [si]

cmp si,0      ;ASSUMPTION: a bullet can only have 0 x value if it doesn't exist
jz skipmb1
;check if collided too
call chkCol1

;;;;;;;;;;move up
cmp di,0   ;YOU MIGHT WANNA CAHNGE THIS

jz endbullets1


;moving the bullet

ShowRect si,di,2,2,0   ;show black square


sub di,1


ShowRect si,di,2,2,6 ;move the bullet
;;;;;;;;;;;;;;;;;;;;;;;;;


push bx
mov bx,cx
add bx,2
mov [bx],di
pop bx





skipmb1:
sub cx,2    ;here in addition to that of the loop's so it all adds up to 2



cmp cx,bx
jnz movebults1


skipbul1:


popa



ret

endbullets1:
ShowRect si,di,2,2 0   ;delete the bullet

push bx
lea bx, bulletsno1
mov ax,[bx]
dec ax
mov [bx],ax

mov si,cx

mov bx,0  ;use bx one more time
mov [si] , bx   ;make the x value zero for later check



pop bx
;check if it's the last bullet
;skip move bullet

cmp ax,0

jz skipbul1

jmp skipmb1



mb1 ENDP

mb2 proc
;move bullets2

pusha


lea bx, bulltes2   ;for comparison
sub bx,2   ;just for checking

; mov cx ,ax

lea cx, bulltes2
add cx,40


; loop over all the bullets and move them too
sub cx,2  ;because we need the y of the bullet not the x of next bullet

movebults2:


mov di,cx
mov di, [di]


sub cx,2

mov si, cx
mov si, [si]

cmp si,0      ;ASSUMPTION: a bullet can only have 0 x value if it doesn't exist
jz skipmb2
;check if collided too
call chkCol2

;;;;;;;;;;move up
cmp di,0   ;YOU MIGHT WANNA CAHNGE THIS

jz endbullets2


;moving the bullet

ShowRect si,di,2,2,0   ;show black square


sub di,1


ShowRect si,di,2,2,7 ;move the bullet
;;;;;;;;;;;;;;;;;;;;;;;;;


push bx
mov bx,cx
add bx,2
mov [bx],di
pop bx





skipmb2:
sub cx,2    ;here in addition to that of the loop's so it all adds up to 2



cmp cx,bx
jnz movebults2


skipbul2:


popa



ret

endbullets2:
ShowRect si,di,2,2 0   ;delete the bullet

push bx
lea bx, bulletsno2
mov ax,[bx]
dec ax
mov [bx],ax


mov bx,0
mov si,cx
mov [si],bx   ;make the x value zero for later check
;check if it's the last bullet
;skip move bullet
pop bx

cmp ax,0

jz skipbul2

jmp skipmb2



mb2 ENDP

mp1 proc far

pusha

lea di, p1position
mov si, [di]
add di,2
mov di,[di]

ShowTring si,di, 6




popa
ret

mp1 ENDP

mp2 proc far

pusha

lea di, p2position
mov si, [di]
add di,2
mov di,[di]

ShowTring si,di, 7




popa
ret

mp2 ENDP


;paste here lol


UpdateRegMem proc far
    ;;;;;;;; Write balls content
    WriteBalCont Ob1,WGb1,DGb1,Bb1,Gb1,balls1

    WriteBalCont Ob2,WGb2,DGb2,Bb2,Gb2,balls2
    ;;;;;;;; Write Registers Content
    WriteRegCont ax1, reg1
    WriteRegCont ax2, reg3
    WriteRegCont si1, reg2
    WriteRegCont si2, reg4

    ;;;;;;;; Write memory Content
    DrawMemory mem1,col1
    DrawMemory mem2, col2
    ret
UpdateRegMem endp



GetForbid proc far
    mov Sending, 0
    mov Receiving, 0
    mov displacment1, 0
    mov displacment2, 0

    mov ah, 0
    mov al,13h
    int 10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 1      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ah, 9
    mov dx, offset PromtF
    int 21h

SendForbChar:    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        jnz ReadyToSendFC
        Jmp receiveFC 

ReadyToSendFC:
     	mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jnz SaveFC
        jmp receiveFC
SaveFC:
        ;Remove char from buffer 
        mov ah, 0
        int 16h 
        ;Check if backspace, remove the letter
        cmp al, 8h
        je RemoveFC     
        ;Check if enter was pressed
        cmp al, 0Dh
        je PrintFC    ;print the whole message
        
        mov bx, displacment1
        cmp bx, 1
        jnz NotMaxlengthFC
        jmp receiveFC
        ;Else, add the char in the message         
NotMaxlengthFC:
        mov bx, displacment1
        lea si, SentForbChar
        mov [si][bx], al 
        inc displacment1 
        mov messageoffset, si 
        
        mov  dl, column1   ;Column
        mov  dh, 4   ;Row
        mov  bh, 0   ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h

        
        ;mov  al, dl
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
        
        inc column1
        
        jmp receiveFC
RemoveFC:  
        dec displacment1    
        mov bx, displacment1 
        lea si, SentForbChar
        mov byte ptr [si][bx], 24h 
        dec column1
        mov  dl, column1   ;Column
        mov  dh, 4   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h
        
        mov  al, ' '
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
                                 
        jmp receiveFC

PrintFC:  
;Check that Transmitter Holding Register is Empty     
        mov Sending, 1
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        JZ PrintFC     ;Not empty 
  		
        ;print the message
        mov di, messageoffset
        mov al, byte ptr [di]
        cmp al, 24h
        je SendingEndFC
  		
        mov dx , 3F8H        
        out dx, al 
        
        inc messageoffset
        jmp PrintPoints
SendingEndFC: 
        mov dx , 3F8H
        mov al, 24h        
        out dx, al     ;Send $ to end sending 
        mov column1, 0
        mov displacment1, 0
        jmp CheckForEndFC

receiveFC:   
        ;Check that Data is Ready
	    mov dx , 3FDH		; Line Status Register
        in al , dx 
        test al , 1
        JNZ ReadyToReceiveFC
        JMP SendForbChar                                   ;Not Ready
 ;If Ready read the VALUE in Receive data register
ReadyToReceiveFC:
      	mov dx , 03F8H
        in al , dx
        
        cmp al, 24h
        je ReceiveEndedFC

        mov bx, displacment2
        lea si, ReceivedForbChar
        mov [si][bx], al 
        inc displacment2 
        mov messageoffset, si 
        
        jmp SendForbChar  
ReceiveEndedFC:
        mov Receiving, 1
        jmp CheckForEndFC

CheckForEndFC:
        cmp Sending, 1
        je FCSent
        jmp SendForbChar
FCSent:    
        cmp Receiving, 1
        je FCReceived
        jmp receiveFC
FCReceived:
    mov displacment2, 0
    ret
GetForbid ENDP



PowerUp1 proc far
;Executing a command on your own processor (consumes 5 points)
    mov ExecutePowerUp1, 0
    cmp player,1
    jne ExecuteForPlayer2
    jmp ExecuteForPlayer1

ExecuteForPlayer2:
    cmp Score2InDec, 5
    jb DoNotExecute
    sub Score2InDec, 5
    ScoreToString Score2, Score2InDec
    mov ExecutePowerUp1, 1
    jmp DoNotExecute

ExecuteForPlayer1:
    cmp Score1InDec, 5
    jb DoNotExecute
    sub Score1InDec, 5
    ScoreToString Score1, Score1InDec
    mov ExecutePowerUp1, 1

DoNotExecute:
    ret
PowerUp1 ENDP


PowerUp2 proc far
;Executing a command on your processor and your opponent processor at the same time (consumes 3 points)
    mov ExecutePowerUp2, 0
    cmp player,1
    jne ExecuteForPlayer22
    jmp ExecuteForPlayer12

ExecuteForPlayer22:
    cmp Score2InDec, 3
    jb DoNotExecute1
    sub Score2InDec, 3
    ScoreToString Score2, Score2InDec
    mov ExecutePowerUp2, 1
    jmp DoNotExecute1

ExecuteForPlayer12:
    cmp Score1InDec, 3
    jb DoNotExecute1
    sub Score1InDec, 3
    ScoreToString Score1, Score1InDec
    mov ExecutePowerUp2, 1

DoNotExecute1:
    ret
PowerUp2 ENDP




PowerUp3 proc far
;Changing the forbidden character only once (consumes 8 points)
    cmp player,1
    jne ExecuteForPlayer23
    jmp ExecuteForPlayer13

ExecuteForPlayer23:
    cmp Score2InDec, 8
    jb DoNotExecute2
;Check Whether it was used before
    cmp player2PU3, 1
    jne CanExecute2
    jmp DoNotExecute2
CanExecute2:
;player2
    sub Score2InDec, 8
    ScoreToString Score2, Score2InDec

    Call GetForbid
    mov player2PU3,1

    jmp DoNotExecute2

ExecuteForPlayer13:
    cmp Score1InDec, 8
    jb DoNotExecute2
    ;Check Whether it was used before
    cmp player1PU3, 1
    jne CanExecute1
    jmp DoNotExecute2
CanExecute1:
    sub Score1InDec, 8
    ScoreToString Score1, Score1InDec

    Call GetForbid
    mov player1PU3,1
DoNotExecute2:
    ret
PowerUp3 ENDP



PowerUp4 proc far
;Changing the forbidden character only once (consumes 30 points)
    cmp player,1
    jne ExecuteForPlayer24
    jmp ExecuteForPlayer14

ExecuteForPlayer24:
    cmp Score2InDec, 30
    ; jb DoNotExecute3
    jge skDNE3
    jmp DoNotExecute3
    skDNE3:
;Check Whether it was used before
    cmp player2PU4, 1
    jne CanExecute3
    jmp DoNotExecute3
CanExecute3:


    sub Score2InDec, 30
    ScoreToString Score2, Score2InDec

    ;Clear All registers
    Regzero ax1
    Regzero bx1
    Regzero cx1
    Regzero dx1
    Regzero si1
    Regzero di1
    Regzero sp1
    Regzero bp1


    Regzero ax2
    Regzero bx2
    Regzero cx2
    Regzero dx2
    Regzero si2
    Regzero di2
    Regzero sp2
    Regzero bp2

    mov player2PU4,1
    jmp DoNotExecute3

ExecuteForPlayer14:
    cmp Score1InDec, 30
    ; jb DoNotExecute3
    jge SKDN3
    jmp DoNotExecute3
    SKDN3:
    ;Check Whether it was used before
    cmp player1PU4, 1
    jne CanExecute4
    jmp DoNotExecute3
CanExecute4:
    sub Score1InDec, 30
    ScoreToString Score1, Score1InDec

    ;Clear all registers
    Regzero ax1
    Regzero bx1
    Regzero cx1
    Regzero dx1
    Regzero si1
    Regzero di1
    Regzero sp1
    Regzero bp1


    Regzero ax2
    Regzero bx2
    Regzero cx2
    Regzero dx2
    Regzero si2
    Regzero di2
    Regzero sp2
    Regzero bp2

    mov player1PU4,1


DoNotExecute3:
    ret
PowerUp4 ENDP

PowerUp5 proc far
    
    pusha 

    ;Changing the target character only once (Does NOT consume points)
    cmp player,1                ; Checks if player 1 is the one using the powerup
    jne Player2usespower5       ; jumps if player 2 is the one using it
    jmp player1usespower5

Player2usespower5:
    cmp player2PU5,1            ; if it's 1, then it has already been used once
                                ; So can't use it again
    jnz Skippoplz               ; if it's not zero, then he can use it
    jmp Cantusepower5
Skippoplz:
    mov player2PU5,1            ; Sets power usage to 1, so that it can't be used again
    jmp Canusepower5

player1usespower5:
    cmp player1PU5,1            ; if it's 1, then it has already been used once
                                ; So can't use it again
    jnz Skippoplz1              ; if it's not zero, then he can use it
    jmp Cantusepower5
Skippoplz1:
    mov player1PU5,1            ; Sets power usage to 1, so that it can't be used again
    jmp Canusepower5

Canusepower5:           ; Jumps here if power can be used

    mov cl, 4           ; moving 4 chars
    mov SI, offset targetlol            ; The variable we will write in
    ; emptying the dummytarget
    emptynextolettaro:
    mov ch,36           ; ASCII of '$'
    mov [SI],ch         ; and places it inside SI
    inc SI              ; pointing to the next place
    dec cl              ; if cl reaches 0, then we have moved all 4 chars
    jnz emptynextolettaro

    ; Temporarily executing the powerup
    mov ah, 0
    mov al,13h
    int 10h
    
    mov ax, 0
    mov ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 1      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ah, 9
    mov dx, offset Promttarg
    int 21h

    mov ax, 0
    mov ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 3      ;Ycoordinated
    mov bx, 0
    int 10h                          

    mov ax, 0
    mov ah, 0AH
    mov dx, offset buffertarg   ; the new target is placed in its variable

    int 21h

    convertToUCL dummytargetlol

    ; Validating the powerup
    ; Checks whether dummytargetlol was already in one of the registers
    ; If it was, then it's invalid, so dont change the real targetlol
    ; If it wasn't, then it's valid, so go change the real targetlol
    mov valTarLol,0             ; Initializes it to be valid
                                ; Then check if it is truly valid
    regloss ax1,dummytargetlol,valTarLol
    regloss bx1,dummytargetlol,valTarLol
    regloss cx1,dummytargetlol,valTarLol
    regloss dx1,dummytargetlol,valTarLol
    regloss si1,dummytargetlol,valTarLol
    regloss di1,dummytargetlol,valTarLol
    regloss sp1,dummytargetlol,valTarLol
    regloss bp1,dummytargetlol,valTarLol

    regloss ax2, dummytargetlol, valTarLol
    regloss bx2, dummytargetlol, valTarLol
    regloss cx2, dummytargetlol, valTarLol
    regloss dx2, dummytargetlol, valTarLol
    regloss si2, dummytargetlol, valTarLol
    regloss di2, dummytargetlol, valTarLol
    regloss sp2, dummytargetlol, valTarLol
    regloss bp2, dummytargetlol, valTarLol
    ; If it changes to 1 because of one of these registers
    ; Then it was an invalid target, so skip the powerup execution
    ; and the player who called the power up, can't call it again
    cmp valTarLol,1
    jnz StilloValido        ; If it's not zero, then it's valid
    jmp Cantusepower5       ; If it's zero, then it's invalid
    StilloValido:

    ; Having reached this point, then the powerup was valid, and the
    ; new target was not found in one of the registers. So, change
    ; the real targetlol



    mov DI, offset dummytargetlol       ; The variable we are writing from
    mov SI, offset targetlol            ; The variable we will write in
    mov cl, 4                           ; moving 4 chars
nextolettaro:
    mov ch,[DI]         ; Takes the value inside DI
    mov [SI],ch         ; and places it inside SI
    inc SI              ; pointing to the next place
    inc DI              ; pointing to the next char
    dec cl              ; if cl reaches 0, then we have moved all 4 chars
    jnz nextolettaro

    ;mov ax, word ptr dummytargetlol
    ;mov word ptr targetlol,ax


    

Cantusepower5:                  ; Jumps here if power can't be used
    popa   
    ret

PowerUp5 ENDP




GetMpp proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h



        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset PromtvalMP
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset buffermmp
        int 21h

        mov al, byte ptr mmmp
        sub al,30h  
        
        cmp al,1
        jz mv2
        sub al,1
        jmp mv1


        mv2:
            mov al,2
        mv1:
        mov  byte ptr player ,al
        mov bl,al
        call sendbayte


    popa

    ret
GetMpp ENDP



;Checks for power ups and executes them
chkPUUS proc far

pusha

  ;check on level

    cmp level,1
    jz lvl1chkk1

    ;level2:
    ; mov ah,1         ; byshoof etdas 3la 7aga (ZF=1) wala la (ZF=0)
    ; int 16h
    

    ;F4=closee
    cmp al,62
    ; jz endgmods  ;end the game
    jnz skkkk1
    jmp endgmods

    skkkk1:

    ;F5=pu1
    cmp ah,63
    jz callp1
    ;F6=pu2
    cmp ah,64
    jz callp2
    ;F7=pu3
    cmp ah,65
    jz callp3
    ;F8=pu4
    cmp ah,66
    jz callp4

    ;F9=pu5
    ; cmp ah,67
    ; jz callp5

    jmp sklell

    lvl1chkk1:
    ;Check if any of the F's has been pressed
    ; mov ah,0         ; byshoof etdas 3la 7aga (ZF=1) wala la (ZF=0)
    ; int 16h

    ;F4=close
    cmp ah,62
    ; jz endgmods   ;end the game
    jnz skkk1
    jmp endgmods

    skkk1:
    ;F5=pu1
    cmp ah,63
    jz callp1
    ;F6=pu2
    cmp ah,64
    jz callp2
    ;F7=pu3
    cmp ah,65
    jz callp3
    ;F8=pu4
    cmp ah,66
    jz callp4


    jmp sklell1


    ;if level 2, there should be an F9

    callp1:
    Call PowerUp1
    jmp sklell1


    callp2:
    Call PowerUp2
    jmp sklell1

    callp3:
    Call PowerUp3
    jmp sklell1

    callp4:
    Call PowerUp4

    CALL UpdateRegMem
    jmp sklell1

    callp5:
    Call PowerUp5
    jmp sklell1


    sklell1:
popa
ret

chkPUUS ENDP



chkPUUR proc far

pusha

  ;check on level

    cmp level,1
    jz lvl1chkk

    ;level2:
    ; mov ah,1         ; byshoof etdas 3la 7aga (ZF=1) wala la (ZF=0)
    ; int 16h
    

    ;F4=closee
    cmp al,62
    ; jz endgmods  ;end the game
    jnz skkkk
    jmp endgmods

    skkkk:

    ;F5=pu1
    cmp al,63
    jz callp1
    ;F6=pu2
    cmp al,64
    jz callp2
    ;F7=pu3
    cmp al,65
    jz callp3
    ;F8=pu4
    cmp al,66
    jz callp4

    ;F9=pu5
    ; cmp ah,67
    ; jz callp5

    jmp sklell

    lvl1chkk:
    ;Check if any of the F's has been pressed
    ; mov ah,0         ; byshoof etdas 3la 7aga (ZF=1) wala la (ZF=0)
    ; int 16h

    ;F4=close
    cmp al,62
    ; jz endgmods   ;end the game
    jnz skjkk1
    jmp endgmods

    skjkk1:
    ;F5=pu1
    cmp al,63
    jz callp11
    ;F6=pu2
    cmp al,64
    jz callp12
    ;F7=pu3
    cmp al,65
    jz callp13
    ;F8=pu4
    cmp al,66
    jz callp14


    jmp sklell


    ;if level 2, there should be an F9

    callp11:
    Call PowerUp1
    jmp sklell


    callp12:
    Call PowerUp2
    jmp sklell

    callp13:
    Call PowerUp3
    jmp sklell

    callp14:
    Call PowerUp4

    CALL UpdateRegMem
    jmp sklell

    callp15:
    Call PowerUp5
    jmp sklell


    sklell:
popa
ret

chkPUUR ENDP



Read1 proc far
    mov displacment1, 0
    mov displacment2, 0
    mov al, CurrentPlayer
    cmp player, al
    jne ReceiveCommand
    jmp WriteCommand
ReceiveCommand:
    ;Check that Data is Ready
    mov dx , 3FDH		; Line Status Register
    in al , dx 
    test al , 1
    jz ReceiveCommand                                 ;Not Ready
 ;If Ready read the VALUE in Receive data register
      	mov dx , 03F8H
        in al , dx
        
        call chkPUUR

        cmp al, 24h
        je ReceiveEndedC

        mov bx, displacment2
        lea si, Command
        mov [si][bx], al 
        inc displacment2 
        mov messageoffset, si 
        
        jmp ReceiveCommand  
ReceiveEndedC:
        mov Receiving, 1
        mov displacment2, 0
        ret
WriteCommand:
sendCommand:
call chkPUUR    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        jz sendCommand 
ReadyToSendCommand:
     	mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jz ReadyToSendCommand

        ;Remove char from buffer 
        mov ah, 0
        int 16h 
        ;Check if backspace, remove the letter
        cmp al, 8h
        je RemoveLetterC        
        ;Check if enter was pressed
        cmp al, 0Dh
        jne Tempjmp
        jmp PrintCommand    ;print the whole message
Tempjmp:        
        mov bx, displacment1
        cmp bx, 16
        jnz NotMaxlengthC
        jmp sendCommand
        ;Else, add the char in the message         
NotMaxlengthC:
        mov bx, displacment1
        lea si, Command
        mov [si][bx], al 
        inc displacment1 
        mov messageoffset, si 
        
        cmp player, 1
        je player1C
        ;Player 2:
        mov  ah, 02     ;SetCursorPosition
        mov dl, commandcol2      ;X coordinated
        mov dh, 19      ;Ycoordinated
        mov bx, 0
        int 10h
        inc commandcol2
        jmp PrintCommandChar

        player1C:
        mov ah, 02h    ;SetCursorPosition
        mov dl, commandcol1       ;X coordinated
        mov dh, 19      ;Ycoordinated
        mov bx, 0
        int 10h
        inc commandcol1
PrintCommandChar:        
        ;mov  al, dl
        mov  bl, 0AAh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
        
        jmp sendCommand
RemoveLetterC:  
        dec displacment1    
        mov bx, displacment1 
        lea si, Command
        mov byte ptr [si][bx], 24h 

        cmp player, 1
        je player1CC
        ;Player 2:
        dec commandcol2
        mov  ah, 02     ;SetCursorPosition
        mov dl, commandcol2      ;X coordinated
        mov dh, 19      ;Ycoordinated
        mov bx, 0
        int 10h
        jmp PrintCommandCharC

        player1CC:
        dec commandcol1
        mov ah, 02h    ;SetCursorPosition
        mov dl, commandcol1       ;X coordinated
        mov dh, 19      ;Ycoordinated
        mov bx, 0
        int 10h

        PrintCommandCharC:        
        mov  al, ' '
        mov  bl, 0AAh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
                                 
        jmp sendCommand

PrintCommand:  
;Check that Transmitter Holding Register is Empty     
        mov Sending, 1
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        JZ PrintCommand     ;Not empty 
  		
        ;print the message
        mov di, messageoffset
        mov al, byte ptr [di]
        cmp al, 24h
        je SendingEndC
  		
        mov dx , 3F8H        
        out dx, al 
        
        inc messageoffset
        jmp PrintCommand
SendingEndC: 
        mov dx , 3F8H
        mov al, 24h        
        out dx, al     ;Send $ to end sending 
        mov commandcol1, 1
        mov commandcol2, 22
        mov displacment1, 0
        ret
Read1 ENDP



Interpret proc
    mov valid2Chk,1     ; The command is valid, until proven otherwise
    lea si, command
    mov CommandNo, 0
RemoveSpaces:
    mov al, [si]
    cmp al, 0D         ;check if end of string is reached
    je Incorrect
    cmp al, 20h         ;check if the character is space
    je Increment        ;increment the pointer
    jmp CompareCommand
Increment:
    inc si
    jmp RemoveSpaces    ;check for spaces again

CompareCommand:         ;start comparing the command
    mov bx, si          ;Save the beginning of the command

;Compare if it is OR first
    lea di, ORCommandstr
    mov cx, 2           ;length of command is 3
    REPE CMPSB          ;compare with the first command in the list
    je SaveOpCodeOR

    lea di, CommandList ;Start comparing commands from the list if it is not OR command
    mov si, bx          ;reset the pointer to the beginning of the command line

Again:
    mov cx, 3           ;length of command is 3
    REPE CMPSB          ;compare with the first command in the list
    je SaveOpCode       ;if equal save the opcode
    inc CommandNo       ;else, increment command number

    cmp CommandNo, 16d  ;check if the end of the command list is reached
    jz Incorrect       ;command is incorrect

    ;Preparing the pointers for the next check
    mov si, bx          ;reset the pointer to the beginning of the command line
    mov ax, 0
    mov al, CommandNo
    mov dl, 3
    MUL dl               ;Calculate the displacement to move the pointer to the next command in the list
    lea di, CommandList ;reset the pointer
    add di , ax         ;move 3*(number of commands checked) steps

    jmp Again           ;compare with the next command in the list

SaveOpCodeOR:
    mov si, bx          ;reset the pointer to the beginning of the command line to save the opcode
    lea di, opcode      ;set destination string to opcode to save the opcode
    mov cx,2
    REPE MOVSB
    jmp CheckOperand

SaveOpCode:
    mov si, bx          ;reset the pointer to the beginning of the command line to save the opcode
    lea di, opcode      ;set destination string to opcode to save the opcode
    mov cx,3
    REPE MOVSB
    jmp CheckOperand

CheckOperand:
    mov bx, si          ;Save the beginning of the operand
    ; SI now points at the first char after the valid command
    ; That char should be a space or $, or else the command would be invalid
    ; ($ in case of commands that take no operands for example)

    mov al, 32          ; ASCII of space
    cmp al,[si]         ; checks if the DI currently has a space
    jz ContainSpace     ; if it does contain space, then it's valid

    mov al,0DH           ; ASCII of enter
    cmp al,[si]         ; checks if the DI currently has a space
    jz ContainEnter    ; if it does contain space, then it's valid

    ; if it reaches this point, then the command is not followed be neither a space
    ; nor a $, then it is an invalid command
    jmp Incorrect

    ContainSpace:
    ContainEnter:
    ret
Incorrect:
    mov valid2Chk,0     ; command is invalid, no need to continue the checks
    cmp player, 1
    je Player1LosePoint
    dec Score2InDec
    ScoreToString Score2, Score2InDec
    jmp EndInterpret
Player1LosePoint:
    dec Score1InDec
    ScoreToString Score1, Score1InDec
EndInterpret:
ret
Interpret ENDP

Interface proc far
    mov cx,4
    MOV BX, 54
rectangles:
    ShowRegister 16, BX
    ShowRegister 80, BX
    ShowRegister 176,BX
    ShowRegister 240,BX
    add BX, 16
    dec cx
    jz Next78
    jmp rectangles

Next78:
mov bl, 7
    mov al, 0
    mov ah, 1
    mov si, 0
    mov cx, 4
RegNames1:
    WriteChar disp, disp1, bl
    WriteChar 'X',disp2 , bl
    add bl, 2
    inc disp
    loop RegNames1
    cmp si, 1
    jz next2
    inc si
    mov cx, 4
    mov bl, 7
    mov disp, 41h
    add disp1, 20
    add disp2, 20
    jmp RegNames1

next2:
    mov disp1, 0
    mov disp2, 1  ;resets after each loop 34an tozbot mn el a5er idk why
    mov disp, 41h
    mov bl, 8
    mov bh, 9
    mov cx, 2
regnames2:
    writechar 'S', bl, 7
    writechar 'I', bh, 7
    writechar 'D', bl, 9
    writechar 'I', bh, 9
    add bl, 20
    add bh, 20
    dec cx
    jz next3
    jmp regnames2

next3:
    mov bl, 8
    mov bh, 9
    mov cx, 2
regnames3:
    WriteChar 'S', bl, 11
    WriteChar 'P', bh, 11
    WriteChar 'B', bl, 13
    WriteChar 'P', bh, 13
    add bl, 20
    add bh, 20
    dec cx
    ; jz next4
    jnz skdi
    jmp circles
    skdi:
    jmp regnames3


circles:
    ;first set
    ShowCirc 27, 138,7,6
    ShowCirc 43, 138,7,7
    ShowCirc 59, 138,7,8
    ShowCirc 75, 138,7,9
    ShowCirc 91, 138,7,10
    ;second set
    ShowCirc 187, 138,7,6
    ShowCirc 203, 138,7,7
    ShowCirc 219, 138,7,8
    ShowCirc 235, 138,7,9
    ShowCirc 251, 138,7,10

next4:
;;Draw Vertical Line
    mov cx, 157
    mov dx, 0
    mov al, 5
    mov ah, 0ch
back:
    int 10h
    inc dx
    cmp dx, 165
    jnz back

;;Draw Horizontal Lines
    mov cx, 0
    mov dx, 165
    mov al, 5
    mov ah, 0ch
back1:
    int 10h
    inc cx
    cmp cx, 320
    jnz back1

    mov cx, 0
    mov dx, 148
    mov al, 5
    mov ah, 0ch
    back2:   int 10h
            inc cx
            cmp cx, 320
            jnz back2

    ;;Rectangle 1:
;;Draw Horizontal Line
    mov cx, 5
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    back5:   int 10h
            inc cx
            cmp cx, 65
            jnz back5
;;Draw Vertical Line
    mov cx, 5
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    back6:   int 10h
            inc dx
            cmp dx, 25
            jnz back6
;;Draw Horizontal Line
    mov cx, 5
    mov dx, 25
    mov al, 5
    mov ah, 0ch
    back7:   int 10h
            inc cx
            cmp cx, 65
            jnz back7
;;Draw Vertical Line
    mov cx, 65
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    back8:   int 10h
            inc dx
            cmp dx, 25
            jnz back8

;;Rectangle 2:
;;Draw Horizontal Line
    mov cx, 175
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    back9:   int 10h
            inc cx
            cmp cx, 235
            jnz back9
;;Draw Vertical Line
    mov cx, 175
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    back10:   int 10h
            inc dx
            cmp dx, 25
            jnz back10
;;Draw Horizontal Line
    mov cx, 175
    mov dx, 25
    mov al, 5
    mov ah, 0ch
    back11:   int 10h
            inc cx
            cmp cx, 235
            jnz back11
;;Draw Vertical Line
    mov cx, 235
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    back12:   int 10h
            inc dx
            cmp dx, 25
            jnz back12

    ;;;;draw Memory :;;;;;;;
    WriteMemory HexNumbers
    ret
Interface ENDP

ResetVar proc
    push di
    push si
    push cx
    Lea di, VarHexstr1
    lea si, VarHexstr3
    mov cx, 2
    REPE MOVSW
    mov cx, 2
    Lea di, VarHexstr2
    REPE MOVSW

    mov cx, 6           ; To reset 6 chars
    Lea di, operand1   ; The variable we want to reset
    REPE MOVSB

    mov cx, 6           ; To reset 6 chars
    Lea di, operand2   ; The variable we want to reset
    REPE MOVSB

    mov cx, 16          ; To reset 16 chars
    Lea di, command    ; The variable we want to reset
    REPE MOVSB

    mov cx, 5           ; To reset 5 chars
    Lea di, opcode     ; The variable we want to reset
    REPE MOVSB
    ; lesa hnzwd el resets bta3t youssef


    mov AddressTypeSRC, 0
    mov AddressTypeDEST, 0
    mov MemoryTypeSRC, 0
    mov MemoryTypeDEST, 0
    mov AddressingMode, 0

    pop cx
    pop si
    pop di
    ret
ResetVar endp

MovCommand proc
    cmp operandsize, 1
    jnz sizeword
    jmp sizebyte
sizeword:
    ;size = word
    ;;we need to check whether the source or dest is memory
    cmp sourcemem, 1
    jnz checkdest
    jmp sourceismem
checkdest:
    cmp destmem, 1
    jz dummyjmp
    jmp allregisters
dummyjmp:
    jmp destismem
sourceismem: ;;source is memory
    readmemword varhexnum, [si], varhexstr1
    writeword [di], varhexnum, varhexstr2
    jmp EndCommand

destismem:   ;;destination is memory
    readword varhexnum, [si], varhexstr1
    writememword [di], varhexnum, varhexstr2
    jmp EndCommand
allregisters:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    writeword [di], varhexnum, varhexstr2
    jmp EndCommand
sizebyte: ;;if it is a byte, then read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    writebyte [di], varhexnum, varhexstr2
EndCommand:
    ret
MovCommand ENDP

AddCommand proc
    cmp operandsize, 1
    jnz SizeWord1
    jmp SizeByte1
SizeWord1:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp SourceMem, 1
    jnz CheckDest1
    jmp SourceIsMem1
CheckDest1:
    cmp DestMem, 1
    jz Dummyjmp1
    jmp AllRegisters1
Dummyjmp1:
    jmp DestIsMem1
SourceIsMem1: ;;Source is memory
    ReadMemWord varhexnum1, [si], varhexstr2
    readword varhexnum, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    add ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine2
    jmp SetCarry
NextLine2:
    mov carry, 0
    jmp EndMov1
DestIsMem1:   ;;Destination is memory
    readword varhexnum, [si], varhexstr1
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    add ax, bx
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jnc NextLine
    jmp SetCarry
NextLine:
    mov carry, 0
    jmp EndMov1
AllRegisters1:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    add ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine1
    jmp SetCarry
NextLine1:
    mov carry, 0
    jmp EndMov1
SizeByte1: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    add al, bl
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
    jc SetCarry
    mov carry, 0
    jmp EndMov1
SetCarry: mov carry, 1
EndMov1:
    cmp player, 1
    je SetCarryForP1
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry
SetCarryForP1:
    mov al, carry
    mov carry1, al
EndSaveCarry:
    ret
AddCommand ENDP

AdcCommand proc
    cmp player, 1
    je SetCarryP1
    mov al, carry2
    mov carry, al
    jmp StartADC
SetCarryP1:
    mov al, carry1
    mov carry, al
StartADC:

    cmp operandsize, 1
    jnz SizeWord2
    jmp SizeByte2
SizeWord2:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp SourceMem, 1
    jnz CheckDest2
    jmp SourceIsMem2
CheckDest2:
    cmp DestMem, 1
    jz Dummyjmp2
    jmp AllRegisters2
Dummyjmp2:
    jmp DestIsMem2
SourceIsMem2: ;;Source is memory
    ReadMemWord varhexnum1, [si], varhexstr2
    readword varhexnum, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    cmp carry, 1
    je addwithcarry
    jmp Nocarry
addwithcarry:
    STC
    adc ax, bx
    jmp SavetheResult3
Nocarry:
    add ax, bx
SavetheResult3:
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine3
    jmp SetCarry2
NextLine3:
    mov carry, 0
    jmp EndMov2
DestIsMem2:   ;;Destination is memory
    readword varhexnum, [si], varhexstr1
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    cmp carry, 1
    je addwithcarry2
    jmp Nocarry2
addwithcarry2:
    STC
    adc ax, bx
    jmp SavetheResult2
Nocarry2:
    add ax, bx
SavetheResult2:
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jnc NextLine4
    jmp SetCarry2
NextLine4:
    mov carry, 0
    jmp EndMov2
AllRegisters2:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    cmp carry, 1
    je addwithcarry3
    jmp Nocarry3
addwithcarry3:
    STC
    adc ax, bx
    jmp SavetheResult1
Nocarry3:
    add ax, bx
SavetheResult1:
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine5
    jmp SetCarry2
NextLine5:
    mov carry, 0
    jmp EndMov2
SizeByte2: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    mov ah, 0
    mov bh, 0
    cmp carry, 1
    je addwithcarry1
    jmp Nocarry1
addwithcarry1:
    STC
    adc al, bl
    jmp SavetheResult
Nocarry1:
    add al, bl
SavetheResult:
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
    jc SetCarry2
    mov carry, 0
    jmp EndMov2
SetCarry2: mov carry, 1
EndMov2:
    cmp player, 1
    je SetCarryForP11
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry1
SetCarryForP11:
    mov al, carry
    mov carry1, al
EndSaveCarry1:
    ret
AdcCommand ENDP

SubCommand proc
    cmp operandsize, 1
    jnz SizeWord3
    jmp SizeByte3
SizeWord3:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp SourceMem, 1
    jnz CheckDest3
    jmp SourceIsMem3
CheckDest3:
    cmp DestMem, 1
    jz Dummyjmp3
    jmp AllRegisters3
Dummyjmp3:
    jmp DestIsMem3
SourceIsMem3: ;;Source is memory
    ReadMemWord varhexnum1, [si], varhexstr2
    readword varhexnum, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    sub ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine6
    jmp SetCarry3
NextLine6:
    mov carry, 0
    jmp EndMov3
DestIsMem3:   ;;Destination is memory
    readword varhexnum, [si], varhexstr1
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum1
    mov bx, varhexnum
    sub ax, bx
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jnc NextLine7
    jmp SetCarry3
NextLine7:
    mov carry, 0
    jmp EndMov3
AllRegisters3:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum1
    mov bx, varhexnum
    sub ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine8
    jmp SetCarry3
NextLine8:
    mov carry, 0
    jmp EndMov3
SizeByte3: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr2
    mov ax, varhexnum1
    mov bx, varhexnum
    sub al, bl
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
    jc SetCarry3
    mov carry, 0
    jmp EndMov3
SetCarry3: mov carry, 1
EndMov3:
    cmp player, 1
    je SetCarryForP12
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry2
SetCarryForP12:
    mov al, carry
    mov carry1, al
EndSaveCarry2:
    ret
SubCommand ENDP

SbbCommand proc
    cmp player, 1
    je SetCarryToP1
    mov al, carry2
    mov carry, al
    jmp StartSBB
SetCarryToP1:
    mov al, carry1
    mov carry, al
StartSBB:

    cmp operandsize, 1
    jnz SizeWord4
    jmp SizeByte4
SizeWord4:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp SourceMem, 1
    jnz CheckDest4
    jmp SourceIsMem4
CheckDest4:
    cmp DestMem, 1
    jz Dummyjmp4
    jmp AllRegisters4
Dummyjmp4:
    jmp DestIsMem4
SourceIsMem4: ;;Source is memory
    ReadMemWord varhexnum1, [si], varhexstr2
    readword varhexnum, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    cmp carry, 1
    je subwithcarry
    jmp NoBorrow
subwithcarry:
    STC
    sbb ax, bx
    jmp SavetheResult4
NoBorrow:
    sub ax, bx
SavetheResult4:
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine11
    jmp SetCarry4
NextLine11:
    mov carry, 0
    jmp EndMov4
DestIsMem4:   ;;Destination is memory
    readword varhexnum, [si], varhexstr1
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum1
    mov bx, varhexnum
    cmp carry, 1
    je subwithcarry2
    jmp NoBorrow2
subwithcarry2:
    STC
    sbb ax, bx
    jmp SavetheResult5
NoBorrow2:
    sub ax, bx
SavetheResult5:
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jnc NextLine9
    jmp SetCarry4
NextLine9:
    mov carry, 0
    jmp EndMov4
AllRegisters4:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum1
    mov bx, varhexnum
    cmp carry, 1
    je subwithcarry3
    jmp NoBorrow3
subwithcarry3:
    STC
    sbb ax, bx
    jmp SavetheResult6
NoBorrow3:
    sub ax, bx
SavetheResult6:
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine10
    jmp SetCarry4
NextLine10:
    mov carry, 0
    jmp EndMov4
SizeByte4: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr2
    mov ax, varhexnum1
    mov bx, varhexnum
    mov ah, 0
    mov bh, 0
    cmp carry, 1
    je subwithcarry1
    jmp NoBorrow1
subwithcarry1:
    STC
    sbb al, bl
    jmp SavetheResult7
NoBorrow1:
    sub al, bl
SavetheResult7:
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
    jc SetCarry4
    mov carry, 0
    jmp EndMov4
SetCarry4: mov carry, 1
EndMov4:
    cmp player, 1
    je SetCarryForP13
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry3
SetCarryForP13:
    mov al, carry
    mov carry1, al
EndSaveCarry3:
    ret
SbbCommand ENDP

XorCommand proc
    mov carry, 0
    cmp operandsize, 1
    jnz SizeWord5
    jmp SizeByte5
SizeWord5:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp SourceMem, 1
    jnz CheckDest5
    jmp SourceIsMem5
CheckDest5:
    cmp DestMem, 1
    jz Dummyjmp5
    jmp AllRegisters5
Dummyjmp5:
    jmp DestIsMem5
SourceIsMem5: ;;Source is memory
    ReadMemWord varhexnum1, [si], varhexstr2
    readword varhexnum, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    XOR ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov5
DestIsMem5:   ;;Destination is memory
    readword varhexnum, [si], varhexstr1
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    XOR ax, bx
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jmp EndMov5
AllRegisters5:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    XOR ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov5
SizeByte5: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    XOR al, bl
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
EndMov5:
    cmp player, 1
    je SetCarryForP14
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry4
SetCarryForP14:
    mov al, carry
    mov carry1, al
EndSaveCarry4:
    ret
XorCommand ENDP

OrCommand proc
    mov carry, 0
    cmp operandsize, 1
    jnz SizeWord6
    jmp SizeByte6
SizeWord6:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp SourceMem, 1
    jnz CheckDest6
    jmp SourceIsMem6
CheckDest6:
    cmp DestMem, 1
    jz Dummyjmp6
    jmp AllRegisters6
Dummyjmp6:
    jmp DestIsMem6
SourceIsMem6: ;;Source is memory
    ReadMemWord varhexnum1, [si], varhexstr2
    readword varhexnum, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    OR ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov6
DestIsMem6:   ;;Destination is memory
    readword varhexnum, [si], varhexstr1
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    OR ax, bx
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jmp EndMov6
AllRegisters6:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    OR ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov6
SizeByte6: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    OR al, bl
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
EndMov6:
    cmp player, 1
    je SetCarryForP15
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry5
SetCarryForP15:
    mov al, carry
    mov carry1, al
EndSaveCarry5:
    ret
OrCommand ENDP

AndCommand proc
    mov carry, 0
    cmp operandsize, 1
    jnz SizeWord7
    jmp SizeByte7
SizeWord7:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp SourceMem, 1
    jnz CheckDest7
    jmp SourceIsMem7
CheckDest7:
    cmp DestMem, 1
    jz Dummyjmp7
    jmp AllRegisters7
Dummyjmp7:
    jmp DestIsMem7
SourceIsMem7: ;;Source is memory
    ReadMemWord varhexnum1, [si], varhexstr2
    readword varhexnum, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    AND ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov7
DestIsMem7:   ;;Destination is memory
    readword varhexnum, [si], varhexstr1
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum
    mov bx, varhexnum1
    AND ax, bx
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jmp EndMov7
AllRegisters7:   ;;sorce and destination are registers
    readword varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    AND ax, bx
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov7
SizeByte7: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr2
    mov ax, varhexnum
    mov bx, varhexnum1
    AND al, bl
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
EndMov7:
    cmp player, 1
    je SetCarryForP16
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry6
SetCarryForP16:
    mov al, carry
    mov carry1, al
EndSaveCarry6:
    ret
AndCommand ENDP

SHRCommand proc
    cmp operandsize, 1
    jnz SizeWord8
    jmp SizeByte8
SizeWord8:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp DestMem, 1
    jnz AllRegisters8
    jmp SizeByte8
AllRegisters8:   ;;sorce and destination are registers
    readbyte varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov cx, varhexnum
    mov ch, 0
    mov ax, varhexnum1
    SHR ax, cl
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine13
    jmp SetCarry8
NextLine13:
    mov carry, 0
    jmp EndMov8
SizeByte8: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr1
    mov cx, varhexnum
    mov ch, 0
    mov ax, varhexnum1
    mov ah, 0
    SHR al, cl
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
    jc SetCarry8
    mov carry, 0
    jmp EndMov8
SetCarry8: mov carry, 1
EndMov8:
    cmp player, 1
    je SetCarryForP17
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry7
SetCarryForP17:
    mov al, carry
    mov carry1, al
EndSaveCarry7:
    ret
SHRCommand ENDP

ShlCommand proc
    cmp operandsize, 1
    jnz SizeWord9
    jmp SizeByte9
SizeWord9:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp DestMem, 1
    jnz AllRegisters9
    jmp SizeByte9
AllRegisters9:   ;;sorce and destination are registers
    readbyte varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov cx, varhexnum
    mov ch, 0
    mov ax, varhexnum1
    SHL ax, cl
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine15
    jmp SetCarry9
NextLine15:
    mov carry, 0
    jmp EndMov9
SizeByte9: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr1
    mov cx, varhexnum
    mov ch, 0
    mov ax, varhexnum1
    mov ah, 0
    SHL al, cl
    mov ah, 0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
    jc SetCarry9
    mov carry, 0
    jmp EndMov9
SetCarry9: mov carry, 1
EndMov9:
    cmp player, 1
    je SetCarryForP18
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry8
SetCarryForP18:
    mov al, carry
    mov carry1, al
EndSaveCarry8:
    ret
ShlCommand ENDP

RorCommand proc
    cmp operandsize, 1
    jnz SizeWord10
    jmp SizeByte10
SizeWord10:
    ;size = word
    ;;We need to check whether the source or dest is memory
    cmp DestMem, 1
    jnz AllRegisters10
    jmp SizeByte10
AllRegisters10:   ;;sorce and destination are registers
    readbyte varhexnum, [si], varhexstr1
    readword varhexnum1, [di], varhexstr2
    mov cx, varhexnum
    mov ch, 0
    mov ax, varhexnum1
    ROR ax, cl
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jnc NextLine16
    jmp SetCarry10
NextLine16:
    mov carry, 0
    jmp EndMov10
SizeByte10: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum, [si], varhexstr1
    readbyte varhexnum1, [di], varhexstr1
    mov cx, varhexnum
    mov ch, 0
    mov ax, varhexnum1
    mov ah, 0
    ROR al, cl
    mov ah, 0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
    jc SetCarry10
    mov carry, 0
    jmp EndMov10
SetCarry10: mov carry, 1
EndMov10:
    cmp player, 1
    je SetCarryForP19
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry9
SetCarryForP19:
    mov al, carry
    mov carry1, al
EndSaveCarry9:
    ret
RorCommand ENDP

IncCommand proc
    cmp operandsize, 1
    jnz SizeWord11
    jmp SizeByte11
SizeWord11:
    ;size = word
    ;;We need to check whether the dest is memory
    cmp DestMem, 1
    jz DestIsMem11
    jmp AllRegisters11
DestIsMem11:   ;;Destination is memory
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum1
    inc ax
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jmp EndMov11
AllRegisters11:   ;;sorce and destination are registers
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum1
    inc ax
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov11
SizeByte11: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum1, [di], varhexstr1
    mov ax, varhexnum1
    mov ah,0
    inc al
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
EndMov11:
    ret
IncCommand ENDP

DecCommand proc
    cmp operandsize, 1
    jnz SizeWord12
    jmp SizeByte12
SizeWord12:
    ;size = word
    ;;We need to check whether the dest is memory
    cmp DestMem, 1
    jz DestIsMem12
    jmp AllRegisters12
DestIsMem12:   ;;Destination is memory
    readmemword varhexnum1, [di], varhexstr1
    mov ax, varhexnum1
    dec ax
    mov varhexnum1, ax
    writememword [di], varhexnum1, varhexstr2
    jmp EndMov12
AllRegisters12:   ;;sorce and destination are registers
    readword varhexnum1, [di], varhexstr2
    mov ax, varhexnum1
    dec ax
    mov varhexnum1, ax
    writeword [di], varhexnum1, varhexstr2
    jmp EndMov12
SizeByte12: ;;If it is a byte, then Read and write works for both registers and memory
    readbyte varhexnum1, [di], varhexstr1
    mov ax, varhexnum1
    mov ah,0
    dec al
    mov ah,0
    mov varhexnum1, ax
    writebyte [di], varhexnum1, varhexstr2
EndMov12:
    ret
DecCommand ENDP

ClcCommand proc
    mov carry, 0
    cmp player, 1
    je SetCarryForP110
    mov al, carry
    mov carry2, al
    jmp EndSaveCarry10
SetCarryForP110:
    mov al, carry
    mov carry1, al
EndSaveCarry10:
    ret
ClcCommand ENDP

MulCommand proc
    cmp player, 2       ;If player 2, execute on player 1 MP
    je Player1MP
    jmp Player2MP
Player1MP:
    cmp operandsize, 1
    jnz SizeWord13
    jmp SizeByte13
SizeWord13:
    readword varhexnum, [di], varhexstr1
    readword varhexnum1, ax1, varhexstr2
    mov ax, varhexnum1
    mov bx, varhexnum
    mul bx
    mov varhexnum1, ax
    mov varhexnum, dx
    writeword ax1, varhexnum1, varhexstr2
    writeword dx1, varhexnum, varhexstr2
    jmp EndMul
SizeByte13:
    readbyte varhexnum, [di], varhexstr1
    readbyte varhexnum1, al1, varhexstr1
    mov ax, varhexnum1
    mov ah, 0
    mov bx, varhexnum
    mov bh, 0
    mul bl
    mov varhexnum1, ax
    writeword ax1, varhexnum1, varhexstr2
    jmp EndMul
Player2MP:
    cmp operandsize, 1
    jnz SizeWord14
    jmp SizeByte14
SizeWord14:
    readword varhexnum, [di], varhexstr1
    readword varhexnum1, ax2, varhexstr2
    mov ax, varhexnum1
    mov bx, varhexnum
    mul bx
    mov varhexnum1, ax
    mov varhexnum, dx
    writeword ax2, varhexnum1, varhexstr2
    writeword dx2, varhexnum, varhexstr2
    jmp EndMul
SizeByte14:
    readbyte varhexnum, [di], varhexstr1
    readbyte varhexnum1, al2, varhexstr1
    mov ax, varhexnum1
    mov ah, 0
    mov bx, varhexnum
    mov bh, 0
    mul bl
    mov varhexnum1, ax
    writeword ax2, varhexnum1, varhexstr2
EndMul:
    ret
MulCommand ENDP

DivCommand proc
    cmp player, 2       ;If player 2, execute on player 1 MP
    je Player1MP1
    jmp Player2MP1
Player1MP1:
    cmp operandsize, 1
    jnz SizeWord15
    jmp SizeByte15
SizeWord15:
    readword varhexnum, [di], varhexstr1
    readword varhexnum1, ax1, varhexstr1
    readword varhexnum2, dx1, varhexstr1
    mov ax, varhexnum1
    mov bx, varhexnum
    mov dx, varhexnum2
    Div bx
    mov varhexnum1, ax
    mov varhexnum, dx
    writeword ax1, varhexnum1, varhexstr2
    writeword dx1, varhexnum, varhexstr2
    jmp EndDiv
SizeByte15:
    readbyte varhexnum, [di], varhexstr1
    readword varhexnum1, ax1, varhexstr1
    mov ax, varhexnum1
    mov bx, varhexnum
    mov bh, 0
    Div bl
    mov varhexnum1, ax
    writeword ax1, varhexnum1, varhexstr2
    jmp EndDiv
Player2MP1:
    cmp operandsize, 1
    jnz SizeWord16
    jmp SizeByte16
SizeWord16:
    readword varhexnum, [di], varhexstr1
    readword varhexnum1, ax2, varhexstr1
    readword varhexnum2, dx2, varhexstr1
    mov ax, varhexnum1
    mov bx, varhexnum
    mov dx, varhexnum2
    Div bx
    mov varhexnum1, ax
    mov varhexnum, dx
    writeword ax2, varhexnum1, varhexstr2
    writeword dx2, varhexnum, varhexstr2
    jmp EndDiv
SizeByte16:
    readbyte varhexnum, [di], varhexstr1
    readword varhexnum1, ax2, varhexstr1
    mov ax, varhexnum1
    mov bx, varhexnum
    mov bh, 0
    Div bl
    mov varhexnum1, ax
    writeword ax2, varhexnum1, varhexstr2
EndDiv:
    ret
DivCommand ENDP

DRandL proc far
;Draw registers and lines
pusha
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;draw Memory :;;;;;;;
        WriteMemory HexNumbers
        mov bl, 7
        mov al, 0
        mov ah, 1
        mov si, 0
        mov cx, 4
    RegNames1_:
        WriteChar disp, disp1, bl
        WriteChar 'X',disp2 , bl
        add bl, 2
        inc disp
        loop RegNames1_
        cmp si, 1
        jz next2_
        inc si
        mov cx, 4
        mov bl, 7
        mov disp, 41h
        add disp1, 20
        add disp2, 20
        jmp RegNames1_

    next2_:
        mov disp1, 0
        mov disp2, 1  ;resets after each loop 34an tozbot mn el a5er idk why
        mov disp, 41h
        mov bl, 8
        mov bh, 9
        mov cx, 2
    regnames2_:
        writechar 'S', bl, 7
        writechar 'I', bh, 7
        writechar 'D', bl, 9
        writechar 'I', bh, 9
        add bl, 20
        add bh, 20
        dec cx
        jz next3_
        jmp regnames2_

    next3_:
        mov bl, 8
        mov bh, 9
        mov cx, 2
    regnames3_:
        WriteChar 'S', bl, 11
        WriteChar 'P', bh, 11
        WriteChar 'B', bl, 13
        WriteChar 'P', bh, 13
        add bl, 20
        add bh, 20
        dec cx
        jz next4_
        jmp regnames3_
    next4_:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   mov cx,4
    MOV BX, 54
Drectangles:
    ShowRegister 16, BX
    ShowRegister 80, BX
    ShowRegister 176,BX
    ShowRegister 240,BX
    add BX, 16
    dec cx
    jz DNext
    jmp Drectangles

DNext:
;;Draw Vertical Line
    mov cx, 157
    mov dx, 0
    mov al, 5
    mov ah, 0ch
Dback:
    int 10h
    inc dx
    cmp dx, 165
    jnz Dback

;;Draw Horizontal Lines
    mov cx, 0
    mov dx, 165
    mov al, 5
    mov ah, 0ch
Dback1:
    int 10h
    inc cx
    cmp cx, 320
    jnz Dback1

    mov cx, 0
    mov dx, 148
    mov al, 5
    mov ah, 0ch
    Dback2:   int 10h
            inc cx
            cmp cx, 320
            jnz Dback2

    ;;Rectangle 1:
;;Draw Horizontal Line
    mov cx, 5
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    Dback5:   int 10h
            inc cx
            cmp cx, 65
            jnz Dback5
;;Draw Vertical Line
    mov cx, 5
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    Dback6:   int 10h
            inc dx
            cmp dx, 25
            jnz Dback6
;;Draw Horizontal Line
    mov cx, 5
    mov dx, 25
    mov al, 5
    mov ah, 0ch
    Dback7:   int 10h
            inc cx
            cmp cx, 65
            jnz Dback7
;;Draw Vertical Line
    mov cx, 65
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    Dback8:   int 10h
            inc dx
            cmp dx, 25
            jnz Dback8

;;Rectangle 2:
;;Draw Horizontal Line
    mov cx, 175
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    Dback9:   int 10h
            inc cx
            cmp cx, 235
            jnz Dback9
;;Draw Vertical Line
    mov cx, 175
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    Dback10:   int 10h
            inc dx
            cmp dx, 25
            jnz Dback10
;;Draw Horizontal Line
    mov cx, 175
    mov dx, 25
    mov al, 5
    mov ah, 0ch
    Dback11:   int 10h
            inc cx
            cmp cx, 235
            jnz Dback11
;;Draw Vertical Line
    mov cx, 235
    mov dx, 10
    mov al, 5
    mov ah, 0ch
    Dback12:   int 10h
            inc dx
            cmp dx, 25
            jnz Dback12
Dcircles:
    ;first set
    ShowCirc 27, 138,7,6
    ShowCirc 43, 138,7,7
    ShowCirc 59, 138,7,8
    ShowCirc 75, 138,7,9
    ShowCirc 91, 138,7,10
    ;second set
    ShowCirc 187, 138,7,6
    ShowCirc 203, 138,7,7
    ShowCirc 219, 138,7,8
    ShowCirc 235, 138,7,9
    ShowCirc 251, 138,7,10


popa
ret

DRandL ENDP




;Applies what was recieved from the other side
Applyrec proc far
    pusha

    ;0,1,2,3   up,down,left,right p1
    ;4,5,6,7   up,down,left,right p2
    ;8,9 shoot1,2
    ;10,11 collided1,2

    cmp chr,15
    ; jz uppp
    jnz skARR
    jmp uppp
    
    skARR:

    cmp chr,1
    ; jz downnn
    jnz skARR1
    jmp downnn
    skARR1:

    cmp chr,2
    ; jz lefttt
    jnz skARR2
    jmp lefttt
    skARR2:


    cmp chr,3
    ; jz righttt
    jnz skARR3
    jmp righttt
    
    skARR3:

    cmp chr,4
    ; jz WWW
    jnz skARR4
    jmp WWW
    skARR4:




    cmp chr,5
    ; jz SSS
    jnz skARR5
    jmp SSS
    skARR5:



    cmp chr,6
    ; jz aSa    ;AAA
    jnz skARR6
    jmp ASA   ;AAA
    
    skARR6:


    cmp chr,7
    ; jz DDD
    jnz skARR7
    jmp DDD

    skARR7:


    ;8,9 shoot1,2
    ;10,11 collided1,2


    cmp chr,8
    ; jz shoott1
    jnz skARR8
    jmp shoott1
    
    skARR8:


    cmp chr,9
    ; jz shoott2
    jnz skARR9
    jmp shoott2

    skARR9:

    cmp chr,10
    ; jz colldd1
    jnz skARRA
    jmp colldd1

    skARRA:

    ; cmp chr,11
    ; ; jz colldd2
    ; jnz skARRB
    jmp colldd2



    uppp: 
    lea di,p1position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp di,8


        ;jz movee
        jnz uouo
        jmp endApprc
        uouo:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    sub di,2
    mov bx,di

    lea di,p1position
    add di,2
    mov [di],bx


    jmp endApprc      ; ynzl y5ls



    downnn:
    lea di,p1position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp di,196


        ;jz movee
        jnz uouo1
        jmp endApprc
        uouo1:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    add di,2
    mov bx,di

    lea di,p1position
    add di,2
    mov [di],bx


    jmp endApprc      ; ynzl y5ls


    lefttt:
    lea di,p1position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp si,10


        ;jz movee
        jnz uouo2
        jmp endApprc
        uouo2:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    sub si,2
    mov bx,si

    lea si,p1position
    mov [si],bx


    jmp endApprc      ; ynzl y5ls




    righttt:
    lea di,p1position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp si,150


        ;jz movee
        jnz uouo3
        jmp endApprc
        uouo3:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    add si,2
    mov bx,si

    lea si,p1position
    mov [si],bx


    jmp endApprc      ; ynzl y5ls


    WWW:
    lea di,p2position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp di,8


        ;jz movee
        jnz uouo4
        jmp endApprc
        uouo4:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    sub di,2
    mov bx,di

    lea di,p2position
    add di,2
    mov [di],bx


    jmp endApprc      ; ynzl y5ls



    SSS:
    lea di,p2position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp di,196


        ;jz movee
        jnz uouo5
        jmp endApprc
        uouo5:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    add di,2
    mov bx,di

    lea di,p2position
    add di,2
    mov [di],bx


    jmp endApprc      ; ynzl y5ls



    aSa:  ;AAA  
    lea di,p2position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp si,168


        ;jz movee
        jnz uouo6
        jmp endApprc
        uouo6:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    sub si,2
    mov bx,si

    lea si,p2position
    mov [si],bx


    jmp endApprc      ; ynzl y5ls


    DDD:
    lea di,p2position
    mov si,[di]
    add di,2
    mov di,[di]

    cmp si,310


        ;jz movee
        jnz uouo7
        jmp endApprc
        uouo7:

    call wtFrVR    ;Wait for VR

    ShowTring si,di, 0

    add si,2
      mov bx,si

    lea si,p2position
    mov [si],bx


    jmp endApprc      ; ynzl y5ls





    shoott1: 
    call spwbult1
    jmp endApprc      ; ynzl y5ls

    shoott2:
    call spwbult2
    jmp endApprc      ; ynzl y5ls



    colldd1:

    lea si,shapeC
    add si,2
    mov di,[si]
    sub si,2
    mov si,[si]

    ShowCirc si,di,5,0   ;draw a black circle

    ;for now

    add si,158

    ShowCirc si,di,5,0   ;draw a black circle

    ;now we set the shape to not exist
    call FinBul1
    call FinBul2

    lea di, shexist
    mov si,0
    mov [di],si



    mov si, ShpClr
    sub si,5
    add Score1InDec,si
    ScoreToString Score1, Score1InDec


    add si,5
    ;update the number of balls

    ;6,7,8,9,10

    cmp si,6
    jz addOrr1

    cmp si,7
    jz addWGr1

    cmp si,8
    jz addDGr1

    cmp si,9
    jz addBr1

    jmp addGr1



    addOrr1:
    add Ob1,1
    jmp skABr

    addWGr1:
    add WGb1,1
    jmp skABr

    addDGr1:
    add DGb1,1
    jmp skABr

    addBr1:
    add Bb1,1
    jmp skABr

    addGr1:
    add Gb1,1


    skABr:

    mov ShpClr,6
    mov modee,3    ;back to playing the main game


    mov TrnsBMG, 2    ;default number
    mov Trnsnmbr,0

    CALL rndfreq    ;pick a random number of turns before starting the minigame


    jmp endApprc



    colldd2:
    lea si,shapeC
    add si,2
    mov di,[si]
    sub si,2
    mov si,[si]

    ShowCirc si,di,5,0   ;draw a black circle

    ;for now

    add si,158

    ShowCirc si,di,5,0   ;draw a black circle

    ;now we set the shape to not exist
    call FinBul1
    call FinBul2

    lea di, shexist
    mov si,0
    mov [di],si



    mov si, ShpClr
    sub si,5
    add Score2InDec,si
    ScoreToString Score2, Score2InDec


    add si,5
    ;update the number of balls

    ;6,7,8,9,10

    cmp si,6
    jz addOrr2

    cmp si,7
    jz addWGr2

    cmp si,8
    jz addDGr2

    cmp si,9
    jz addBr2

    jmp addGr2



    addOrr2:
    add Ob2,1
    jmp skABr2

    addWGr2:
    add WGb2,1
    jmp skABr2

    addDGr2:
    add DGb2,1
    jmp skABr2

    addBr2:
    add Bb2,1
    jmp skABr2

    addGr2:
    add Gb2,1


    skABr2:

    mov ShpClr,6
    mov modee,3    ;back to playing the main game


    mov TrnsBMG, 2    ;default number
    mov Trnsnmbr,0

    CALL rndfreq    ;pick a random number of turns before starting the minigame





    endApprc:

    mov hasrc,0

    popa
    ret

Applyrec ENDP



minigame proc far

pusha

mov bl,1 
call sendbayte


mov hasrc,0

mov Shexist,1

mov modee,2 ;set the game mode to 3 for minigame

call rndclr   ;feeh hena mo4kla!
; Erroor

jmp movee


;player1

up:
  lea di,p1position
  mov si,[di]
  add di,2
  mov di,[di]

  cmp di,8


    ;jz movee
    jnz skmgj
    jmp movee
    skmgj:

  call wtFrVR    ;Wait for VR





  ShowTring si,di, 0

  sub di,2
  mov bx,di

  lea di,p1position
  add di,2
  mov [di],bx


  push bx
    mov bl,15  ; 15 is for p1 up
    call sendbayte

  pop bx

  jmp movee      ; ynzl ymove



down:

  lea di,p1position
  mov si,[di]
  add di,2
  mov di,[di]




  cmp di,196      ; da keda el max, momken possible values: 165 or more likely ****148****

  ;jz movee
  jnz skmgj2
  jmp movee
  skmgj2:

  call wtFrVR    ;Wait for VR

  ShowTring si,di, 0

  add di,2
  mov bx,di

  lea di,p1position
  add di,2
  mov [di],bx


  push bx
    mov bl,1  ; 1 is for p1 down
    call sendbayte

  pop bx

  jmp movee      ; ynzl ymove


left:

  lea di,p1position
  mov si,[di]
  add di,2
  mov di,[di]

  cmp si,10    ;must be even

  ;jz movee
  jnz skmgj3
  jmp movee
  skmgj3:



  call wtFrVR    ;Wait for VR

  ShowTring si,di, 0
  sub si,2
  mov bx,si

  lea si,p1position
  mov [si],bx


  push bx
    mov bl,2  ; 2 is for p1 left
    call sendbayte

  pop bx

  jmp movee      ; ynzl ymove

right:

  lea di,p1position
  mov si,[di]
  add di,2
  mov di,[di]

  cmp si,150    ;player1 side , must be even!



    ;jz movee
    jnz skmgj4
    jmp movee
    skmgj4:

  call wtFrVR    ;Wait for VR

  ShowTring si,di, 0


  add si,2
  mov bx,si

  lea si,p1position
  mov [si],bx


  push bx
  mov bl,3  ; 3 is for p1 right
  call sendbayte

  pop bx

  jmp movee     ; ynzl ymove






;;;player 2

W:
  lea di,p2position
  mov si,[di]
  add di,2
  mov di,[di]

  cmp di,8


  ;jz movee
  jnz skmgj5
  jmp movee
  skmgj5:


  call wtFrVR    ;Wait for VR





  ShowTring si,di, 0

  sub di,2
  mov bx,di

  lea di,p2position
  add di,2
  mov [di],bx

  push bx
  mov bl,4  ; 4 is for p2 up
  call sendbayte

  pop bx

  jmp movee      ; ynzl ymove




S:

  lea di,p2position
  mov si,[di]
  add di,2
  mov di,[di]




  cmp di,196      ; da keda el max, momken possible values: 165 or more likely ****148****


  ;jz movee
  jnz skmgj6
  jmp movee
  skmgj6:



  call wtFrVR    ;Wait for VR

  ShowTring si,di, 0

  add di,2
  mov bx,di

  lea di,p2position
  add di,2
  mov [di],bx


  push bx
  mov bl,5  ; 5 is for p2 down
  call sendbayte

  pop bx

  jmp movee      ; ynzl ymove


A:

  lea di,p2position
  mov si,[di]
  add di,2
  mov di,[di]

  cmp si,168   ;must be even


  ;jz movee
  jnz skmgj7
  jmp movee
  skmgj7:

  call wtFrVR    ;Wait for VR

  ShowTring si,di, 0
  sub si,2
  mov bx,si

  lea si,p2position
  mov [si],bx


  push bx
  mov bl,6  ; 6 is for p2 left
  call sendbayte

  pop bx


  jmp movee      ; ynzl ymove


D:

  lea di,p2position
  mov si,[di]
  add di,2
  mov di,[di]

  cmp si,310   ; must be even!


  ;jz movee
  jnz skmgj8
  jmp movee
  skmgj8:



  call wtFrVR    ;Wait for VR


  ShowTring si,di, 0


  add si,2
  mov bx,si

  lea si,p2position
  mov [si],bx



  push bx
  mov bl,7  ; 7 is for p2 Right
  call sendbayte

  pop bx

  jmp movee     ; ynzl ymove



space:

    ;p1 shooting

    call spwbult1

    push bx
    mov bl,8  ; 8 is for p1 shooting
    call sendbayte
    pop bx

    jmp movee



F:
    ;p2 shoot (cx,dx)

    call spwbult2


    push bx
    mov bl,9  ; 9 is for p2 shooting
    call sendbayte
    pop bx
    
    jmp movee



    reccc:   ;; in case of recieval

    call Applyrec

    jmp normMovee
    




movee:

    ;phase2: 

    ;you need to check first that the bullet hasn't been popped in the other terminal
    ; you need to update the position of the other player
    
    ;recieves in chr
    ;has a flag called hasrc
    call recbayte

    cmp hasrc,0
    jz normMovee
    jmp reccc







    normMovee:
    ;BackGround

        pusha
        WriteName Name1, player
        inc player
        WriteName Name2, player
        dec player
        ;;;;Write Player Scores:;;;;;;;
        WriteScore Score1, player
        inc player
        WriteScore Score2, player
        dec player

        ;;;;;;;;; Draw game interface;;;;
        Call DRandL

        popa



    call mb1 ;move bullets of player1
    call mb2 ;move bullets of player2


    cmp modee,3  ;if one of them collided the game mode will change and we should go out
    ; jz MGend
    jnz skMGen
    jmp MGend
    skMGen:

    call mo  ;move object






    call mp1
    call mp2



    mov ah,1       ; byshoof etdas 3la 7aga (ZF=1) wala la (ZF=0)
    int 16h        ;the interrupt updates the ah

    mov dh, ah     ; 3lshan a-save el rkm el hkaren byh
    ; jz  movee      ; lw la, print w check tany
    jnz skmgj9
    jmp movee
    skmgj9:




    ;consumes letter

    mov ah,0
    int 16h
    ; up(48h)
    ; down(50h)
    ; right(04Dh)
    ; left (04BH)
    ; space (57)

    ;for p2
    ; W (17)
    ; A (30)
    ; S (31	)
    ; D (32)
    ; F (33)







; up
cmp dh,48h    ; dyh scan codes el arrow keys
; jz  up
jnz skmgjA
jmp up
skmgjA:

; down
cmp dh,50h     ; howa by7ot elly etdas fy al
; jz  down
jnz skmgjB
jmp down
skmgjB:


;right
cmp dh, 04Dh
; jz  right
jnz skmgjC
jmp right
skmgjC:


;left
cmp dh, 04BH
; jz left

jnz skmgjD
jmp left
skmgjD:


cmp dh,57
; jz  space
jnz skmgjE
jmp space
skmgjE:


;W
cmp dh,17
; jz  W
jnz skmgjF
jmp W
skmgjF:

;A
cmp dh,30
; jz  A
jnz skmgjG
jmp A
skmgjG:


;S
cmp dh,31
; jz  S
jnz skmgjH
jmp S
skmgjH:


;D
cmp dh,32
; jz  D
jnz skmgjI
jmp D
skmgjI:

;F
cmp dh,33
; jz F
jnz skmgjJ
jmp F
skmgjJ:


jmp movee      ; erg3 etba3


MGend:    ;minigame end


popa
ret

minigame  ENDP




SourceAddressStringParser proc near
push dx
push cx
push bx
push ax
mov ImmediateValue1, 0
mov ImmediateValue2,0
mov MemoryTypeSRC,0
mov AddressTypeSRC,0

cmp operand2,'[' ;check if operand2 is an direct or an indirect memory
jz ISnotregSRC
jmp ISregSRC
ISnotregSRC:
cmp operand2+2,']' ;check if direct memory ---> direct memory is defined as [x] where x is on range of 0 to F
jz ddddddd
jmp IsInDirectMemorySRC
ddddddd:
mov cx,0
MemoryAssignmentSRC:
; mov cl,operand2+1
; cmp cl,29h
; jg NoAddressErrorSRC1
; jmp AddressErrorSRC
; NoAddressErrorSRC1:
; cmp cl,39h
; jg StartfromAMemory
; sub cl,30h
; jmp Player1MemorySRC
; StartfromAMemory:
; cmp cl,40h
; jg NoAddressErrorSRC2
; jmp AddressErrorSRC
; NoAddressErrorSRC2:
; cmp cl,47h
; jl NoAddressErrorSRC3
; jmp AddressErrorSRC
; NoAddressErrorSRC3:
; sub cl,37h
Str_Hex operand2, dummy
mov cx, dummy
Player1MemorySRC:
cmp player,1
jnz Player2DirectMemorySRC
mov si,offset mem2
add si,cx
cmp AddressTypeSRC,0
jz NotendAddressCheckerSRC1

mov MemoryTypeSRC,1

jmp endAddressCheckerSRC

NotendAddressCheckerSRC1:
mov AddressTypeSRC,3
mov MemoryTypeSRC,1
jmp endAddressCheckerSRC
Player2DirectMemorySRC:
mov si,offset mem1
add si,cx
cmp AddressTypeSRC,0
jz NotendAddressCheckerSRC2
jmp endAddressCheckerSRC
NotendAddressCheckerSRC2:
mov AddressTypeSRC,3
jmp endAddressCheckerSRC
IsInDirectMemorySRC: ;Use address inside each of those variables as offset to be delivered by si
cmp operand2+1,'B'
jnz IsSISRC
cmp operand2+2,'X'
jz NoAddressErrorSRC4
jmp AddressErrorSRC
NoAddressErrorSRC4:
cmp player,1
jnz Player2BXIndirectSRC
mov cl,bl2+1
mov operand2+1,cl
mov operand2+2,']'
mov operand2+3,'$'
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
Player2BXIndirectSRC:
mov cl,bl1+1
mov operand2+1,cl
mov operand2+2,']'
mov operand2+3,'$'
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
IsSISRC:
cmp operand2+1,'S'
jnz IsDISRC
cmp operand2+2,'I'
jz NoAddressErrorSRC5
jmp AddressErrorSRC
NoAddressErrorSRC5:
cmp player,1
jnz Player2SIIndirectSRC
mov cl,si2+3
mov operand2+1,cl
mov operand2+2,']'
mov operand2+3,'$'
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
Player2SIIndirectSRC:
mov cl,si1+3
mov operand2+1,cl
mov operand2+2,']'
mov operand2+3,'$'
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
IsDISRC:
cmp operand2+1,'D'
jz NoAddressErrorSRC6
jmp AddressErrorSRC
NoAddressErrorSRC6:
cmp operand2+2,'I'
jz NoAddressErrorSRC7
jmp AddressErrorSRC
NoAddressErrorSRC7:
cmp player,1
jnz Player2DIIndirectSRC
mov cl,di2+3
mov operand2+1,cl
mov operand2+2,']'
mov operand2+3,'$'
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
Player2DIIndirectSRC:
mov cl,di1+3
mov operand2+1,cl
mov operand2+2,']'
mov operand2+3,'$'
mov AddressTypeSRC,4
jmp MemoryAssignmentSRC
ISregSRC:         ;operand2 is a register
cmp operand2,'A'
jz IfitisnotBSRC
jmp IfitisBSRC
IfitisnotBSRC:
cmp operand2+1,'X' ;check if it is Register AX
jnz IsItAHSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'H' ;check if it is Register AH
jnz IsItALSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'L' ;check if it is Register AL
jz NoAddressErrorSRC8
jmp AddressErrorSRC
NoAddressErrorSRC8:
cmp player,1 ;check if player1 is entering this command
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
cmp operand2,'B'
jz IfitisnotCSRC
jmp IfitisCSRC
IfitisnotCSRC:
cmp operand2+1,'X' ;check if it is Register BX
jnz IsItBHSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'H' ;check if it is Register BH
jnz IsItBLSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'L' ;check if it is Register BL
jz NoAddressErrorSRC9
jmp AddressErrorSRC
NoAddressErrorSRC9:
cmp player,1 ;check if player1 is entering this command
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
cmp operand2,'C'
jz IfitisnotDSRC
jmp IfitisDSRC
IfitisnotDSRC:
cmp operand2+1,'X' ;check if it is Register CX
jnz IsItCHSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'H' ;check if it is Register CH
jnz IsItCLSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'L' ;check if it is Register CL
jz NoAddressErrorSRC10
jmp AddressErrorSRC
NoAddressErrorSRC10:
cmp player,1 ;check if player1 is entering this command
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
cmp operand2,'D'
jz IsnotImmediateSRC
jmp IsImmediateSRC
IsnotImmediateSRC:
cmp operand2+1,'X' ;check if it is Register DX
jnz IsItDHSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'H' ;check if it is Register DH
jnz IsItDLSRC
cmp player,1 ;check if player1 is entering this command
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
cmp operand2+1,'L' ;check if it is Register DL
jnz IsImmediateSRC
cmp player,1 ;check if player1 is entering this command
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
    mov si,offset operand2
     ;;;;
     Str_Hex operand2, dummy
        mov cx, 0
        mov cx, dummy
     cmp ch,0h
     jnz SixteenbitImmediateSRC
     mov MemoryTypeSRC,1 ;Immediate value is 8 bits
     jmp Player1ImmediateSRC
     SixteenbitImmediateSRC:
     mov MemoryTypeSRC,2 ;immeduate value is 16 bits
     Player1ImmediateSRC:
     cmp player,1 ;assign Immediate value to immediate 2 and send its offset with si
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
cmp player,1 ;check who made the error and -1 his/her points
jnz Player2errSRC
sub Score1InDec,1
mov AddressTypeSRC,0
jmp endAddressCheckerSRC
Player2errSRC:
sub Score2InDec,1
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
mov MemoryTypeDEST,0
mov AddressTypeDEST,0
cmp operand1,'[' ;check if operand1 is an direct or an indirect memory
jz ISnotregDEST
jmp ISregDEST
ISnotregDEST:
cmp operand1+2,']' ;check if direct memory ---> direct memory is defined as [x] where x is on range of 0 to F
jz eeeeeeeeeeeee
jmp IsInDirectMemoryDEST
eeeeeeeeeeeee:
mov cx,0
MemoryAssignmentDEST:
; mov cl,operand1+1
; cmp cl,29h
; jg NoAddressErrorDEST1
; jmp AddressErrorDEST
; NoAddressErrorDEST1:
; cmp cl,39h
; jg StartfromAMemoryDEST
; sub cl,30h
; jmp Player1MemoryDEST
; StartfromAMemoryDEST:
; cmp cl,40h
; jg NoAddressErrorDEST2
; jmp AddressErrorDEST
; NoAddressErrorDEST2:
; cmp cl,47h
; jl NoAddressErrorDEST3
; jmp AddressErrorDEST
; NoAddressErrorDEST3:
; sub cl,37h
Str_Hex operand1, dummy
mov cx, dummy
Player1MemoryDEST:
cmp player,1
jnz Player2DirectMemoryDEST
mov di,offset mem2
add di,cx
cmp AddressTypeDEST,0
jz NotendAddressCheckerDEST1


mov MemoryTypeDEST,1
jmp endAddressCheckerDEST
NotendAddressCheckerDEST1:

mov AddressTypeDEST,3
mov MemoryTypeDEST,1
jmp endAddressCheckerDEST
Player2DirectMemoryDEST:
mov di,offset mem1
add di,cx
cmp AddressTypeDEST,0
jz NotendAddressCheckerDEST2
jmp endAddressCheckerDEST
NotendAddressCheckerDEST2:
mov AddressTypeDEST,3
jmp endAddressCheckerDEST
IsInDirectMemoryDEST: ;Use address inside each of those variables as offset to be delivered by si
cmp operand1+1,'B'
jnz IsSIDEST
cmp operand1+2,'X'
jz NoAddressErrorDEST4
jmp AddressErrorDEST
NoAddressErrorDEST4:
cmp player,1
jnz Player2BXIndirectDEST
mov cl,bl2+1
mov operand1+1,cl
mov operand1+2,']'
mov operand1+3,'$'
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
Player2BXIndirectDEST:
mov cl,bl1+1
mov operand1+1,cl
mov operand1+2,']'
mov operand1+3,'$'
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
IsSIDEST:
cmp operand1+1,'S'
jnz IsDIDEST
cmp operand1+2,'I'
jz NotAddressErrorDEST5
jmp AddressErrorDEST
NotAddressErrorDEST5:
cmp player,1
jnz Player2SIIndirectDEST
mov cl,si2+3
mov operand1+1,cl
mov operand1+2,']'
mov operand1+3,'$'
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
Player2SIIndirectDEST:
mov cl,si1+3
mov operand1+1,cl
mov operand1+2,']'
mov operand1+3,'$'
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
IsDIDEST:
cmp operand1+1,'D'
jz NoAddressErrorDEST6
jmp AddressErrorDEST
NoAddressErrorDEST6:
cmp operand1+2,'I'
jz NoAddressErrorDEST7
jmp AddressErrorDEST
NoAddressErrorDEST7:
cmp player,1
jnz Player2DIIndirectDEST
mov cl,di2+3
mov operand1+1,cl
mov operand1+2,']'
mov operand1+3,'$'
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
Player2DIIndirectDEST:
mov cl,di1+3
mov operand1+1,cl
mov operand1+2,']'
mov operand1+3,'$'
mov AddressTypeDEST,4
jmp MemoryAssignmentDEST
ISregDEST:         ;operand1 is a register
cmp operand1,'A'
jz IfitisnotBDEST
jmp IfitisBDEST
IfitisnotBDEST:
cmp operand1+1,'X' ;check if it is Register AX
jnz IsItAHDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'H' ;check if it is Register AH
jnz IsItALDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'L' ;check if it is Register AL
jz NoAddressErrorDEST8
jmp AddressErrorDEST
NoAddressErrorDEST8:
cmp player,1 ;check if player1 is entering this command
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
cmp operand1,'B'
jz IfitisnotCDEST
jmp IfitisCDEST
IfitisnotCDEST:
cmp operand1+1,'X' ;check if it is Register BX
jnz IsItBHDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'H' ;check if it is Register BH
jnz IsItBLDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'L' ;check if it is Register BL
jz NoAddressErrorDEST9
jmp AddressErrorDEST
NoAddressErrorDEST9:
cmp player,1 ;check if player1 is entering this command
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
cmp operand1,'C'
jz IfitisnotDDEST
jmp IfitisDDEST
IfitisnotDDEST:
cmp operand1+1,'X' ;check if it is Register CX
jnz IsItCHDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'H' ;check if it is Register CH
jnz IsItCLDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'L' ;check if it is Register CL
jz NoAddressErrorDEST10
jmp AddressErrorDEST
NoAddressErrorDEST10:
cmp player,1 ;check if player1 is entering this command
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
cmp operand1,'D'
jz NoAddressErrorDEST11
jmp AddressErrorDEST
NoAddressErrorDEST11:
cmp operand1+1,'X' ;check if it is Register DX
jnz IsItDHDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'H' ;check if it is Register DH
jnz IsItDLDEST
cmp player,1 ;check if player1 is entering this command
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
cmp operand1+1,'L' ;check if it is Register DL
jnz NoAddressErrorDEST12
jmp AddressErrorDEST
NoAddressErrorDEST12:
cmp player,1 ;check if player1 is entering this command
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
cmp player,1 ;check who made the error and -1 his/her points
jnz Player2errDEST
sub Score1InDec,1
mov AddressTypeDEST,0
jmp endAddressCheckerDEST
Player2errDEST:
sub Score2InDec,1
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
jz NotendAddressingMode1 ;error mn abl el function deeh msh lazem nen2so marteen
jmp endAddressingMode
NotendAddressingMode1:
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
cmp player,1 ;check who made the error and -1 his/her points
jnz Player2errMode
sub Score1InDec,1
jmp endAddressingMode
Player2errMode:
sub Score2InDec,1
endAddressingMode:
ret
AddressingmodeDesignator endp






GetInitianlPoints proc
    mov Sending, 0
    mov Receiving, 0
    mov displacment1, 0
    mov displacment2, 0
    mov ah, 0
    mov al,13h
    int 10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 1      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ah, 9
    mov dx, offset Promt2
    int 21h

SendInitialPoints:    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        jnz ReadyToSendPoints
        Jmp receivePoints 

ReadyToSendPoints:
     	mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jnz SavePoints
        jmp receivePoints
SavePoints:
        ;Remove char from buffer 
        mov ah, 0
        int 16h 
        ;Check if backspace, remove the letter
        cmp al, 8h
        je RemoveNumber     
        ;Check if enter was pressed
        cmp al, 0Dh
        je PrintPoints    ;print the whole message
        
        mov bx, displacment1
        cmp bx, 3
        jnz NotMaxlength3
        jmp receivePoints
        ;Else, add the char in the message         
NotMaxlength3:
        mov bx, displacment1
        lea si, SentPoints
        mov [si][bx], al 
        inc displacment1 
        mov messageoffset, si 
        
        mov  dl, column1   ;Column
        mov  dh, 4   ;Row
        mov  bh, 0   ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h

        
        ;mov  al, dl
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
        
        inc column1
        
        jmp receivePoints
RemoveNumber:  
        dec displacment1    
        mov bx, displacment1 
        lea si, SentPoints
        mov byte ptr [si][bx], 24h 
        dec column1
        mov  dl, column1   ;Column
        mov  dh, 4   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h
        
        mov  al, ' '
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
                                 
        jmp receivePoints

PrintPoints:  
;Check that Transmitter Holding Register is Empty     
        mov Sending, 1
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        JZ PrintPoints     ;Not empty 
  		
        ;print the message
        mov di, messageoffset
        mov al, byte ptr [di]
        cmp al, 24h
        je SendingEnd3
  		
        mov dx , 3F8H        
        out dx, al 
        
        inc messageoffset
        jmp PrintPoints
SendingEnd3: 
        mov dx , 3F8H
        mov al, 24h        
        out dx, al     ;Send $ to end sending 
        mov column1, 0
        mov displacment1, 0
        jmp CheckForEnd3

receivePoints:   
        ;Check that Data is Ready
	    mov dx , 3FDH		; Line Status Register
        in al , dx 
        test al , 1
        JNZ ReadyToReceive3
        JMP SendInitialPoints                                   ;Not Ready
 ;If Ready read the VALUE in Receive data register
ReadyToReceive3:
      	mov dx , 03F8H
        in al , dx
        
        cmp al, 24h
        je ReceiveEnded3

        mov bx, displacment2
        lea si, REceivedPoints
        mov [si][bx], al 
        inc displacment2 
        mov messageoffset, si 
        
        jmp SendInitialPoints  
ReceiveEnded3:
        mov Receiving, 1
        jmp CheckForEnd3

CheckForEnd3:
        cmp Sending, 1
        je PointSent
        jmp SendInitialPoints
PointSent:    
        cmp Receiving, 1
        je PointsReceived
        jmp receivePoints
PointsReceived:
    mov displacment2, 0
    ret
GetInitianlPoints ENDP


SelectInitialPoints proc
    ;;Check if the score is a valid number
    ConvertToUCL score1
    ScoreToDecimal Score1, Score1InDec
    ;;Check if the score is a valid number
    ConvertToUCL Score2
    ScoreToDecimal Score2, Score2InDec

    cmp Score1InDec, 100
    ja SetDefault1
    jmp CheckPlayer22

SetDefault1:
    mov Score1InDec, 100

CheckPlayer22:
    cmp Score2InDec, 100
    ja SetDefault2
    jmp CompareScores

SetDefault2:
    mov Score2InDec, 100

CompareScores:
    mov ax, Score1InDec
    mov bx, Score2InDec
    cmp ax, bx
    ja Score2Less
    mov Score2InDec, ax
    jmp SaveScoreStr
Score2Less:
    mov Score1InDec, bx

SaveScoreStr:
    ScoreToString Score1, Score1InDec
    ScoreToString Score2, Score2InDec

    ret
SelectInitialPoints ENDP


GetOpnum proc far
;Get the number of operands
;THIS CODE ASSUMES THAT THERE WERE NO ERRORS IN THE COMMAND PART ie. "ADD","SUB".. etc.
pusha

    mov bl,valid2Chk
    cmp bl,0        ; If bl=0, then command is invalid, and point has already
                    ; been deducted, then skip all checks.
    jnz ContChks     ; Continues the checks, if the command is valid so far
    jmp EGtOm

ContChks:
    mov DI, offset command   ; The variable containing the command



    cmp player, 1
    ; je Player1Input1Forbchk

elb3do:
;     ; player 2 is the one who typed the command
;     ; So the command must not contain the forbidden char written by player1 (ForbiddenChar1)
;     mov bl, ForbiddenChar1      ; forbiddenchar written by player 1
;     cmp bl,[DI]                 ; checks if the DI currently has the forbidden char
;     jz DedoPointo1              ; if it contains the forbidden char, deduce points

;     ; Check if the command has ended
;     mov bl,36                   ; ASCII of '$'
;     cmp bl,[DI]
;     jnz skipJumplel40VSOP2
;     jmp EndInput1Forbchk        ; Forbidden character wasn't used
; skipJumplel40VSOP2:

;     mov bl,13                   ; ASCII of 'Enter'
;     cmp bl,[DI]

;     jnz skipJumplel40VSOP2EN
;     jmp EndInput1Forbchk        ; Forbidden character wasn't used
; skipJumplel40VSOP2EN:

;     inc DI                      ; Checks the next letter
;     jmp elb3do

; DedoPointo1:
    ; jmp EGtOmNot                ; deducts point from the player

;     jmp EndInput1Forbchk        ; No forbidden character was used



; Player1Input1Forbchk:
;     ; player 1 is the one who typed the command
;     ; So the command must not contain the forbidden char written by player2 (ForbiddenChar2)
;     mov bl, ForbiddenChar2      ; forbiddenchar written by player 2
;     cmp bl,[DI]                 ; checks if the DI currently has the forbidden char
;     jz DedoPointo2              ; if it contains the forbidden char, deduce points

;     ; Check if the command has ended
;     mov bl,36                   ; ASCII of '$'
;     cmp bl,[DI]
;     jnz skipJumple2l40VSOP2
;     jmp EndInput1Forbchk        ; Forbidden character wasn't used
; skipJumple2l40VSOP2:

;     mov bl,13                   ; ASCII of 'Enter'
;     cmp bl,[DI]

;     jnz skipJumple2l40VSOP2EN
;     jmp EndInput1Forbchk        ; Forbidden character wasn't used
; skipJumple2l40VSOP2EN:

;     inc DI                      ; Checks the next letter
;     jmp Player1Input1Forbchk

; DedoPointo2:
;     jmp EGtOmNot                ; deducts point from the player

;     jmp EndInput1Forbchk        ; No forbidden character was used



EndInput1Forbchk:


mov al, opcode[0]




mov bl, 65          ; ASCII of 'A' ;handles add,adc
cmp bl,al
jz twoOCF


mov bl, 79          ; ASCII of 'O' ;handles add,adc
cmp bl,al
jz twoOCF



mov bl, 88          ; ASCII of 'X' ;handles add,adc
cmp bl,al
jz twoOCF


mov bl,77            ; ASCII of 'M' ;handles  SHR,SHL,SUB,SBB
cmp bl,al
jz Uchk


mov bl,83            ; ASCII of 'S' ;handles  SHR,SHL,SUB,SBB
cmp bl,al
jz hchk

mov bl, 82          ; ASCII of 'R' ;handles ROR
cmp bl,al
; jz twoOCS
jnz skRtwoOCS1
jmp twoOCS


skRtwoOCS1:



mov bl, 67          ; ASCII of 'C' ;handles CLC
cmp bl,al
; jz NOC
jnz skCNOCS1
jmp NOC


skCNOCS1:

mov bl, 78          ; ASCII of 'N' ;handles CLC
cmp bl,al
; jz NOC
jnz skCNOCS2
jmp NOC


skCNOCS2:


jmp OneOC



mov bl, 82          ; ASCII of 'R' ;handles ROR
cmp bl,al



Uchk:
mov al, opcode[1]
mov bl,85             ; ASCII of 'U' ;handles  MOV,MUL
cmp bl,al

;jz OneOC
jnz skOneOCS1
jmp OneOC    ;MUL


skOneOCS1:
jmp twoOCF  ;MOV








hchk:
mov al, opcode[1]
mov bl,72             ; ASCII of 'H' ;handles  SHR,SHL
cmp bl,al

; jz   twoOCS
jnz sktwoOCS1
jmp twoOCS    ;SHR,SHL


sktwoOCS1:
jmp twoOCF     ;SUB,SBB




twoOCF:
    TwoOpF command, operand1, operand2, valid2Chk
    mov bl,valid2Chk
    cmp bl,0        ; If bl=0, then command is invalid, and point has
                    ; to be deducted, then skip all checks and deduct point.
    jnz ContChks2   ; Continues the checks, if the command is valid so far
    jmp EGtOmNot
ContChks2:

call SourceAddressStringParser   ; bt2ra el source and puts the type in memtypeasrc

mov SourceOffset, si

call DestinationAddressStringParser  ; bt2ra el Dst and puts the type in memtypeaDST

mov DestinationOffset, di

call AddressingmodeDesignator    ;makes sure that when there are 2 operands you ...



jmp EGtOm

twoOCS:
; TwoOpS command, operand1, operand2
TwoOpS command, operand1, operand2, valid2Chk
    mov bl,valid2Chk
    cmp bl,0        ; If bl=0, then command is invalid, and point has
                    ; to be deducted, then skip all checks and deduct point.
    jnz ContChks3   ; Continues the checks, if the command is valid so far
    jmp EGtOmNot
ContChks3:
call SourceAddressStringParser   ; bt2ra el source and puts the type in memtypeasrc

mov SourceOffset, si

call DestinationAddressStringParser  ; bt2ra el Dst and puts the type in memtypeaDST

mov DestinationOffset, di


call AddressingmodeDesignator    ;makes sure that when there are 2 operands you ...

jmp EGtOm

OneOC:
; OneOp command, operand1, operand2
    OneOp command, operand1, operand2, valid2Chk
    mov bl,valid2Chk
    cmp bl,0        ; If bl=0, then command is invalid, and point has
                    ; to be deducted, then skip all checks and deduct point.
    jnz ContChks4   ; Continues the checks, if the command is valid so far
    jmp EGtOmNot
ContChks4:
call DestinationAddressStringParser  ; bt2ra el Dst and puts the type in memtypeaDST
mov DestinationOffset, di
jmp EGtOm

NOC:
; NoOp command, operand1, operand2
    NoOp command, operand1, operand2, valid2Chk
    mov bl,valid2Chk
    cmp bl,0        ; If bl=0, then command is invalid, and point has
                    ; to be deducted, then skip all checks and deduct point.
    jnz ContChks5   ; Continues the checks, if the command is valid so far
    jmp EGtOmNot
ContChks5:

; Having reached this point, the command is valid
    jmp Egtom



EGtOmNot:   ; Deducts point from score

    cmp player,1
    je Player1LosePointN
    dec Score2InDec
    ScoreToString Score2, Score2InDec
    jmp EndInterpretN
Player1LosePointN:
    dec Score1InDec
    ScoreToString Score1, Score1InDec
EndInterpretN:


EGtOm:  ; The end of the procedure

popa
ret

GetOpnum ENDP


TranslateAddressingModes proc
    mov al, MemoryTypeDEST
    mov operandsize, al

   cmp AddressTypeDEST, 2
   je DestIsReg
   mov DestMem, 1
   jmp CheckSourceType
DestIsReg: mov DestMem, 0

CheckSourceType:
   cmp AddressTypeSRC, 1
   je SourceIsImmed
   cmp AddressTypeSRC, 2
   jne SourceIsmemory
   jmp SourceIsReg
SourceIsmemory:
   mov SourceMem, 1
SourceIsImmed:

   mov SourceMem, 0
   ;;;Convert immediate value to string
   ;;;player1 --> immediatevalue2
   cmp Player, 1
   je ConvertImmed2
   ;;Convert immediate 1
   Hex_Str ImmediateValue1, ImmediateValueStr
    cmp operandsize, 1
    je LoadLeastSigByte
    lea si, ImmediateValueStr
    jmp AddItInvar
LoadLeastSigByte:
    lea si, ImmediateValueStr+2
AddItInvar:
    mov SourceOffset, si
    jmp EndOfTranslating

ConvertImmed2:
   Hex_Str ImmediateValue2, ImmediateValueStr
    cmp operandsize, 1
    je LoadLeastSigByte1
    lea si, ImmediateValueStr
    jmp AddItInvar1
LoadLeastSigByte1:
    lea si, ImmediateValueStr+2
AddItInvar1:
   mov SourceOffset, si
   jmp EndOfTranslating

SourceIsReg:
   mov SourceMem, 0

EndOfTranslating:
    ret
TranslateAddressingModes ENDP


CallCommandsProc proc


   call TranslateAddressingModes

   lea si, opcode
   mov CommandNo, 0

   ;Compare if it is OR first
   lea di, ORCommandstr
   mov cx, 2           ;length of command is 3
   REPE CMPSB          ;compare with the first command in the list
   je CallORCommand

   lea di, CommandList ;Start comparing commands from the list if it is not OR command
   lea si, opcode          ;reset the pointer to the beginning of the command line
CheckAgain:
   mov cx, 3           ;length of command is 3
   REPE CMPSB          ;compare with the first command in the list
   je CallCommands       ;if equal save the opcode

   inc CommandNo       ;else, increment command number

   ;Preparing the pointers for the next check
   lea si, opcode          ;reset the pointer to the beginning of the command line
   mov ax, 0
   mov al, CommandNo
   mov dl, 3
   MUL dl               ;Calculate the displacement to move the pointer to the next command in the list
   lea di, CommandList ;reset the pointer
   add di , ax         ;move 3*(number of commands checked) steps

   jmp CheckAgain           ;compare with the next command in the list

CallORCommand:
    ;mov si, SourceOffset
    mov di, DestinationOffset
    lea si, HexNum
    CALL OrCommand
    jmp CommandsCalledEnd

;;0 = 'ADD', 1 = 'ADC', 2 = 'SUB', 3 = 'SBB', 4 = 'DIV', 5 = 'MUL', 6 = 'MOV', 7 = 'NOP',8 = 'INC', 9 ='DEC', 10 = 'XOR',
; 11 = 'AND', 12 = 'SHR', 13 = 'SHL', 14 = 'CLC', 15 = 'ROR'
CallCommands:
    mov si, SourceOffset
    mov di, DestinationOffset
    cmp CommandNo, 0
    je CallAddCommand

    cmp CommandNo, 1
    je CallAdcCommand

    cmp CommandNo, 2
    je CallSubCommand

    cmp CommandNo, 3
    je CallSbbCommand

    cmp CommandNo, 4
    je CallDivCommand

    cmp CommandNo, 5
    je CallMulCommand

    cmp CommandNo, 6
    je CallMovCommand

    cmp CommandNo, 7
    je CallNopCommand

    cmp CommandNo, 8
    je CallIncCommand

    cmp CommandNo, 9
    je CallDecCommand

    cmp CommandNo, 10
    je CallXorCommand

    cmp CommandNo, 11
    je CallAndCommand

    cmp CommandNo, 12
    je CallShrCommand

    cmp CommandNo, 13
    je CallShlCommand

    cmp CommandNo, 14
    je CallClcCommand

    cmp CommandNo, 15
    je CallRorCommand


CallAddCommand:
    CALL AddCommand
    jmp CommandsCalledEnd

CallAdcCommand:
    CALL AdcCommand
    jmp CommandsCalledEnd

CallSubCommand:
    CALL SubCommand
    jmp CommandsCalledEnd

CallSbbCommand:
    CALL SbbCommand
    jmp CommandsCalledEnd

CallDivCommand:
    CALL DivCommand
    jmp CommandsCalledEnd

CallMulCommand:
    CALL MulCommand
    jmp CommandsCalledEnd

CallMovCommand:
    CALL MovCommand
    jmp CommandsCalledEnd

CallNopCommand:
    ;CALL NopCommand
    jmp CommandsCalledEnd

CallIncCommand:
    CALL IncCommand
    jmp CommandsCalledEnd

CallDecCommand:
    CALL DecCommand
    jmp CommandsCalledEnd

CallXorCommand:
    CALL XorCommand
    jmp CommandsCalledEnd

CallAndCommand:
    CALL AndCommand
    jmp CommandsCalledEnd

CallShrCommand:
    CALL ShrCommand
    jmp CommandsCalledEnd

CallShlCommand:
    CALL ShlCommand
    jmp CommandsCalledEnd

CallClcCommand:
    CALL ClcCommand
    jmp CommandsCalledEnd

CallRorCommand:
    CALL RorCommand
    jmp CommandsCalledEnd

CommandsCalledEnd:
    ret
CallCommandsProc ENDP



GetLevel proc

    cmp ISentTheInvitation, 1    
    je PromtToChooseL    
    jmp ReceiveLevel
PromtToChooseL:
    mov CurrentPlayer, 1    
    mov ah, 0
    mov al,13h
    int 10h

    mov ax, 0
    mov ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 1      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ah, 9
    mov dx, offset PromtL
    int 21h

sendLevel:    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        Jz sendLevel
CheckAgain1:
        mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jz CheckAgain1

        ;Remove char from buffer 
        mov ah, 0
        int 16h 

        cmp al, 31h
        je Level1Chosen
        cmp al, 32h
        je Level2Chosen
        jmp sendLevel

Level1Chosen:
        mov dx , 3F8H        
        out dx, al 
        mov level, 1
        ret
Level2Chosen:
        mov dx , 3F8H        
        out dx, al 
        mov level, 2
        ret

ReceiveLevel:
        mov CurrentPlayer, 2
        ;Check that Data is Ready
	mov dx , 3FDH		; Line Status Register
        in al , dx 
        test al , 1
        Jz ReceiveLevel

        mov dx , 03F8H
        in al , dx 

        cmp al, 31h
        je Level1Chosen1
;Level 2
        mov level, 2
        ShowLevel level, Promt8
        ret
Level1Chosen1:
        mov Level, 1
        ShowLevel level, Promt8
        ret
GetLevel ENDP


GetAx1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalax
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h

        mov ax, word ptr rreggs
        mov word ptr ax1,ax

    popa

    ret
GetAx1 ENDP

GetBx1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalbx
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr bx1,ax

    popa

    ret
Getbx1 ENDP



GetCx1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalcx
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr cx1,ax

    popa

    ret
Getcx1 ENDP


GetDx1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvaldx
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr dx1,ax

    popa

    ret
GetDx1 ENDP


GetSI1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalsi
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr si1,ax

    popa

    ret
GetSI1 ENDP

GetDI1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvaldi
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr di1,ax

    popa

    ret
GetDI1 ENDP



GetSP1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalsp
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr sp1,ax

    popa

    ret
GetSP1 ENDP



GetBP1 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn1
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalbp
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr bp1,ax

    popa

    ret
GetBP1 ENDP


GetRegs1 proc


    Call GetAx1
    Call GetBx1
    Call GetCx1
    Call GetDx1
    Call GetSI1
    Call GetDI1
    Call GetSP1
    Call GetBP1
    ret
GetRegs1 ENDP

GetAx2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX2
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalax
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h

        mov ax, word ptr rreggs
        mov word ptr ax2,ax

    popa

    ret
GetAx2 ENDP

GetBx2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalbx
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr bx2,ax

    popa

    ret
Getbx2 ENDP



GetCx2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalcx
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr cx2,ax

    popa

    ret
Getcx2 ENDP


GetDx2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvaldx
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr dx2,ax

    popa

    ret
GetDx2 ENDP


GetSI2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalsi
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr si2,ax

    popa

    ret
GetSI2 ENDP

GetDI2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvaldi
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr di2,ax

    popa

    ret
GetDI2 ENDP



GetSP2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalsp
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr sp2,ax

    popa

    ret
GetSP2 ENDP



GetBP2 proc
    pusha

    mov ah, 0
    mov al,13h
    int 10h

    ;AX1
        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 0      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset ppnn2
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 2      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promtvalbp
        int 21h


        mov ax, 0
        mov ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 4      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov ah, 0AH
        mov dx, offset bufferrregs
        int 21h
        mov ax,word ptr rreggs
        mov word ptr bp2,ax

    popa

    ret
GetBP2 ENDP


GetRegs2 proc


    Call GetAx2
    Call GetBx2
    Call GetCx2
    Call GetDx2
    Call GetSI2
    Call GetDI2
    Call GetSP2
    Call GetBP2
    ret
GetRegs2 ENDP




Chklst1 proc far
;wait for VR
pusha

regloss ax1,targetlol,lose1
regloss bx1,targetlol,lose1
regloss cx1,targetlol,lose1
regloss dx1,targetlol,lose1
regloss si1,targetlol,lose1
regloss di1,targetlol,lose1
regloss sp1,targetlol,lose1
regloss bp1,targetlol,lose1


popa
ret

Chklst1 ENDP



Chklst2 proc far
;wait for VR
pusha

regloss ax2, targetlol, lose2
regloss bx2, targetlol, lose2
regloss cx2, targetlol, lose2
regloss dx2, targetlol, lose2
regloss si2, targetlol, lose2
regloss di2, targetlol, lose2
regloss sp2, targetlol, lose2
regloss bp2, targetlol, lose2


popa

ret

Chklst2 ENDP


MainScreen proc far
        mov ax, 13h
        int 10h

        cmp F1PressedSender, 1
        je ReWriteChatInv
        cmp F1PressedReceiver, 1
        je ReWriteChatInv
        cmp F2PressedSender, 1
        je ReWriteGameInv
        cmp F2PressedReceiver, 1
        je ReWriteGameInv
        jmp DrawNotificationBar
ReWriteChatInv:         
        ChattingInvitation Promt7
        jmp DrawNotificationBar
ReWriteGameInv:
        GameInvitation Promt6
DrawNotificationBar:
        ;Draw Horizontal Lines
        mov cx, 0
        mov dx, 150
        mov al, 5
        mov ah, 0ch
        back91:   
        int 10h
        inc cx
        cmp cx, 320
        jnz back91

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 5      ;X coordinated
        mov dh, 9      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promt3
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 5      ;X coordinated
        mov dh, 12      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promt4
        int 21h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 5      ;X coordinated
        mov dh, 15      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promt5
        int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sendInvitation:    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        jnz ReadyToSendInvitation
        Jmp receiveInvitation                               ;Not empty

;If empty put the VALUE in Transmit data register
ReadyToSendInvitation:
     	mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jnz CheckKeyPressed
        jmp receiveInvitation
CheckKeyPressed:
        ;Remove char from buffer 
        mov ah, 0
        int 16h 

        ;F1 --> ah = 59
        ;F2 --> ah = 60
        ;F3 --> ah = 61
        ;ESC --> al = 1Bh

        cmp al, 1BH     ;ESC
        je Escape
        cmp ah, 59     ;F1
        je F1Chat
        cmp ah, 60     ;F2
        je F2Game
        jmp receiveInvitation
Escape:
        mov dx , 3F8H        
        out dx, al
        mov EscapePressed, 1
        ret
F1Chat:
        ChattingInvitation Promt7
        mov dx , 3F8H
        mov al, ah        
        out dx, al
        mov F1PressedSender, 1
        cmp F1PressedReceiver, 1
        je ChattingShouldStart
        jmp receiveInvitation
ChattingShouldStart:
        mov F1PressedSender, 0
        mov F1PressedReceiver, 0
        mov ChatModuleStart, 1
        ret
F2Game:
        GameInvitation Promt6
        mov dx , 3F8H 
        mov al, ah       
        out dx, al
        mov F2PressedSender, 1
        cmp F2PressedReceiver, 1
        je GameShouldStart
        mov ISentTheInvitation, 1 ;if F2PressedReceiver = 0, then i am the one who sent the invitation
        jmp receiveInvitation
GameShouldStart:
        mov F2PressedSender, 0
        mov F2PressedReceiver, 0
        mov GameModuleStart, 1
        ret

receiveInvitation:   
        ;Check that Data is Ready
	mov dx , 3FDH		; Line Status Register
        in al , dx 
        test al , 1
        JNZ ReadyToReceiveInvitation
        JMP sendInvitation                                   ;Not Ready
 ;If Ready read the VALUE in Receive data register
ReadyToReceiveInvitation:
      	mov dx , 03F8H
        in al , dx

        cmp al, 1BH
        je EscapeReceived
        cmp al, 59
        je F1Received
        cmp al, 60
        je F2Received
        jmp sendInvitation
EscapeReceived:
        mov EscapePressed, 1
        ret

F1Received:
        ChattingInvitation Promt7
        mov F1PressedReceiver, 1
        cmp F1PressedSender, 1
        je ChattingShouldStart1
        JMP sendInvitation
ChattingShouldStart1:
        mov F1PressedSender, 0
        mov F1PressedReceiver, 0
        mov ChatModuleStart, 1
        ret 

F2Received:
        GameInvitation Promt6
        mov F2PressedReceiver, 1
        cmp F2PressedSender, 1
        je GameShouldStart1
        JMP sendInvitation
GameShouldStart1:
        mov F2PressedSender, 0
        mov F2PressedReceiver, 0
        mov GameModuleStart, 1
        ret
        
MainScreen ENDP

InputNames proc far
        mov Sending, 0
        mov Receiving, 0
        mov ax, 13h
        int 10h

        mov ax, 0
        mov  ah, 02     ;SetCursorPosition
        mov dl, 1      ;X coordinated
        mov dh, 1      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ah, 9
        mov dx, offset Promt1
        int 21h

sendName:    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
AGAIN1:  In al , dx 			;Read Line Status
        test al , 00100000b
        jnz ReadyToSendName
        Jmp receiveName 

ReadyToSendName:
     	mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jnz SaveName
        jmp receiveName

       jmp receiveName
SaveName:
        ;Remove char from buffer 
        mov ah, 0
        int 16h 
        ;Check if backspace, remove the letter
        cmp al, 8h
        je RemoveLetter        
        ;Check if enter was pressed
        cmp al, 0Dh
        je PrintName    ;print the whole message
        
        mov bx, displacment1
        cmp bx, 15
        jnz NotMaxlength
        jmp receiveName
        ;Else, add the char in the message         
NotMaxlength:
        mov bx, displacment1
        lea si, SentName
        mov [si][bx], al 
        inc displacment1 
        mov messageoffset, si 
        
        mov  dl, column1   ;Column
        mov  dh, 4   ;Row
        mov  bh, 0   ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h

        
        ;mov  al, dl
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
        
        inc column1
        
        jmp receiveName
RemoveLetter:  
        dec displacment1    
        mov bx, displacment1 
        lea si, SentName
        mov byte ptr [si][bx], 24h 
        dec column1
        mov  dl, column1   ;Column
        mov  dh, 4   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h
        
        mov  al, ' '
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
                                 
        jmp receiveName

PrintName:  
;Check that Transmitter Holding Register is Empty     
        mov Sending, 1
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        JZ PrintName     ;Not empty 
  		
        ;print the message
        mov di, messageoffset
        mov al, byte ptr [di]
        cmp al, 24h
        je SendingEnd
  		
        mov dx , 3F8H        
        out dx, al 
        
        inc messageoffset
        jmp PrintName
SendingEnd: 
        mov Inputsize, 0 
        inc inputnumber 
        mov dx , 3F8H
        mov al, 24h        
        out dx, al     ;Send $ to end sending 
        mov column1, 0
        mov displacment1, 0
        jmp CheckForEnd

receiveName:   
        ;Check that Data is Ready
Next1:	mov dx , 3FDH		; Line Status Register
CHK1:    in al , dx 
        test al , 1
        JNZ ReadyToReceive
        JMP sendName                                   ;Not Ready
 ;If Ready read the VALUE in Receive data register
ReadyToReceive:
      	mov dx , 03F8H
        in al , dx
        
        cmp al, 24h
        je ReceiveEnded

        mov bx, displacment2
        lea si, ReceivedName
        mov [si][bx], al 
        inc displacment2 
        mov messageoffset, si 
        
        jmp sendName  
ReceiveEnded:
        mov Receiving, 1
        jmp CheckForEnd

CheckForEnd:
        cmp Sending, 1
        je NameSent
        jmp sendName
NameSent:    
        cmp Receiving, 1
        je NameReceived
        jmp receiveName
NameReceived:
        mov displacment2, 0
        ret
InputNames endp

ChatModule proc far
    mov ax, 13h
    int 10h     
    
    ;Draw Horizontal Lines
    mov cx, 0
    mov dx, 94
    mov al, 5
    mov ah, 0ch
back92:   
    int 10h
    inc cx
    cmp cx, 320
    jnz back92
    
    mov  dl, 0   ;Column
    mov  dh, 0   ;Row
    mov  bh, 0   ;Display page
    mov  ah, 02h  ;SetCursorPosition
    int  10h
    
    mov ah, 9
    mov dx, offset SentName
    int 21h
               
    mov  dl, 0   ;Column
    mov  dh, 12   ;Row
    mov  bh, 0   ;Display page
    mov  ah, 02h  ;SetCursorPosition
    int  10h 
    
    mov ah, 9
    mov dx, offset ReceivedName
    int 21h
              
;;;;;sender
send:    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        jnz ReadyToSend
        Jmp receive                               ;Not empty

;If empty put the VALUE in Transmit data register
ReadyToSend:
     	mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jnz SaveChar
        jmp receive
SaveChar:
        ;Remove char from buffer 
        mov ah, 0
        int 16h 
        ;Check if backspace, remove the letter
        cmp al, 8h
        je Remove        
        ;Check if enter was pressed
        cmp al, 0Dh
        jne NextCompare
        jmp Print    ;print the whole message
NextCompare:
        cmp ah, 61
        je ChattingEnd

        mov bx, displacment1
        cmp bx, 40
        jnz MaxSizeNotReached
        jmp receive

ChattingEnd:
        mov dx , 3F8H  
        mov al, ah      
        out dx, al
        mov Inputsize, 0 
        mov inputnumber, 0  
        mov column1, 0
        mov row1, 13
        mov displacment1, 0
        lea di, message1
        lea si, dummystr
        mov cx, 21
        REP MOVSW
        ret
        ;Else, add the char in the message         
MaxSizeNotReached:
        mov bx, displacment1
        lea si, message1
        mov [si][bx], al 
        inc displacment1 
        mov messageoffset, si 
        
        mov  dl, column1   ;Column
        mov  dh, row1   ;Row
        mov  bh, 0   ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h

        
        ;mov  al, dl
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
        
        inc column1
        
        jmp receive
Remove:  
        dec displacment1    
        mov bx, displacment1 
        lea si, message1
        mov byte ptr [si][bx], 24h 
        dec column1
        mov  dl, column1   ;Column
        mov  dh, row1   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h
        
        mov  al, ' '
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
                                 
        jmp receive
Print:  
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        JZ Print     ;Not empty 
  		
        ;print the message
        mov di, messageoffset
        mov al, byte ptr [di]
        cmp al, 24h
        je Reset
  		
        mov dx , 3F8H        
        out dx, al 
        
        inc messageoffset
        jmp print
Reset: 
        mov Inputsize, 0 
        inc inputnumber 
        mov dx , 3F8H
        mov al, 24h        
        out dx, al     ;Send $ to end sending     
        mov column1, 0
        inc row1
        mov displacment1, 0
        lea di, message1
        lea si, dummystr
        mov cx, 21
        REP MOVSW
        cmp inputnumber, 12
        je incrementpage
        jmp receive
incrementpage:
        ;inc pagenumber
;        mov ah, 06
;        mov al, 24
;        mov bh, 7
;        mov cx, 0
;        mov dx, 0
;        int 10h            
        jmp receive
    
;;;;;;receiver 
receive:   
        ;Check that Data is Ready
Next:	mov dx , 3FDH		; Line Status Register
CHK:    in al , dx 
        test al , 1
        JNZ Ready
        JMP send                                   ;Not Ready
 ;If Ready read the VALUE in Receive data register
Ready:
      	mov dx , 03F8H
        in al , dx
        
        cmp al, 24h
        je ResetValues
        
        cmp al, 61
        je chattingEnd1

        mov  dl, col  ;Column
        mov  dh, row2   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h
               
        mov  bl, 0Ch  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h   
        
        inc col
        jmp send  
ResetValues:
        mov col, 0
        inc row2            
        jmp send    

chattingEnd1:
        mov col, 0
        mov row2, 1 
        ret
ChatModule ENDP

SendReceiveRegCont proc far
    ;If you're player 1, you'll send first
    ;If you're player 2, you'll receive first
    cmp CurrentPlayer, 1
    je SendFIrst
    jmp ReceiveFirst
;;;;;;;;;;;;;;;;;;;;;;Player 1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SendFIrst:
;;Send contents of MP1
lea di, ah1
mov RegisterOffset, di
mov counter, 0
mov RegisterDisp, 0
;Check that Transmitter Holding Register is Empty     
;CheckIsEmpty:
        mov dx , 3FDH		; Line Status Register
CheckIsEmpty:
        In al , dx 			;Read Line Status
        test al , 00100000b
        Jz CheckIsEmpty

        mov di, RegisterOffset       
        mov al, byte ptr [di]
        mov dx , 3F8H        
        out dx, al
        inc di 
        mov RegisterOffset, di
        inc counter
        mov cx, counter
        cmp cx, 30
        je ThenReceive
        jmp CheckIsEmpty
ThenReceive:
;;Receive contents of MP2
lea di, ah2
mov RegisterOffset, di
mov counter, 0
    ;Check that Data is Ready
;CheckIfReady:
	    mov dx , 3FDH		; Line Status Register
CheckIfReady:
        in al , dx 
        test al , 1
        JZ CheckIfReady                                  ;Not Ready
 ;If Ready read the VALUE in Receive data register
      	mov dx , 03F8H
        in al , dx
        mov di, RegisterOffset 
        mov [di], al
        inc di 
        mov RegisterOffset, di
        inc counter
        mov cx, counter
        cmp cx, 30
        jne CheckIfReady
        ret
;;;;;;;;;;;;;;;;;;;;;;Player 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReceiveFirst:
;;Receive Contetnts of MP1
lea di, ah1
mov RegisterOffset, di
mov counter, 0
;Check that Data is Ready
;CheckIfReady1:
	    mov dx , 3FDH		; Line Status Register
CheckIfReady1:
        in al , dx 
        test al , 1
        JZ CheckIfReady1                                  ;Not Ready
 ;If Ready read the VALUE in Receive data register
      	mov dx , 03F8H
        in al , dx
        mov di, RegisterOffset
        mov [di], al
        inc di 
        mov RegisterOffset, di
        inc counter
        mov cx, counter
        cmp cx, 30
        je ThenSend
        jmp CheckIfReady1
ThenSend:
;;Send contents of MP2
lea di, ah2 
mov RegisterOffset, di
mov counter, 0
;Check that Transmitter Holding Register is Empty     
;CheckIsEmpty1:
        mov dx , 3FDH		; Line Status Register
CheckIsEmpty1:
        In al , dx 			;Read Line Status
        test al , 00100000b
        Jz CheckIsEmpty1

        mov di, RegisterOffset
        mov al, byte ptr [di]
        mov dx , 3F8H        
        out dx, al
        inc di 
        mov RegisterOffset, di
        inc counter
        mov cx, counter
        cmp cx, 30
        jne CheckIsEmpty1
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SendReceiveRegCont ENDP



InlineChat proc far
mov displacment1, 0
mov displacment2, 0
mov col, 0
mov column1, 0
mov dummyyyyy, 0
mov dummyyyyy2, 0
      
;;;;;sender
sendI:    
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        jnz ReadyToSendI
        Jmp receiveI                               ;Not empty

;If empty put the VALUE in Transmit data register
ReadyToSendI:
     	mov dx , 3F8H		; Transmit data register         
        mov ah, 1
        int 16h
        jnz SaveCharI
        jmp receiveI

SaveCharI:
        cmp dummyyyyy, 1
        jne ppppp
        mov  ah, 02h    ;SetCursorPosition
        mov dl, 0      ;X coordinated
        mov dh, 47      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov dx, offset spaces2 
        mov ah,9h
        int 21h
        mov dummyyyyy, 0
ppppp:
        ;Remove char from buffer 
        mov ah, 0
        int 16h 
        ;Check if backspace, remove the letter
        cmp al, 8h
        je RemoveI
        ;Check if enter was pressed
        cmp al, 0Dh
        jne NextCompareI
        jmp PrintI    ;print the whole message
NextCompareI:
        cmp ah, 68
        je ChattingEndI

        mov bx, displacment1
        cmp bx, 40
        jnz MaxSizeNotReachedI
        jmp receiveI

ChattingEndI:
        mov dx , 3F8H  
        mov al, ah      
        out dx, al
        mov Inputsize, 0 
        mov inputnumber, 0  
        mov column1, 0
        mov row1, 13
        mov displacment1, 0
        lea di, message1
        lea si, dummystr
        mov cx, 21
        REP MOVSW
        ret
        ;Else, add the char in the message         
MaxSizeNotReachedI:
        mov bx, displacment1
        lea si, message1
        mov [si][bx], al 
        inc displacment1 
        mov messageoffset, si 
        
        mov  dl, column1   ;Column
        mov  dh, 47   ;Row
        mov  bh, 0   ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h

        
        ;mov  al, dl
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
        
        inc column1
        
        jmp receiveI
RemoveI:  
        dec displacment1    
        mov bx, displacment1 
        lea si, message1
        mov byte ptr [si][bx], 24h 
        dec column1
        mov  dl, column1   ;Column
        mov  dh, 47   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h
        
        mov  al, ' '
        mov  bl, 0ACh  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h
                                 
        jmp receiveI
PrintI:  
;Check that Transmitter Holding Register is Empty     
        mov dx , 3FDH		; Line Status Register
        In al , dx 			;Read Line Status
        test al , 00100000b
        JZ PrintI     ;Not empty 
  		
        ;print the message
        mov di, messageoffset
        mov al, byte ptr [di]
        cmp al, 24h
        je ResetI
  		
        mov dx , 3F8H        
        out dx, al 
        
        inc messageoffset
        jmp printI
ResetI: 
        mov dummyyyyy, 1
        mov Inputsize, 0 
        inc inputnumber 
        mov dx , 3F8H
        mov al, 24h        
        out dx, al     ;Send $ to end sending     
        mov column1, 0
        inc row1
        mov displacment1, 0
        lea di, message1
        lea si, dummystr
        mov cx, 21
        REP MOVSW
        cmp inputnumber, 12
        je incrementpageI
        jmp receiveI
incrementpageI:
        ;inc pagenumber
;        mov ah, 06
;        mov al, 24
;        mov bh, 7
;        mov cx, 0
;        mov dx, 0
;        int 10h            
        jmp receiveI
    
;;;;;;receiver 
receiveI:   
        ;Check that Data is Ready
	    mov dx , 3FDH		; Line Status Register
        in al , dx 
        test al , 1
        JNZ ReadyI
        JMP sendI                                   ;Not Ready
 ;If Ready read the VALUE in Receive data register
ReadyI:
      	mov dx , 03F8H
        in al , dx
        
        cmp dummyyyyy2, 1
        jne ppppppp
        mov  ah, 02h    ;SetCursorPosition
        mov dl, 0      ;X coordinated
        mov dh, 49      ;Ycoordinated
        mov bx, 0
        int 10h

        mov ax, 0
        mov dx, offset spaces2 
        mov ah,9h
        int 21h
        mov dummyyyyy2, 0
ppppppp:

        cmp al, 24h
        je ResetValuesI
        
        cmp al, 68
        je chattingEnd1I

        mov  dl, col  ;Column
        mov  dh, 49   ;Row
        mov  bh, 0    ;Display page
        mov  ah, 02h  ;SetCursorPosition
        int  10h
               
        mov  bl, 0Ch  ;Color is red
        mov  bh, 0    ;Display page
        mov  ah, 0Eh  ;Teletype
        int  10h   
        
        inc col
        jmp sendI  
ResetValuesI:
        mov dummyyyyy2, 1
        mov col, 0
        inc row2            
        jmp sendI    

chattingEnd1I:
        mov col, 0
        mov row2, 1 
        ret
InlineChat ENDP




main proc far
    mov ax, @data
    mov ds, ax
    mov es, ax


;//////////////////////////Initialization////////////////////////////////
    
    mov dx,3fbh 			; Line Control Register
    mov al,10000000b		;Set Divisor Latch Access Bit
    out dx,al				;Out its
   
    mov dx,3f8h			
    mov al,0ch			
    out dx,al
    
    mov dx,3f9h
    mov al,00h
    out dx,al
    
    mov dx,3fbh
    mov al,00011011b 
    out dx,al

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    CALL InputNames   
    CALL GetInitianlPoints   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ReturnToMainScreen:
    Call MainScreen      
        
    cmp ChatModuleStart, 1   
    je StartChatting
    cmp EscapePressed, 1 
    je ProgramEnded 
    cmp GameModuleStart, 1
    je StartGame

StartChatting:
    mov ChatModuleStart, 0    
    CALL ChatModule
    jmp ReturnToMainScreen
        
ProgramEnded:
    mov ax, 13h
    int 10h
    mov ah,4ch
    int 21h     
    hlt


StartGame:
    cmp ISentTheInvitation, 1   
    je Name1IsSent
    lea si, SentName
    lea di ,Name2
    mov cx, 8
    REP MOVSW
    lea si, ReceivedName
    lea di ,Name1
    mov cx, 8
    REP MOVSW
    jmp CheckPoints
Name1IsSent:
    lea si, SentName
    lea di ,Name1
    mov cx, 8
    REP MOVSW
    lea si, ReceivedName
    lea di ,Name2
    mov cx, 8
    REP MOVSW 

CheckPoints:    
    cmp ISentTheInvitation, 1   
    je Points1IsSent
    lea si, SentPoints
    lea di ,Score2
    mov cx, 2
    REP MOVSW
    lea si, ReceivedPoints
    lea di ,Score1
    mov cx, 2
    REP MOVSW
    jmp LetUserChooseLevel
Points1IsSent:
    lea si, SentPoints
    lea di ,Score1
    mov cx, 2
    REP MOVSW
    lea si, ReceivedPoints
    lea di , Score2
    mov cx, 2
    REP MOVSW 

    ;ShowRect 0,0,320,200,0
    ;CALL Interface
     ;;;;Write Player Names:;;;;;;;
    ;WriteName Name1, player
    ;inc player
    ;WriteName Name2, player
    ;dec player
    ;;;;Write Player Scores:;;;;;;;
    ;WriteScore Score1, player
    ;inc player
    ;WriteScore Score2, player
    ;dec player
    ;CALL UpdateRegMem
    ;CALL InlineChat

LetUserChooseLevel:       
    ;m4 mot2kd mn el tarteeb!
    Call GetLevel

    cmp Level,2   ;if level2 get reggs
    jz GRR    ;Get Regs
    jmp skGrr

    ;GetInitial
    GRR:
    cmp CurrentPlayer, 1
    je InitializeMP1
    Call GetRegs2
    CALL SendReceiveRegCont
    jmp skGrr
InitializeMP1:    
    Call GetRegs1
    CALL SendReceiveRegCont


    skGrr:

Call GetForbid

    cmp ISentTheInvitation, 1   
    je FC1IsSent
    lea si, SentForbChar
    lea di ,ForbiddenChar2
    mov cx, 2
    REP MOVSB
    lea si, ReceivedForbChar
    lea di ,ForbiddenChar1
    mov cx, 2
    REP MOVSB
    jmp SelecttheInitialScores
FC1IsSent:
    lea si, SentForbChar
    lea di ,ForbiddenChar1
    mov cx, 2
    REP MOVSB
    lea si, ReceivedForbChar
    lea di , ForbiddenChar2
    mov cx, 2
    REP MOVSB

SelecttheInitialScores:
    ConvertToUCL ForbiddenChar1
    ConvertToUCL ForbiddenChar2
    
    Call SelectInitialPoints





    mov ah, 0
    mov al,13h
    int 10h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;Write Player Names:;;;;;;;
    WriteName Name1, player
    inc player
    WriteName Name2, player
    dec player
    ;;;;Write Player Scores:;;;;;;;
    WriteScore Score1, player
    inc player
    WriteScore Score2, player
    dec player


    ;Show forbidden characters if level 1
    cmp level, 1
    je ShowFC
    jmp TakeLevel2Input
ShowFC:
    ;Show forbidden characters
    jmp BeforeGameLoop

TakeLevel2Input:
    ;;;; take initial values of registers

BeforeGameLoop:
    ;;;;;;;;; Draw game interface;;;;
    ;BackGround
    CALL Interface
    CALL UpdateRegMem



    ;////////////////////////////////Game Loop//////////////////////////////////
    ;;;;;; starts with player 1
GameLoop:
    cmp level, 1
    je Level1
    jmp Level2
Level1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Level 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;; Take input from user
    ;;;; Check if it is power up --> if yes execute it
    ;;;; Take Command from the user
    ;;;; interpret and execute it
    ShowRect 0,0,320,200,0

    WriteChar ForbiddenChar1 ,10,0
    WriteChar ForbiddenChar2 ,31,0
    ;BackGround
    ;;;;Write Player Names:;;;;;;;
    WriteName Name1, DummyPlayerNum
    inc DummyPlayerNum
    WriteName Name2, DummyPlayerNum
    dec DummyPlayerNum
    ;;;;Write Player Scores:;;;;;;;
    WriteScore Score1, DummyPlayerNum
    inc DummyPlayerNum
    WriteScore Score2, DummyPlayerNum
    dec DummyPlayerNum

    ;;;;;;;;; Draw game interface;;;;
    CALL Interface
    CALL UpdateRegMem
;     cmp xxxx, 0
;     je gg
;     jne qq
;  gg:
;     CALL InlineChat
;     mov xxxx, 4  
;     qq: 
    CALL Read1
    Erase spaces
    ConvertToUCL command
    CALL Interpret

    ;//////////////////////Aly's + Youssef's Part
    ;Get operation number, after that we will have both of op1,op2 ready.

    call GetOpnum    ;

    ;now op1,op2 are ready

    ;///////////////////////////Samiha's Part


    CALL CallCommandsProc



    jmp CheckWinner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Level 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Level2:
    ;;;; Take input from user
    ;;;; Check if it is power up --> if yes execute it
    ;;;; Take Command from the user
    ;;;; Choose MP
    ;;;; interpret and execute command

    ShowRect 0,0,320,200,0

    ;BackGround
    ;;;;Write Player Names:;;;;;;;
    WriteName Name1, DummyPlayerNum
    inc DummyPlayerNum
    WriteName Name2, DummyPlayerNum
    dec DummyPlayerNum
    ;;;;Write Player Scores:;;;;;;;
    WriteScore Score1, DummyPlayerNum
    inc DummyPlayerNum
    WriteScore Score2, DummyPlayerNum
    dec DummyPlayerNum

    ;;;;;;;;; Draw game interface;;;;
    CALL Interface
    CALL UpdateRegMem

    CALL Read1
    Erase spaces
    ConvertToUCL command
    CALL Interpret



    ;decides which processor to execute the command on
    mov al, CurrentPlayer
    cmp player, al
    jne ReceiveMP
    jmp chooseMP

    chooseMP:
    mov al, player

    mov pplayer, al

    Call GetMpp

    jmp  cmpltGL



    ReceiveMP:




    RecMP: ;Recieve the MP
    
    ;Check that Data is Ready
    mov dx , 3FDH		; Line Status Register
    in al , dx 
    test al , 1
    jz RecMP                                 ;Not Ready
 ;If Ready read the VALUE in Receive data register
      	mov dx , 03F8H
        in al , dx
        
        cmp al, 1      ; it will be either 1 or 2
        je RecMPEnd
        cmp al,2
        je RecMPEnd


        jmp RecMP  
RecMPEnd:
        






    mov al, chr
    mov player, al 

    ;//////////////////////Aly's + Youssef's Part
    ;Get operation number, after that we will have both of op1,op2 ready.

    cmpltGL:

    call GetOpnum    ;

    ;now op1,op2 are ready

    ;///////////////////////////Samiha's Part

    CALL CallCommandsProc
    mov al, pplayer
    mov player, al

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Check Winner ;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckWinner:
    call ResetVar
    ;;;;;; Check if Player pressed F4
    ;;;;;; If yes quit game and show scores
    ;;;;;; Else Read command of the other player



    add Trnsnmbr,1    ;increase turns number
    push ax
    mov ax, TrnsBMG    ;turns before minigame
    cmp ax, Trnsnmbr
    pop ax
    jz CMG    ;call minigame
    jmp skCMG

    CMG:

    mov al, CurrentPlayer
    cmp player, al
    jne RecBMG
    jmp begMG


    RecBMG: ;Recieve the beginning of MG
    
    ;Check that Data is Ready
    mov dx , 3FDH		; Line Status Register
    in al , dx 
    test al , 1
    jz RecBMG                                 ;Not Ready
 ;If Ready read the VALUE in Receive data register
      	mov dx , 03F8H
        in al , dx
        
        cmp al, 1   ;the thing will be 1 
        je RecBMGEnd

        jmp RecBMG  
RecBMGEnd:
        jmp begMG


    begMG: ;begin minigame

    ;we need to send for the start of the minigame


    call minigame
    skCMG:


    call Chklst1
    call Chklst2

    cmp score1,0 
    jz loss1

    cmp score2,0 
    ; jz loss2
    jnz skls
    jmp loss2


    skls:

    ;check if any of them lost

    cmp lose1,1
    ; jz loss1
    jnz skls1
    jmp loss1
    skls1:

    cmp lose2,1
    ; jz loss2
    jnz skls2
    jmp loss2
    skls2:

    cmp player, 1
    je IncrementPlayer
    jne DecrementPlayer
IncrementPlayer:
    inc player
    Jmp GameLoop
DecrementPlayer:
    dec player
    Jmp GameLoop

    ShowRect 0,0,320,200,0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
loss1:
ShowRect 0,0,320,200,0
mov ah, 0
    mov al,13h
    int 10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 1      ;Ycoordinated
    mov bx, 0
    int 10h

    mov ah, 9
    mov dx, offset PromtLS1
    int 21h
jmp trunhaya

loss2:
ShowRect 0,0,320,200,0

mov ah, 0
    mov al,13h
    int 10h

    mov ax, 0
    mov  ah, 02     ;SetCursorPosition
    mov dl, 1      ;X coordinated
    mov dh, 1      ;Ycoordinated
    mov bx, 0
    int 10h
    mov ah, 9
    mov dx, offset PromtLS2
    int 21h
jmp trunhaya

endgmods:
    ShowRect 0,0,320,200,0


trunhaya:
;wait for 5 seconds
MOV     CX, 0FH
MOV     DX, 4240H
MOV     AH, 86H
INT     15H

MOV     CX, 0FH
MOV     DX, 4240H
MOV     AH, 86H
INT     15H

MOV     CX, 0FH
MOV     DX, 4240H
MOV     AH, 86H
INT     15H

MOV     CX, 0FH
MOV     DX, 4240H
MOV     AH, 86H
INT     15H

MOV     CX, 0FH
MOV     DX, 4240H
MOV     AH, 86H
INT     15H


;edit here

jmp ReturnToMainScreen

mov ah,4ch
int 21h



hlt
main ENDP
End main
