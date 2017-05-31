	   org 6000H
ATTRP	EQU 5C8DH	    ;SYSTEM PAPER 
ROMCLS	EQU 0D6BH	    ;ROM CLS Routine
ROMBDR	EQU 229BH	    ;ROM BORDER Routine

start   ld hl,ATTRP	    ;Set the PAPER and BORDER to Black, ink to bright white
	ld (hl),71
	xor a
	call ROMBDR
	call ROMCLS

	ld a,2              ; upper screen
        call 5633           ; open channel
	call newfont	    ; Define new font
loop    ld de,string        ; address of string
        ld bc,eostr-string  ; length of string to print
        call 8252           ; print our string
        ;jp loop            ; repeat until screen is full

        ld bc,(encar1)
        ld a,c
        add a,20
        ld c,a
        ld (encar1),bc

 sploo1 ld b,120
 
 sploop ld hl,spritedata
 	push bc
 	push hl
	ld bc,(playx)    
        call sprite		;print sam
        ld bc,(encar1)
        ld hl,spritedata+288
        call sprite		;print bird
        halt
        pop hl
 	ld bc,(playx)
        call sprite		;clear sam
        ld hl,spritedata+288
        ld bc,(encar1)
        call sprite		;clear bird

        halt
        ld hl,spritedata+32
        ld bc,(playx)
        inc b
        inc b
        ld (playx),bc
        call sprite		;move and print sam

        ld hl,spritedata+288
        ld bc,(encar1)
        inc b
        inc b
        ld (encar1),bc
        call sprite		;move and print bird

        halt
        ld hl,spritedata+32
        ld bc,(playx)
        call sprite
        ld hl,spritedata+288
        ld bc,(encar1)
        call sprite
        pop bc
        halt
        djnz sploop

   	ld b,120
 
 sploo2 ld hl,spritedata+128
 	push bc
 	push hl
	ld bc,(playx)    
        call sprite			;print sam
        ld hl,spritedata+352
        ld bc,(encar1)
        call sprite			;print bird
        halt
        pop hl
 	ld bc,(playx)
        call sprite			; clear sam
        ld hl,spritedata+352
        ld bc,(encar1)
        call sprite			;clear bird
        ld hl,spritedata+160
        ld bc,(playx)
        dec b
        dec b
        ld (playx),bc
        halt
        call sprite			;move and print sam
        ld hl,spritedata+352
        ld bc,(encar1)
        dec b
        dec b
        ld (encar1),bc
        call sprite			;move and print bird
        halt
        ld hl,spritedata+160
        ld bc,(playx)
        call sprite			;clear sam
        ld hl,spritedata+352
        ld bc,(encar1)
        call sprite			;clear bird
        pop bc
        halt
        djnz sploo2
        jp sploo1
        ret

string  defb 22,10,1	    ; @ 10,13
	defb 16, 3	    ; Magenta Ink
	defb 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
	defb 22,14,1
	defb 16, 5
	defb 'abcdefghijklmnopqurstuvwxyz'
	defb 22, 8 ,1
	defb '0123456789!"#$%&()*+,-./:;<=>?'
eostr  equ $

; This is the sprite routine and expects coordinates in (c ,b) form,
; where c is the vertical coord from the top of the screen (0-176), and
; b is the horizontal coord from the left of the screen (0 to 240).
; Sprite data is stored as you'd expect in its unshifted form as this
; routine takes care of all the shifting itself.  This means that sprite
; handling isn't particularly fast but the graphics only take 1/8th of the
; space they would require in pre-shifted form.

; On entry HL must point to the unshifted sprite data.

sprit7 xor 7               ; complement last 3 bits.
       inc a               ; add one for luck!
sprit3 rl d                ; rotate left...
       rl c                ; ...into middle byte...
       rl e                ; ...and finally into left character cell.
       dec a               ; count shifts we've done.
       jr nz,sprit3        ; return until all shifts complete.

; Line of sprite image is now in e + c + d, we need it in form c + d + e.

       ld a,e              ; left edge of image is currently in e.
       ld e,d              ; put right edge there instead.
       ld d,c              ; middle bit goes in d.
       ld c,a              ; and the left edge back into c.
       jr sprit0           ; we've done the switch so transfer to screen.

sprite ld (dispx),bc       ; store coords in dispx for now.
       call scadd          ; calculate screen address.
       ld a,16             ; height of sprite in pixels.
sprit1 ex af,af'           ; store loop counter.
       push de             ; store screen address.
       ld c,(hl)           ; first sprite graphic.
       inc hl              ; increment poiinter to sprite data.
       ld d,(hl)           ; next bit of sprite image.
       inc hl              ; point to next row of sprite data.
       ld (sprtmp),hl      ; store it for later.
       ld e,0              ; blank right byte for now.
       ld a,b              ; b holds y position.
       and 7               ; how are we straddling character cells?
       jr z,sprit0         ; we're not straddling them, don't bother shifting.
       cp 5                ; 5 or more right shifts needed?
       jr nc,sprit7        ; yes, shift from left as it's quicker.
       and a               ; oops, carry flag is set so clear it.
sprit2 rr c                ; rotate left byte right...
       rr d                ; ...through middle byte...
       rr e                ; ...into right byte.
       dec a               ; one less shift to do.
       jr nz,sprit2        ; return until all shifts complete.
sprit0 pop hl              ; pop screen address from stack.
       ld a,(hl)           ; what's there already.
       xor c               ; merge in image data.
       ld (hl),a           ; place onto screen.
       inc l               ; next character cell to right please.
       ld a,(hl)           ; what's there already.
       xor d             ; merge with middle bit of image.
       ld (hl),a           ; put back onto screen.
       inc l               ; next bit of screen area.
       ld a,(hl)           ; what's already there.
       xor e              ; right edge of sprite image data.
       ld (hl),a           ; plonk it on screen.
       ld a,(dispx)        ; vertical coordinate.
       inc a               ; next line down.
       ld (dispx),a        ; store new position.
       and 63              ; are we moving to next third of screen?
       jr z,sprit4         ; yes so find next segment.
       and 7               ; moving into character cell below?
       jr z,sprit5         ; yes, find next row.
       dec l               ; left 2 bytes.
       dec l               ; not straddling 256-byte boundary here.
       inc h               ; next row of this character cell.
sprit6 ex de,hl            ; screen address in de.
       ld hl,(sprtmp)      ; restore graphic address.
       ex af,af'           ; restore loop counter.
       dec a               ; decrement it.
       jp nz,sprit1        ; not reached bottom of sprite yet to repeat.
       ret                 ; job done.
sprit4 ld de,30            ; next segment is 30 bytes on.
       add hl,de           ; add to screen address.
       jp sprit6           ; repeat.
sprit5 ld de,63774         ; minus 1762.
       add hl,de           ; subtract 1762 from physical screen address.
       jp sprit6           ; rejoin loop.

; This routine returns a screen address for (c, b) in de.

scadd  ld a,c              ; get vertical position.
       and 7               ; line 0-7 within character square.
       add a,64            ; 64 * 256 = 16384 (Start of screen display)
       ld d,a              ; line * 256.
       ld a,c              ; get vertical again.
       rrca                ; multiply by 32.
       rrca
       rrca
       and 24              ; high byte of segment displacement.
       add a,d             ; add to existing screen high byte.
       ld d,a              ; that's the high byte sorted.
       ld a,c              ; 8 character squares per segment.
       rlca                ; 8 pixels per cell, mulplied by 4 = 32.
       rlca                ; cell x 32 gives position within segment.
       and 224             ; make sure it's a multiple of 32.
       ld e,a              ; vertical coordinate calculation done.
       ld a,b              ; y coordinate.
       rrca                ; only need to divide by 8.
       rrca
       rrca
       and 31              ; squares 0 - 31 across screen.
       add a,e             ; add to total so far.
       ld e,a              ; hl = address of screen.
       ret

playi  equ $               ; intended direction when turn is possible.
playd  equ playi+1         ; player's current direction.
nplayd equ playd+1         ; next player direction.
playx  equ nplayd+1        ; player x.
playy  equ playx+1         ; player's y coordinate.

encar1 equ playy+1         ; enemy car 1.
encar2 equ encar1+3        ; enemy car 2.
dispx  equ encar2+3        ; general-use coordinates.
dispy  equ dispx+1
seed   equ dispy+1         ; random number seed.
sprtmp equ seed+2          ; sprite temporary address.
termin equ sprtmp+2        ; end of variables.

blank	DS 32,0
fontdata 	EQU 60000
spritedata	EQU 61000
newfont:        ld hl,fontdata-256     ; font minus 32*8.
       		ld (23606),hl       ; point to new font.
       		ret
       		ORG fontdata
       		INCBIN "shortsfuse-font.bin"
       		ORG spritedata
       		INCBIN "shortsfuse-sprites.bin"

END start