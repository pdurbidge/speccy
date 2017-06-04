	   org 6000H
ATTRP		EQU 5C8DH	    ;SYSTEM PAPER 
ROMCLS		EQU 0D6BH	    ;ROM CLS Routine
ROMBDR		EQU 229BH	    ;ROM BORDER Routine
VISIBLE		EQU 2		    ;OLD FORMAT SPRITES FLAGS - 1=visible, 0=X_MSB
FLAGS		EQU 0		    ;NEW FORMAT SPRITES FLAGS - 0-7=palette_offset, 3=x_mirror, 2=y_mirror, 1=rotate, 0=X_MSB
DATABLKSZ	EQU 6
			    ;	
SPR_VISIBLE EQU 128	    ;NEW FORMAT SPRITES - VISIBLE ATTRIBUTE GOES WITH PATTERN
SPR_INVISIBLE EQU 127	    ;NEW FORMAT SPRITES - RESET VISIBLE BIT WHILE LEAVING PATTERN INTACT
start   ld hl,ATTRP	    ;Set the PAPER and BORDER to Black, ink to bright white
	ld (hl),71
	ld a,0
	call ROMBDR
	call ROMCLS



        call loadspimages   ; load the sprite image data
        call initspdata     ; initialise the sprite info blocks to 0
        ;call startspdata    ; set start positions of sprites (OLD FORMAT SPRITES)
       call newstartspdata ; set start positions of sprites (NEW FORMAT SPRITES)
        call displaysp	    ; display the sprites

loop	jp mp1
	ld ix,spdata
	ld h,(ix+0)	    ;increase x pos of sprite 0 and reset to 0 when it hits 320
	ld l,(ix+1)
	inc hl
	ld a,h
	and 1
	ld h,a
	jr z,lt255
	ld a,l
	cp 3fh
	jr c,lt255
	ld hl,0
lt255	ld (ix+0),h
	ld (ix+1),l
	ld bc,DATABLKSZ	   ;now deal with sprite 1
	add ix,bc
	ld h,(ix+0)
	ld l,(ix+1)
	dec hl
	ld a,h
	or l
	jr nz, lab1
	ld hl,304
lab1	ld (ix+0),h
	ld (ix+1),l
	add ix,bc	   ;now deal with sprite 2
	ld a,(ix+3)
	inc a
	cp 208
	jr c,lab2
	xor a
lab2	ld (ix+3),a	
	add ix,bc	   ;now deal with sprite 3
	ld a,(ix+3)
	dec a
	jr nz, lab3
	ld a,208
lab3	ld (ix+3),a

mp1	ld ix,p1data	   ;now deal with PLAYER 1

nota	
	ld bc,57342
	in a,(c)
	rra
	push af
	jr c, notp	  ;P not pressed so jump
	ld a,(ix+1)
	inc a
	inc a
	cp 250
	jr z,notp	  ;cant move more right so jump
	ld (ix+1),a
notp
	pop af
	rra
	jr c, noto	  ;O not pressed so jump
	ld a,(ix+1)
	dec a
	dec a
	cp 50
	jr c,noto	  ;cant move more left so jump
	ld (ix+1),a
noto
	ld bc,32766	  ; Check whether the fire button is pressed (space)
	in a,(c)
	rra
	push af
	jr c, notsp	  ; nope space not pressed so jump

	ld a,(p1fire)	  ;space was pressed, so check if missile is already active
	cp 1
	jp z, notsp 	  ; missile is already moving so ignore this key press
	ld ix,p1missile	  ;fire has been pressed, so set x pos of missile to players position
	ld hl,p1data
	inc hl
	ld a,(hl)
	ld (ix+1),a
	ld a,230
	ld (ix+3),a
	ld a,(ix+5)
	or SPR_VISIBLE
	ld (ix+5),a
	ld a,1
	ld (p1fire),a
notsp
	pop af

	call movep1missile
	call displaysp
	call check_alien_hit
	call check_player_hit

	halt
	jp loop

movep1missile
	ld a,(p1fire)	;Check if the missile is active, return if not
	cp 1
	ret nz
	ld ix,p1missile
	ld a,(ix+3)
	dec a
	dec a
	dec a
	cp 3		; We've reached the top of screen, so stop the missile
	jr nc, firing 
	xor a
	ld (p1fire),a
	ld a,(ix+5)
	and SPR_INVISIBLE
	ld (ix+5),a
	ret
firing	ld (ix+3),a
	ret

check_alien_hit		;Check if we have hit an alien with our missile
	ld a,(p1fire)
	cp 1
	ret nz		; we dont have an active missile, so retun
	ld bc,0303bh	; now check for a sprite collision
	in a,(c)
	rra 
	ret nc		;collision flag isnt set, so return
	xor a
	ld (p1fire),a	; assume we've hit an alien so stop missile moving (need to change this logic)

	ret

check_player_hit	;Check if we've been hit by an aliens missile
	ret

p1fire	defb 0		; 0 not firing. 1 firing

string  defb 22,1,7	    ; @ 10,13
	defb 16, 7	    ; Magenta Ink
	defb 'DOLLAR$OFT PRESENTS'
	defb 22,5,2
	defb 16, 3
	defb "Phil's Wacky HW Sprite Demo"
	defb 22,7,3
	defb 16,7
	defb "Use QAOP to Move the Apple"
	defb 22, 16,11
	defb 16, 2
	defb 'MAY 2017'
eostr  equ $

spimg	
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h	; SUPER SAM MONO
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 24h, 24h, 24h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 24h, 49h, 69h, 49h, 24h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 24h, 49h, 69h, 6dh, 6dh, 69h, 24h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 24h, 69h, 6dh, 69h, 6dh, 69h, 24h, 24h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 24h, 69h, 69h, 6dh, 69h, 49h, 24h, 49h, 24h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 24h, 49h, 69h, 69h, 49h, 45h, 45h, 69h, 49h, 24h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 24h, 24h, 24h, 45h, 45h, 45h, 49h, 45h, 49h, 6dh, 49h, 24h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 24h, 45h, 49h, 49h, 6dh, 6dh, 69h, 45h, 69h, 49h, 6dh, 24h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 24h, 49h, 69h, 6dh, 69h, 6dh, 6dh, 69h, 24h, 49h, 69h, 24h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 24h, 45h, 69h, 6dh, 6dh, 49h, 6dh, 69h, 6dh, 24h, 45h, 24h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 24h, 6dh, 69h, 6dh, 6dh, 49h, 6dh, 69h, 45h, 24h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 24h, 6dh, 69h, 49h, 69h, 69h, 49h, 24h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 24h, 45h, 49h, 45h, 24h, 24h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 24h, 24h, 24h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h


galax1	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h	; GREEN ALIEN
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 0FAh, 018h, 0FAh, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 03h, 018h, 018h, 018h, 018h, 018h, 03h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 018h, 018h, 018h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h


galax2	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h	; THIS IS A WIDE IMAGE, PROBABLY NOT USED
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 0e3h, 018h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 018h, 0e3h, 0e3h, 018h, 018h, 0e3h, 018h, 018h, 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 018h, 0fah, 0fah, 018h, 0fah, 0fah, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 018h, 0fah, 0fah, 018h, 0fah, 0fah, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 03h, 03h, 03h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 03h, 03h, 03h, 0e3h, 0e3h
	defb 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h
	defb 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h
	defb 0e3h, 03h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 03h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h

galax3	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h	;PINK ALIEN
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e2h, 0e3h, 0e2h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e2h, 0e3h, 0e3h, 0e3h, 0e2h, 0e3h, 0e2h, 0e3h, 0e3h, 0e3h, 0e2h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e2h, 0FAh, 0e2h, 0FAh, 0e2h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 03h, 0e2h, 0e2h, 0e2h, 0e2h, 0e2h, 03h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e2h, 0e2h, 0e2h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h, 0e2h, 0e3h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h

galax4  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3			;RED ALIEN
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E3, $E0, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E0, $E3, $E3, $E3, $E0, $E3, $E0, $E3, $E3, $E3, $E0, $E3,$E3, $E3
        db  $E3, $E3, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E0, $DC, $E0, $DC, $E0, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $30, $30, $30, $E0, $E0, $E0, $E0, $E0, $30, $30, $30, $E3,$E3, $E3
        db  $E3, $E3, $30, $30, $E3, $E3, $E0, $E0, $E0, $E3, $E3, $30, $30, $E3,$E3, $E3
        db  $E3, $E3, $30, $30, $E3, $E3, $E3, $E0, $E3, $E3, $E3, $30, $30, $E3,$E3, $E3
        db  $E3, $E3, $30, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $30, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3

galax5:
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3			;TOP ROW ALIEN
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $30, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $30,$E3, $E3
        db  $E3, $30, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $30,$E3, $E3
        db  $E3, $30, $F4, $E0, $E0, $F4, $F4, $E0, $F4, $F4, $E0, $E0, $F4, $30,$E3, $E3
        db  $E3, $30, $F4, $F4, $F4, $F4, $F4, $E0, $F4, $F4, $F4, $F4, $F4, $30,$E3, $E3
        db  $E3, $30, $F4, $F4, $F4, $F4, $F4, $E0, $F4, $F4, $F4, $F4, $F4, $30,$E3, $E3
        db  $E3, $30, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $30,$E3, $E3
        db  $E3, $30, $30, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $F4, $30, $30,$E3, $E3
        db  $E3, $30, $30, $E3, $E3, $E3, $E3, $F4, $E3, $E3, $E3, $E3, $30, $30,$E3, $E3
        db  $E3, $E3, $30, $30, $E3, $E3, $E3, $F4, $E3, $E3, $E3, $30, $30, $E3,$E3, $E3
        db  $E3, $E3, $E3, $30, $E3, $E3, $E3, $F4, $E3, $E3, $E3, $30, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $F4, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $F4, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3,$E3, $E3 	

gun1:	db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $F8, $F8, $E3, $E3, $E3, $E3, $E3, $E3, $E3			;PLAYERS GUN
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $F8, $F8, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $F8, $F8, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $F8, $F8, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E0, $E3, $E0, $E0, $E3, $E0, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $FF, $E3, $E3, $30, $E0, $E0, $30, $E3, $E3, $FF, $E3, $E3, $E3
        db  $E3, $E3, $E3, $FF, $E3, $E3, $30, $E0, $E0, $30, $E3, $E3, $FF, $E3, $E3, $E3
        db  $E3, $E3, $FF, $FF, $FF, $E3, $30, $E0, $E0, $30, $E3, $FF, $FF, $FF, $E3, $E3
        db  $E3, $E3, $FF, $E3, $FF, $30, $30, $E0, $E0, $30, $30, $FF, $E3, $FF, $E3, $E3
        db  $E3, $E3, $FF, $E3, $FF, $E3, $30, $E3, $E3, $30, $E3, $FF, $E3, $FF, $E3, $E3
        db  $E3, $E3, $FF, $E3, $FF, $E3, $30, $E3, $E3, $30, $E3, $FF, $E3, $FF, $E3, $E3
        db  $E3, $E3, $FF, $FF, $FF, $E3, $E3, $E3, $E3, $E3, $E3, $FF, $FF, $FF, $E3, $E3
        db  $E3, $E3, $E3, $FF, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FF, $E3, $E3, $E3

missile1:
	db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $FC, $FC, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3

blank	DS 32,0
fontdata 	EQU 60000
spritedata	EQU 61000
newfont:        ld hl,fontdata-256     ; font minus 32*8.
       		ld (23606),hl       ; point to new font.
       		ret


loadspimages	ld hl,galax1		;location of sprite image
		ld bc,243bh		;control port
		ld a,15h		;15h - sprites
		out (c),a		
		ld bc,253bh		;data port
		ld a,03h		; 3 - sprites on, over border
		out (c),a

		ld bc,12347		;select sprite 0
		xor a
		out (c),a


		ld bc,85		;read 256 bytes of image data and write to port 85
		ld e,0h
sploop1		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop1
		ld e,0h
sploop2		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop2
		ld e,0h
sploop3		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop3
		ld e,0h
		ld hl,galax4
sploop4		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop4
		ld e,0h
sploop5		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop5
		ld e,0h
sploop6		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop6
		ld e,0h
sploop7		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop7	
		ret

initspdata	ld a,(numsprites)	; write 6 bytes of 0 for each sprite 
		ld hl,spdata
		ld de,spdata+1
inloop1		ld bc,DATABLKSZ
		push af
		xor a
		ld (hl),a
		ldir
		pop af
		dec a
		jr nz, inloop1
		ret


newstartspdata	ld de,030ah		; 3 rows of 10 aliens of type 0
		ld ix,spdata		;set the starting params for each sprite - NEW FORMAT SPRITES
		ld hl,04060h
sprloop2	ld (ix+1),h		;x
		ld (ix+3),l		;y
		ld a, FLAGS
		ld (ix+4),a		;flags
		xor a
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ			; move onto next sprite
		add ix,bc
		push de
		ld de, 1400h
		add hl,de
		pop de
		dec e
		jr nz, sprloop2
		ld e,0ah
		ld a,-20
		add a,l
		ld l,a
		ld h,040h
		dec d
		jr nz, sprloop2

		ld hl,05424h
		ld e,08h
sprloop3	ld (ix+1),h		;x
		ld (ix+3),l		;y
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,2
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ			; move onto next sprite
		add ix,bc		
		push de
		ld de,1400h
		add hl,de
		pop de
		dec e
		jr nz, sprloop3

		ld hl,06812h
		ld e,06h
sprloop4	ld (ix+1),h		;x
		ld (ix+3),l		;y
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,3
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ			; move onto next sprite
		add ix,bc		
		push de
		ld de,1400h
		add hl,de
		pop de
		dec e
		jr nz, sprloop4

		ld hl,07c00h
		ld (ix+1),h		;x
		ld (ix+3),l		;y
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,4
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ	
		add ix,bc

		ld hl,0B800h
		ld (ix+1),h		;x
		ld (ix+3),l		;y
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,4
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ	

		add ix,bc		;Set PLAYER 1 positions
		ld hl, 090F0h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,5
		or SPR_VISIBLE
		ld (ix+5),a

		ld bc,DATABLKSZ	
		add ix,bc		;Set missile 1 positions
		ld hl, 090E0h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,6
		;or SPR_VISIBLE		;make it invisible to start with
		ld (ix+5),a

		ret


displaysp	ld a,(numsprites)
		ld d,a
		ld e,a
		ld ix,spdata
		sub e
dloop1		ld bc,12347		;select sprite number
		out (c),a
		ld bc,87
		ld a,(ix+1)		;xpos lsb
		out (c),a
		ld a,(ix+0)
		ld h,a			;xpos msb
		ld a,(ix+3)		;ypos
		out (c),a
		ld a,(ix+4)		;flags
		or h
		out (c),a
		ld a,(ix+5)		;pattern
		out (c),a
		ld bc,DATABLKSZ
		add ix,bc
		dec e
		ld a,d
		sub e
		cp d
		jr nz,dloop1
		ret


		defb 'NUM OF SPRITES:'
numsprites	db 48			;number of sprites

		defb 'SPRITE DATA BLOCKS - 6 bytes per block:'
spdata
r1data		dw 0			;x pos of sprite 0
		dw 0			;y pos
		db 0			;flags
		db 0			;pattern

		dw 0			;sprite 1 
		dw 0
		db 0
		db 0

		dw 0			;sprite 2
		dw 0
		db 0
		db 0

		dw 0			;sprite 3
		dw 0
		db 0
		db 0

		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0

		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
r2data		
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 14 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0

r3data		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
	
r4data		dw 0			;sprite 4 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
	
r5data		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0

r6data		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0
		dw 0			;sprite 31 (PLAYER 1)
		dw 0
		db 0
		db 0

p1data		dw 0			; PLAYER 1
		dw 0
		db 0
		db 0

p1missile	dw 0			;Player 1's Missile
		dw 0
		db 0
		db 0

		db 'END OF SPRITE DATA'

       		ORG fontdata
       		INCBIN "shortsfuse-font.bin"

END start