	   	org 6000H
ATTRP		EQU 5C8DH	    ;SYSTEM PAPER 
ROMCLS		EQU 0D6BH	    ;ROM CLS Routine
ROMBDR		EQU 229BH	    ;ROM BORDER Routine
VISIBLE		EQU 2		    ;OLD FORMAT SPRITES FLAGS - 1=visible, 0=X_MSB
FLAGS		EQU 0		    ;NEW FORMAT SPRITES FLAGS - 0-7=palette_offset, 3=x_mirror, 2=y_mirror, 1=rotate, 0=X_MSB
DATABLKSZ	EQU 16
numaliens	equ 48		    ;number of aliens
delay		equ 3		    ;number of frames to delay between alien movements
			    ;	
SPR_VISIBLE   EQU 128	    ;NEW FORMAT SPRITES - VISIBLE ATTRIBUTE GOES WITH PATTERN
SPR_INVISIBLE EQU 127	    ;NEW FORMAT SPRITES - RESET VISIBLE BIT WHILE LEAVING PATTERN INTACT
SPR_ROTATE    EQU 2	    ;NEW FORMAT SPRITES - SET ROTATE BIT
SPR_VMIRROR   EQU 4	    ;NEW FORMAT SPRITES - SET Vertical Mirror BIT

start   ld hl,ATTRP	    ;Set the PAPER and BORDER to Black, ink to bright white
	ld (hl),71
	ld a,0
	call ROMBDR
	call ROMCLS



        call loadspimages   ; load the sprite image data
        call initspdata     ; initialise the sprite info blocks to 0
        call newstartspdata ; set start positions of sprites (NEW FORMAT SPRITES)
        call displaysp	    ; display the sprites

loop		    
	ld a,(delayvar)	    ; we dont need to move the aliens every frame as that makes them too fast
	dec a		    ; so we wait for a few frames before moving the aliens. We handle the player each frame
	ld (delayvar),a	    ; we also need no delay on swarming aliens
	jr nz,wait1
	ld a,delay
	ld (delayvar),a
	xor a
	ld (waitflag),a
	jr cont
wait1  
	ld a,1
	ld (waitflag),a

cont	ld b,48
	ld ix,spdata
nxtalien
	ld h,(ix+0)	    ;increase x pos of sprite 0 and reset to 0 when it hits 320
	ld l,(ix+1)
	ld d,(ix+8)
	ld e,(ix+9)
	ld a,(alien_dir)	;0=L 1=R
	cp 1
	jr z,mv_r
	dec hl
	dec de
	jr storea
mv_r	inc hl
	inc de
storea	
	ld a,(waitflag)
	or a
	jr nz,swrm
	ld (ix+0),h
	ld (ix+1),l
	ld (ix+8),d
	ld (ix+9),e
swrm	push hl
	call chkswarm
	pop hl

chklbrdr	
	ld a,(ix+5)		;check if this alien is visible. Ignore it for edge detection if invisible
	bit 7,a
	jr z, donxtalien
	ld a,(alien_dir)
	cp 1
	jr z,chkrbrdr
	ld a,h
	cp 0
	jr nz, donxtalien
	ld a,l
	;or h	
	cp 4	
			;chk if we have hit the left side
	jr nc,donxtalien
	ld a,1
	jr storenxtdir

chkrbrdr
	;ld de,260
	;or a
	;sbc hl,de
	;add hl,de
	ld a,h
	cp 1			;check high byte
	jr nz, donxtalien
	ld a,l
	cp 50
	jr c, donxtalien
	xor a
storenxtdir
	ld (nxt_alien_dir),a
donxtalien
	ld de,DATABLKSZ
	add ix,de
	djnz nxtalien

	ld a,(nxt_alien_dir)
	ld (alien_dir),a

mp1	ld ix,p1data	   ;now deal with PLAYER 1


				; PORT AND KEY TABLE
				;32766 B, N, M, Symbol Shift, Space
				;49150 H, J, K, L, Enter
				;57342 Y, U, I, O, P
				;61438 6, 7, 8, 9, 0
				;63486 5, 4, 3, 2, 1
				;64510 T, R, E, W, Q
				;65022 G, F, D, S, A
				;65278 V, C, X, Z, Caps Shift


nota	
	ld bc,57342
	in a,(c)
	rra
	push af
	jr c, notp	  ;P not pressed so jump
	ld h,(ix+0)
	ld l,(ix+1)
	inc hl
	inc hl
	ld a,h
	or a
	jr z,strx
	ld a,l
	cp 48
	jr nc,notp
	jr z,notp	  ;cant move more right so jump
strx	ld (ix+0),h
	ld (ix+1),l

notp
	pop af
	rra
	jr c, noto	  ;O not pressed so jump
	ld h,(ix+0)
	ld l,(ix+1)
	dec hl
	dec hl
	ld a,h
	or a
	jr nz, strx2
	ld a,l
	cp 10
	jr c,noto	  ;cant move more left so jump
strx2	ld (ix+0),h
	ld (ix+1),l

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
	ld a,(hl)
	ld (ix+0),a
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
	ld bc,65022	;check if S pressed for swarm test
	in a,(c)
	rra
	rra
	call nc,start_swarm

	ld bc,64510
	in a,(c)
	rra
	rra
	rra
	rra
	jp nc, start	;R was pressed, so reset and start again.

;######### THIS BIT IS FOR DEBUGGING AND DEVELOPMENT ONLY ############################
alienfire
	ld bc,65022	  ; Check whether the fire button is pressed (f)
	in a,(c)
	rra
	rra
	rra
	rra
	push af
	jr c, notf	  ; nope f not pressed so jump

;	jr tryam2

	ld a,(a1fire)	  ;f was pressed, so check if missile1 is already active
	cp 1
	jp z, tryam2	  ; missile1 is already moving so try missile 2
	ld ix,a1missile	  ;f has been pressed, so set x pos of missile to players position
	ld de,a1fire
	jr setxpos
tryam2	ld a,(a2fire)	  ;f was pressed, so check if missile2 is already active
	cp 1
	jp z, tryam3	  ; missile2 is already moving so try missile 3
	ld de,a2fire
	ld ix,a2missile
	jr setxpos
tryam3	ld a,(a3fire)	
	cp 1
	jp z, notf 	  ; missile3 is already moving so exit
	ld de,a3fire
	ld ix,a3missile
setxpos	ld hl,p1data
	ld a,(hl)
	ld (ix+0),a
	inc hl
	ld a,(hl)
	ld (ix+1),a
	ld a,50		;set y pos of missile
	ld (ix+3),a
	ld a,(ix+5)
	or SPR_VISIBLE
	ld (ix+5),a
	ld a,1
	ld (de),a
notf	pop af
;#####################################################################################
	call movep1missile
	call movealienmissiles
	call displaysp
	halt
	;halt
	call check_alien_hit
	call check_player_hit
	call check_alien_collision
	call display_alien_bang
	call displaysp
	call makesound

	;halt
	jp loop

chkswarm
	ld a,(ix+7)	   	; is this alien set to swarm
	cp 0
	ret z			; return if not
	cp 5
	jr nc, usd		;if the counter is <5 set the flag to show sprite rotated
	set 1,(IX+4)
	jr doswarm
usd	
	cp 15			;if the counter is >15, set the flag to show sprite not rotated but v-Mirrored
	jr nc, doswarm
	set 2,(IX+4)
	res 1,(IX+4)
	cp 128
	;cp 2
	jr nz, doswarm
	xor a
	ld (ix+7),a
	;res 1,(IX+4)
	;res 2,(IX+4)
	ret		;limit to 4 entries in the swarm table
doswarm	ld hl,swarm_table	; point to swarm movement swarm_table
	ld a,(ix+7)
	dec a		
	sla a			; double a - 2bytes per table row
	ld e,a
	ld d,0
	add hl,de		;get correct table address
	ld d,(hl)		;get x movement delta
	inc hl
	ld e,(hl)		;get y movement delta
	inc (ix+7)		;increase swarm counter
	ld h,(ix+1)
	ld a,d
	add a,h			;add x delta
	cp 8
	jr nc,chkr		;has it gone off the left? i.e. is 8 bigger than A. jump if 8 not bigger than A
	ld a,8			
	jr sty
chkr	cp 250
	jr c, sty		;has it gone off the right? i.e. is 250 less than A. jump if 250 is bigger than A
	ld a,250

sty	ld (ix+0),0
	ld (ix+1),a
	ld h,(ix+3)
	ld a,e			;add y delta
	add a,h
	cp 250			; has it gone off the bottom?
	jr c, stx
	
	ld (ix+7),0		;turn off swarming
	ld (ix+4),0		;turn off mirroring etc
	ld h,(ix+8)		;get shadow x
	ld l,(ix+9)
	ld (ix+0),h		;set x to shadow x
	ld (ix+1),l
	ld a,(ix+11)		;reset y to same as shadow y
stx	ld (ix+3),a		;this line stuffs up missile detection on the bottom row if a not in the region of 80H. Suspect missile detect routine needs work.
	ret

display_alien_bang:
	ld ix,r1data
	ld b,48	; total number of aliens
	ld de,DATABLKSZ
bangl1	ld a,(ix+6)	;get counter
	cp 0
	jr z, nobang
	cp 1
	jr z, endbang
	cp 5
	jr nc,bang2	;if counter is <5, display image 7 else display image 8
	ld a, 8
	jr bang2b
bang2	ld hl,snddat+2  
	ld (hl),100
	ld hl,sndwnp	;start explosion sound
	ld (hl),10	; freq of white noise r6
	inc hl
	ld (hl),29	;mixer r7 - CHB Tone Only`
	;ld (hl),47	;mixer r7 - CHB Noise Only
	inc hl
	inc hl
	ld (hl),8	;chB amplitude
	push de
	call makesound
	pop de
	ld a, 7
bang2b	or SPR_VISIBLE
	ld (ix+5),a
	ld a,(ix+6)
	dec a
	ld (ix+6),a
nobang	add ix,de
	djnz bangl1
	ret
endbang	ld a,(ix+5)
	and SPR_INVISIBLE
	ld (ix+5),a

	xor a
	ld (ix+6),a
	ld hl,sndv2	; turn explosion off CHB Amplitude=0
	ld (hl),0
	ret

; Write the contents of our AY buffer to the AY registers.

makesound
       ld hl,snddat        ; start of AY-3-8912 register data.
       ld e,0              ; start with register 0.
       ld d,14             ; 14 to write.
       ld c,253            ; low byte of port to write.
w8912a ld b,255            ; 255*256+253 = port 65533 = select soundchip register.
       out (c),e           ; tell chip which register we're writing.
       ld a,(hl)           ; value to write.
       ld b,191            ; 191*256+253 = port 49149 = write value to register.
       out (c),a           ; this is what we're putting there.
       inc e               ; next sound chip register.
       inc hl              ; next byte to write.
       dec d               ; decrement loop counter.
       jp nz,w8912a        ; repeat until done.
       ret

snddat defw 0              ; tone registers, channel A. r0 r1
       defw 0              ; channel B tone registers. r2 r3
       defw 0              ; as above, channel C. r4 r5 
sndwnp defb 0              ; white noise period. r6
sndmix defb 0             ; tone/noise mixer control. r7
sndv1  defb 0             ; channel A amplitude/envelope generator. r10
sndv2  defb 0            ; channel B amplitude/envelope. r11
sndv3  defb 0              ; channel C amplitude/envelope. r12
sndenv defw 600            ; duration of each note. r13 & r14
       defb 0



start_swarm
	ld ix,r5data
	ld de,DATABLKSZ
	;add ix,de
	ld a,1
	ld (ix+7),a
	add ix,de
	ld (ix+7),a
	ld ix,r6data

	ld (ix+7),a
	ret

movep1missile
	ld a,(p1fire)	;Check if the missile is active, return if not
	cp 1
	ret nz

	ld hl,sndwnp	;start explosion sound
	ld (hl),10	; freq of white noise r6
	inc hl
	ld a,(hl)
	res 3,a	
	ld (hl),a	;mixer r7 - CHA Noise Only
	inc hl
	ld (hl),4	;chA amplitude
	ld ix,p1missile
	ld a,(ix+3)
	sub 7
	jr c,sf		;stop firing 
	cp 2		; We've reached the top of screen, so stop the missile
	jr nc, firing 

sf	ld hl,sndmix	; turn off missile noise
	ld a,(hl)
	set 3,a
	ld (hl),a
	xor a
	ld (p1fire),a
	ld a,(ix+5)
	and SPR_INVISIBLE
	ld (ix+5),a
	ld bc,0303bh	; now check for a sprite collision
	in a,(c)
	ret
firing	ld (ix+3),a
	ret

movealienmissiles
	ld a,(a1fire)	;Check if the missile is active, return if not
	cp 1
	jr nz, am2
	ld ix,a1missile
	ld de,a1fire
	call movam
am2	ld a,(a2fire)
	cp 1
	jr nz, am3
	ld ix,a2missile
	ld de,a2fire
	call movam

am3	ld a,(a3fire)
	cp 1
	ret nz
	ld ix,a3missile
	ld de,a3fire
	call movam
	ret

movam
	ld a,(ix+3)
	add a,3
	jr c,sf2	;stop firing 
	cp 250		; We've reached the bottom of the screen, so stop the missile
	jr c, firing2
sf2	
	xor a
	ld (de),a
	ld a,(ix+5)
	and SPR_INVISIBLE
	ld (ix+5),a
	ret
firing2	ld (ix+3),a
	ret


check_alien_hit		;Check if we have hit an alien with our missile
	ld a,(p1fire)
	cp 1
	ret nz		; we dont have an active missile, so retun
	ld bc,0303bh	; now check for a sprite collision
	in a,(c)
	rra 
	;ret nc		;collision flag isnt set, so return. REMOVED - COLLISON BIT IS SHIT
	ld hl,p1missile+3
	ld a,(hl)
	push af	; get missile Y co-ord
r1chk	ld ix,r1data
	ld c	,1
	ld h,(ix+3)	; get Y-Co-ord of first row of Aliens
	cp h
	jr c, r2chk	; missile has gone past row 1
ca1	sub h
	cp 16
	jr c, ahit
	jr z, ahit
r2chk	pop af
	push af
	ld c,2
	ld ix,r2data	;check against 2nd row of aliens
	ld h,(ix+3)
	cp h
	jr c, r3check	;
ca2	sub h
	cp 16
	jr c, ahit
	jr z, ahit
r3check	pop af
	push af
	ld c,3
	ld ix,r3data	;check against 3rd row of aliens
	ld h,(ix+3)
	cp h
	jr c, r4check	;
ca3	sub h
	cp 15
	jr c, ahit
	jr z, ahit
r4check pop af
	push af
	ld c,4
	ld ix,r4data	;check against 4th row of aliens
	ld h,(ix+3)
	cp h
	jr c, r5check	;
ca4	sub h
	cp 15
	jr c, ahit
	jr z, ahit
r5check pop af
	push af
	ld c,5
	ld ix,r5data	;check against 5th row of aliens
	ld h,(ix+3)
	cp h
	jr c, r6check	;
ca5	sub h
	cp 15
	jr c, ahit
	jr z, ahit
r6check pop af
	push af
	ld c,6
	ld ix,r6data	;check against 6th row of aliens
	ld h,(ix+3)
	cp h
	jr c, nohit
ca6	sub h
	cp 15
	jr c, ahit
	jr z, ahit
nohit	pop af
	ret
ahit			; We have a hit on the Y position, lets check the X position
	ld a,c		;c = the row weve hit
	ld b,0		; now need to find which column
	ld hl,spritesperrow-1	;how many sprites on the row we've hit?
	add hl,bc
	ld a,c
	pop af
	ld a,(hl)
	ld b,a
	ld hl,p1missile+1
	ld a,(hl)
	ld c,a
			; at this point, IX points to the start of the row spdata
			; b holds the number of aliens on this row
			; c holds the x position of the missile
			; lets check each alien on this row's xposition
xchklp	
	ld a,(ix+1)
	sub c
	jr nc, xchk1	; we are to the left of this alien so check next one
	neg
xchk1	cp 8
	jr c, xhit	; we are too far to the right so check next one
	jr z, xhit
			; we are within 15 so thats a hitb

nxtxchk ld de,DATABLKSZ
	add ix,de	;move to next sprite data block
	djnz xchklp
	ret

xhit	
	ld a,(p1missile) ; high byte of missile x pos
	cp (ix+0)	 ; high byte of alien x pos
	ret nz		; high bytes need to be the same for a valid hit. return if not
	ld a,(ix+5)	;Lets check if the alien is visible. Return if not.
	bit 7,a
	ret z
	ld a,(ix+6)
	or a
	ret nz
	xor a
	ld (p1fire),a	; we've hit an active alien so stop missile moving
	ld a,10
	ld (ix+6),a	;set explosion going
	ld ix,p1missile	; make missile invisible
	ld a,(ix+5)
	and SPR_INVISIBLE
	ld (ix+5),a
	ret

check_player_hit	;Check if we've been hit by an aliens missile
	ret

check_alien_collision	;Check if we have collided with a swarming alien
	ld ix,r1data
	ld b,numaliens
colloop	ld a,(ix+7)
	or a
	jr z,nocol1	;this alien isn't swarming so try next one
	ld a,(ix+3)	;get the alien y coord. 
	cp $e8		;compare it with player
	jr c,nocol1	;if its above it, try next alien
	ld e,(ix+1)	;now check xpos
	ld hl,p1data+1
	ld a,(hl)
	sub e
	jr nc,chk1
	;ld (16384),a
	neg
	;ld (16385),a
chk1	cp 8
	jr nc,nocol1

	ld a,5		;There has been a collision. Do appropriate stuff (border change for now)
	call ROMBDR
	ret

nocol1	ld de,DATABLKSZ	;no collision so move onto next alien
	add ix,de
	djnz colloop
	ret



p1fire		defb 0	; 0 not firing. 1 firing
a1fire		defb 0  ;
a2fire		defb 0  ;
a3fire		defb 0  ;
alien_dir	defb 0	; 0=right, 1=left
nxt_alien_dir	defb 0
delayvar	defb 2
waitflag	defb 0

; left edge=0
; bottom = 320
;swarm_table	defb 0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,-2,0,0,0
swarm_table	defb 0,1, -2,-2, -2,-2, -2,-2, -2,-2, -2,-2, -2,0, -2,2, 0,2, 0,2, 0,2, 0,2, 0,2 ,0,2, 0,2, 0,2, 0,2, 0,2, 2,3, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2
		defb 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2
		defb 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2
		defb -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2
		defb -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2 ,0,0

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
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3

bang1img:
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $EC, $E0, $E3, $E3
        db  $E3, $E3, $EC, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $EC, $E0, $E3, $E3, $E3
        db  $E3, $E3, $EC, $EC, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E0, $EC, $EC, $00, $00, $EC, $EC, $E0, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E0, $00, $EC, $EC, $EC, $EC, $00, $E0, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E0, $00, $EC, $EC, $EC, $EC, $00, $E0, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E0, $EC, $EC, $00, $00, $EC, $EC, $E0, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E0, $EC, $E3, $E3, $E3, $E3, $E3, $E3, $EC, $E0, $E3, $E3, $E3
        db  $E3, $E3, $E0, $EC, $EC, $E3, $E3, $E3, $E3, $E3, $E3, $EC, $EC, $E0, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
 
 
 
bang2img:
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E0, $E3, $E3, $E3, $E0, $E0, $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E3, $E3
        db  $E3, $E0, $E0, $E3, $E3, $E3, $E0, $E3, $E3, $E3, $E3, $E0, $E0, $E3, $E3, $E3
        db  $E3, $E3, $E0, $E0, $E3, $E3, $E0, $E3, $E3, $E3, $E0, $E0, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E0, $E3, $E3, $E0, $E0, $E3, $E0, $E0, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E0, $E3, $E0, $E0, $E0, $E0, $E0, $E3, $E3
        db  $E0, $E3, $E3, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E3
        db  $E0, $E0, $E3, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E3, $E0, $E0
        db  $E3, $E0, $E0, $E3, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E3
        db  $E3, $E3, $E0, $E0, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E3, $E0, $E3
        db  $E3, $E3, $E3, $E3, $E0, $E0, $E0, $E3, $E0, $E3, $E3, $E3, $E0, $E0, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E0, $E3, $E3, $E0, $E3, $E0, $E0, $E0, $E3, $E3, $E0, $E3
        db  $E3, $E3, $E3, $E3, $E0, $E3, $E3, $E3, $E0, $E3, $E0, $E0, $E3, $E3, $E0, $E3
        db  $E3, $E3, $E3, $E3, $E0, $E3, $E3, $E3, $E0, $E0, $E3, $E0, $E0, $E3, $E0, $E3
        db  $E3, $E3, $E3, $E0, $E3, $E3, $E3, $E0, $E0, $E3, $E3, $E3, $E0, $E3, $E3, $E0
        db  $E3, $E3, $E0, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E3, $E3


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

		ld d,9			; 8 sprites to read data for
sploop0		ld bc,85		;read 256 bytes of image data and write to port 85 for each sprite
		ld e,0h
sploop1		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop1
		dec d
		jr nz, sploop0
		ret

initspdata	ld a,(numsprites)	; write DATABLKSZ bytes of 0 for each sprite 
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
		ld hl,04080h
sprloop2	ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l		;shadow y
		ld a, FLAGS
		ld (ix+4),a		;flags
		xor a
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ		; move onto next sprite
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

		ld hl,05444h
		ld e,08h
sprloop3	ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,2
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ		; move onto next sprite
		add ix,bc		
		push de
		ld de,1400h
		add hl,de
		pop de
		dec e
		jr nz, sprloop3

		ld hl,06832h
		ld e,06h
sprloop4	ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,3
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ		; move onto next sprite
		add ix,bc		
		push de
		ld de,1400h
		add hl,de
		pop de
		dec e
		jr nz, sprloop4

		ld hl,07c21h
		ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,4
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,DATABLKSZ	
		add ix,bc
		add ix,bc
		add ix,bc

		ld hl,0B821h
		ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l
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
		ld (ix+5),a

		ld bc,DATABLKSZ	
		add ix,bc		;Set alien missile 1 positions
		ld hl, 090E0h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,6
		ld (ix+5),a

		ld bc,DATABLKSZ	
		add ix,bc		;Set alien missile 2 positions
		ld hl, 090E0h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,6
		ld (ix+5),a

		ld bc,DATABLKSZ	
		add ix,bc		;Set alien missile 3 positions
		ld hl, 090E0h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,6
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


get_alien_data				;Find the data block for sprite at b=row, c=column (row 1 is bottom row)
		push de			; return the address of the data block in hl
		push af
		ld hl,0
		ld de,spritesperrow
		dec c
		dec b
		jr z, findcol
getrowcnt	ld a,(de)
		add a,l
		ld l,a
		inc de
		djnz getrowcnt
findcol		ld a,l
		add a,c			
		ld h,a			;h now points to the sprite number
		ld d,DATABLKSZ		;d is the size of a data block
		push bc
		call multhbyd		;multiply h by d and get result in hl
		pop bc
		ld a,l
		ld l,a
		ld de,spdata
		add hl,de		;hl now points to the correct data block
		pop af
		pop de
		ret

multhbyd	ld e,d              ; HL = H * D
       		ld a,h              ; make accumulator first multiplier.
       		ld hl,0             ; zeroise total.
       		ld d,h              ; zeroise high byte so de=multiplier.
      		ld b,8              ; repeat 8 times.
imul1  		rra                 ; rotate rightmost bit into carry.
       		jr nc,imul2         ; wasn't set.
       		add hl,de           ; bit was set, so add de.
       		and a               ; reset carry.
imul2 		rl e                ; shift de 1 bit left.
       		rl d
       		djnz imul1          ; repeat 8 times.
       		ret	







		defb 'NUM OF SPRITES:'
numsprites	db 53			;number of sprites (inc player & missile & alien missiles)

spritesperrow	db 10, 10 ,10, 8, 6, 4	;number of sprites on each row

		defb 'SPRITE DATA BLOCKS - 8 bytes per block:'
spdata
		
		;dw 0			;x pos of sprite 0
		;dw 0			;y pos
		;db 0			;flags
		;db 0			;pattern
		;db 0			;counter (used for explosions)
		;db 0			;swarm counter
		;dw 0			;shadow x (+8 +9)
		;dw 0			;shadow y (+10 +11)

r1data		ds 10*DATABLKSZ		; 10 sprites on Row1 
		

r2data		ds 10*DATABLKSZ		; 10 sprites on Row2 


r3data		ds 10*DATABLKSZ		; 10 sprites on Row3 

	
r4data		ds 8*DATABLKSZ		; 8 sprites on Row4

r5data		ds 6*DATABLKSZ		; 6 sprites on Row5 


r6data		ds 4*DATABLKSZ		; 4 sprites on Row6 



p1data		ds DATABLKSZ		; Player 1s ship

p1missile	ds DATABLKSZ		;Player 1's Missile

a1missile	ds DATABLKSZ		;ALien 1 Missile
a2missile	ds DATABLKSZ		;ALien 2 Missile
a3missile	ds DATABLKSZ		;ALien 3 Missile
		db 'END OF SPRITE DATA'

       		ORG fontdata
       		INCBIN "shortsfuse-font.bin"

END start