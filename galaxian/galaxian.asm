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
SWARM_VALUE   EQU 127	    ;Swarm if the random number is less than this value - used to speed up or slow down swarming

start   ld hl,ATTRP	    ;Set the PAPER and BORDER to Black, ink to bright white
	ld (hl),71
	xor a
	ld (swarming_in_progress),a
	ld hl,hitcount
	ld (hl),a
	call ROMBDR
	call ROMCLS
	ld hl,0
	ld (seed),hl




	;call newfont
        call loadspimages   ; load the sprite image data
        call initspdata     ; initialise the sprite info blocks to 0
        call newstartspdata ; set start positions of sprites (NEW FORMAT SPRITES)
        call displaysp	    ; display the sprites

        call resetscore
        call displayscore	
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
	ld h,(ix+0)	    
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
	push hl
	ld a,(waitflag)
	or a
	jr nz,swrm
	ld (ix+0),h
	ld (ix+1),l
	ld (ix+8),d
	ld (ix+9),e
	;push hl
	call init_swarm		;check whether to initiate a swarm
	;pop hl
swrm	;push hl
	call chkswarm
	pop hl

chklbrdr	
	ld a,(ix+5)		;check if this alien is visible. Ignore it for edge detection if invisible
	bit 7,a
	jr z, donxtalien
	ld a,(ix+7)		;check if this alien is swarming. Ignore it for edge detection if swarming
	cp 0
	jr nz,donxtalien

	ld a,(alien_dir)
	cp 1
	jr z,chkrbrdr
	ld a,h			
	cp 0
	jr nz, donxtalien
	ld a,l			
	cp 4	
				;chk if we have hit the left side
	jr nc,donxtalien 	;not hit the left
	ld a,1			;we've hit the left
	jr storenxtdir

chkrbrdr
	;ld de,260
	;or a
	;sbc hl,de
	;add hl,de

	;ld a,h
	ld a,(ix+0)		;should be able to use hl here, but it has become corrupted so re-reading ix instead
	cp 1			;check high byte
	jr nz, donxtalien
	;ld a,l
	ld a,(ix+1)
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
	ld a,(p1fire)
	or a
	jr nz,notp
	ld hl,p1missile
	ld a,(ix+0)
	ld (hl),a
	inc hl
	ld a,(ix+1)
	ld (hl),a

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
	ld a,(p1fire)
	or a
	jr nz, noto
	ld hl,p1missile
	ld a,(ix+0)
	ld (hl),a
	inc hl
	ld a,(ix+1)
	ld (hl),a

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
	ld a,224
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

	ld a,(swarming_in_progress)
	or a
	jr nz,dont_start_swarm		;we are already swarming so dont bother doing it again

	ld a,(do_swarm)	;check if the swarm flag has been set - swarm if so
	or a
	call nz,start_swarm

dont_start_swarm

	ld bc,64510
	in a,(c)
	rra
	rra
	rra
	rra
	jp nc, start	;R was pressed, so reset and start again.

;######### THIS BIT IS FOR DEBUGGING AND DEVELOPMENT ONLY ############################


;#####################################################################################
	call movep1missile
	call movealienmissiles
	call displaysp
	halt

	call check_alien_hit
	call updatescore
	ld hl,hitcount
	ld a,(hl)
	cp 46
	call z,displaygameover
	call check_player_hit
	call check_alien_collision
	call display_alien_bang
	;call displaysp
	call makesound

	
	jp loop

init_swarm
	xor a
	ld (do_swarm),a
	call random			;get a random number
	cp SWARM_VALUE			;if it is less than 7 then swarm
	ret nc			;else just return

	ld a,1
	ld (do_swarm),a
	ret
do_swarm
	db 0			;flag - initiate a swarm if set to 1

; Pseudo-random number generator.
; Steps a pointer through the ROM (held in seed), returning the contents
; of the byte at that location.

random ld hl,(seed)        ; pointer to ROM.
       res 5,h             ; stay within first 8K of ROM.
       ld a,(hl)           ; get "random" number from location.
       xor l               ; more randomness.
       inc hl              ; increment pointer.
       ld (seed),hl        ; new position.
       ret
 seed	dw 0
 swarming_in_progress
 	db 0

chkswarm
	ld a,(ix+7)	   	; is this alien set to swarm
	cp 0
	ret z			; return if not

	ld (swarming_in_progress),a	;set flag to say we are already swarming
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
doswarm	ld a,(ix+12)		; get swarm direction
	cp 0
	jr nz,setrswm
	ld hl,l_swarm_table	; point to swarm movement swarm_table
	jr swrmnow
setrswm	ld hl,r_swarm_table

swrmnow	ld a,(ix+7)
	dec a		
	sla a			; double a - 2bytes per table row
	ld e,a
	ld d,0
	add hl,de		;get correct table address
	ld d,(hl)		;get x movement delta
	inc hl
	ld e,(hl)		;get y movement delta
	inc (ix+7)		;increase swarm counter
	ld h,(ix+0)
	ld l,(ix+1)		

	push de
	ld e,d
	bit 7,e
	jr nz, negdelta
	ld d,0
	jr storexd
negdelta
	ld d,255
storexd	
	add hl,de		;add x delta
	pop de

	ld a,h
	cp 1
	jr nz,chkxl		;MSB not set so check x left pos

	ld a,l
	cp 48
	;jr c,sty		;has it gone off right. i.e is 48 bigger than A. Jump if 48 not bigger than A
	;jr z,sty
	jr c,chkxl
	;jr z, chkxl

				;So, here we have gone off the right edge. Fix it.
	;ld hl,302

	ld h,1
	ld l,50

	jr sty



chkxl	
	ld a,h
	cp 1
	jr z,sty		;if msb is set then its not at the left so jump over this stuff`
	ld a,l
	cp 8
	jr nc,chkr		;has it gone off the left? i.e. is 8 bigger than A. jump if 8 not bigger than A
	ld a,8
	ld hl,8			
	;jr sty
chkr	;cp 252
	;jr c, sty		;has it gone off the right? i.e. is 250 less than A. jump if 250 is bigger than A
	;ld (16384),a
	;ld a,252

sty	ld (ix+0),h
	ld (ix+1),l
	;ld a,h
	;ld (16390),a
	;ld a,l
	;ld (16391),a
	ld h,(ix+3)
	ld a,e			;add y delta
	add a,h
	cp 250			; has it gone off the bottom?
	jr c, stx
	
	ld (ix+7),0		;It has gone off the bottom so turn off swarming
	;push af
	xor a
	ld (swarming_in_progress),a
	;pop af
	ld (ix+4),0		;turn off mirroring etc
	ld h,(ix+8)		;get shadow x
	ld l,(ix+9)
	ld (ix+0),h		;set x to shadow x
	ld (ix+1),l
	ld a,(ix+11)		;reset y to same as shadow y
stx	ld (ix+3),a		

				;check random number and fire if necessary
alien_shoot
	call random
	cp 10
	ret nc
	push ix
	pop hl
	call shoot_alien_bomb

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
	
	;ld ix,r5data	; To start a swarm, just set ix+7 of the correct alien to 1
	;ld de,DATABLKSZ ; Here we are swarming 3 alens at the same time.
	;ld a,1		 ; 
	;ld (ix+7),a	 ;Normally we would just swarm one alien at a time 
	;add ix,de	 ; but would do 2 or 3 when swarming the top 2 rows
	;ld (ix+7),a
	;ld ix,r6data
;	ld (ix+7),a

	xor a
	ld (do_swarm),a


	ld hl,(swarm_start_pointer)	;Point to the current position in the swarm start table (decides left or right side swarm)
	ld a,(hl)
	inc hl
	ld (swarm_start_pointer),hl
	cp 2				;2 indicates end of table - point back to the start of table again
	jr nz, noreset
	ld hl,swarm_start_table
	ld (swarm_start_pointer),hl
	ld a,(swarm_start_table)
noreset
	cp 0
	jp nz, rightswarm		;0 is left swarm. 1 is right swarm leftswarm

	;jr rightswarm
	;ld bc,0101h	
	call random			;select which row will swarm
	and 7				;ensure it is 1 to 6
	cp 0
	jr z,l1ax			;if it is already 0 dont dec it as that will make it 255
	dec a
	jr l1x
l1ax	ld a,1	

l1x	ld b,a
	ld c,1				;start at col 1 then work inwards if leftmost sprite is dead

left_loop
	push bc
	call get_alien_data		;get hl=address of sprite data for sprite at y=b,x=c
	pop bc
	push hl
	pop ix
	ld a,(ix+5)			;check if this alien is invisible
	bit 7,a
	jp z,find_next_lspot
	ld a,(ix+7)			;check if this alien is swarming
	cp 1
	ret z
	ld (ix+7),1
	ld (ix+12),0
	ld a,b				;check if it this alien is on the top 2 rows
	cp 5
	jr nz, notarow5

	inc c				;this is row 5, so try to swarm next neighbour and above neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	jr z,toprow1
	ld (ix+7),1
	ld (ix+12),0

toprow1 
	ld bc,$0601
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	ret z
	ld (ix+7),1
	ld (ix+12),0

	ret
notarow5
	cp 6
	ret nz

	ld hl,0
	ld (row5count),hl
	ld bc,$0501				;this is row 6, so try to swarm below neighbour and its next neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	jr z,no1isdead
	ld (ix+7),1
	ld (ix+12),0
	ld hl,row5count
	inc (hl)
no1isdead
	ld bc,$0502				;this is row 6, so try to swarm below neighbour and its next neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	jr z,no2isdead
	ld (ix+7),1
	ld (ix+12),0
	ld hl,row5count
	inc (hl)
no2isdead
	ld hl,(row5count)
	ld a,l
	cp 2
	ret z

	ld bc,$0503				;this is row 6, so try to swarm below neighbour and its next neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	ret z
	ld (ix+7),1
	ld (ix+12),0
	ld hl,row5count
	inc (hl)
	ret

find_next_lspot
	inc c
	cp 5
	jp nc,left_loop
	ret

row5count
	dw 0

rightswarm
	;ld bc,030ah	
	call random			;select which row will swarm
	and 7
	cp 0
	jr z,l2x
	dec a				;ensure it is 1 to 6

l2x	ld b,a
	cp 0
	jr nz,notrow0			;if it 0 set it to 1
	ld a,1
	ld b,a

					;Work out the sprite number of the RHS Sprite
notrow0	cp 6				;It is different depending on what the row is
	jr nz,notrow6
	ld c,4
	jr l1
notrow6	cp 5
	jr nz,notrow5
	ld c,6
	jr l1
notrow5 cp 4
	jr nz,notrow4
	ld c,8
	jr l1
notrow4
	ld c,0ah
l1
	push bc
	call get_alien_data		;get hl=address of sprite data for sprite at y=b,x=c
	pop bc
	push hl
	pop ix
	ld a,(ix+5)			;check if this alien is invisible
	bit 7,a
	jp z,find_next_rspot
	ld a,(ix+7)
	cp 1
	ret z
	ld (ix+7),1
	ld (ix+12),1
	ld a,b				;check if it this alien is on the top 2 rows
	cp 5
	jr nz, notbrow5
	dec c				;this is row 5, so try to swarm next neighbour and above neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	jr z,toprow1a
	ld (ix+7),1
	ld (ix+12),1
toprow1a
	ld bc,$0604
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	ret z
	ld (ix+7),1
	ld (ix+12),1
	ret
notbrow5
	cp 6
	ret nz
					;this is row 6. select 2 from the row below
	ld hl,0
	ld (row5count),hl
	ld bc,$0506				;this is row 6, so try to swarm below neighbour and its next neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	jr z,no1isdead2
	ld (ix+7),1
	ld (ix+12),1
	ld hl,row5count
	inc (hl)
no1isdead2
	ld bc,$0505				;this is row 6, so try to swarm below neighbour and its next neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	jr z,no2isdead2
	ld (ix+7),1
	ld (ix+12),1
	ld hl,row5count
	inc (hl)
no2isdead2
	ld hl,(row5count)
	ld a,l
	cp 2
	ret z

	ld bc,$0504				;this is row 6, so try to swarm below neighbour and its next neighbour
	push bc
	call get_alien_data
	pop bc
	push hl
	pop ix
	ld a,(ix+5)
	bit 7,a
	ret z
	ld (ix+7),1
	ld (ix+12),1
	ld hl,row5count
	inc (hl)
	ret
	ret
find_next_rspot
	dec c
	cp 5
	jp nc,l1
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
	ld hl,p1data
	ld a,(hl)
	ld (ix+0),a
	inc hl
	ld a,(hl)
	ld (ix+1),a
	ld a,224
	ld (ix+3),a
	ld a,(ix+5)
	OR SPR_VISIBLE
	ld (ix+5),a

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

check_alien_hit
	ld a,(p1fire)
	cp 1
	ret nz		; we dont have an active missile, so retun
	ld b,48
	ld ix,r1data
alien_chk_loop

	ld a,(ix+5)	;is this alien visible?
	bit 7,a
	jr z,chk_nxt_alien

	ld hl,p1missile+3
	ld a,(hl)	;get missile y co-ord

	ld h,(ix+3)
	cp h
	jr c, chk_nxt_alien
	sub h
	cp 16
	jr c, yhit	;y co-ords match, so lets check the x co-ords
	jr z,yhit
chk_nxt_alien		
	ld de,DATABLKSZ	;we've not hit this one, lets check the next one
	add ix,de
	djnz alien_chk_loop
	ret

yhit


	ld hl,p1missile
	ld d,(hl)
	inc hl
	ld e,(hl)	;de = missile xpos 16bit

	ld h,(ix+0)
	ld l,(ix+1)	;hl = alien xpos 16bit

	or a
	sbc hl,de	
	jr c, hlbigger
	ld de,8
	sbc hl,de
	jr nc, chk_nxt_alien
	jr yhit1
hlbigger
	xor a		;neg hl
	sub l
	ld l,a
	sbc a,a
	sub h
	ld h,a
	ld de,8
	sbc hl,de
	jr nc,chk_nxt_alien

yhit1 	

	ld a,(ix+6)	; is it already exploding?
	or a
	jr nz,chk_nxt_alien

	
	xor a
	ld (p1fire),a	; we've hit an active alien so stop missile moving
	ld a,10
	ld (ix+6),a	;set explosion going

	ld a,(ix+7)
	or a
	jr z,notswarming
	ld a,(ix+14)
	jr upd_score
notswarming
	ld a,(ix+13)

upd_score
	cp 30
	call z,add30
	cp 40
	call z,add40
	cp 50
	call z,add50
	cp 60
	call z,add60
	cp 80
	call z,add80
	cp 100
	call z,add100
	cp 200 
	call z,add200	

	ld ix,p1data	; make missile invisible
	ld hl,p1missile	
	ld a,(ix+0)
	ld (hl),a
	inc hl
	ld a,(ix+1)
	ld (hl),a	
	inc hl
	inc hl
	ld a,224
	ld (hl),a
	;ld a,(ix+5)
	;and SPR_INVISIBLE
	;ld (ix+5),a
	ld hl,hitcount
	inc (hl)
	ret

add30
       ld hl,score+4       ; point to tens column.
       ld b,3             ; 3 tens = 30.
       call uscor          ; up the score.	
       
       xor a
       ret

add40
       ld hl,score+4       ; point to tens column.
       ld b,4             ; 4 tens = 40.
       call uscor          ; up the score.	
       xor a
       ret

add50
       ld hl,score+4       ; point to tens column.
       ld b,5             ; 5 tens = 50.
       call uscor          ; up the score.	
       xor a
       ret

add60
       ld hl,score+4       ; point to tens column.
       ld b,6            ; 6 tens = 60.
       call uscor          ; up the score.	
       xor a
       ret

add80
       ld hl,score+4       ; point to tens column.
       ld b,8             ; 8 tens = 80.
       call uscor          ; up the score.	
       xor a
       ret

add100
       ld hl,score+3	   ;point to hundreds column
       ld b,1
       call uscor         ; up the score.	
       xor a
       ret

add200
       ld hl,score+3	   ;point to hundreds column
       ld b,2
       call uscor         ; up the score.	
       xor a
       ret


check_player_hit	;Check if we've been hit by an aliens missile
	ld a,(a1fire)
	ld ix,a1missile
	cp 1
	jr nz,a2chk
	call chkmissilehit
	or a
	jr nz,missilehit	;'Weve' been hit by the missile
a2chk	ld a,(a2fire)
	ld ix,a2missile
	cp 1
	jr nz,a3chk
	call chkmissilehit
	or a
	jr nz,missilehit	;We've been hit by the missile
a3chk	ld a,(a3fire)
	ld ix,a3missile
	cp 1
	ret nz
	call chkmissilehit
	or a
	ret z
missilehit	
	;ld a,6
	;call ROMBDR
	ret

chkmissilehit			;check if the missile (ix) has hit the player. Return a <> 0 if hit.
	ld a,(ix+3)
	cp 240
	jr c,nomhit		;missile is too high no hit
	ld a,(p1data+1)		;get player x
	sub (ix+1)
	jr z,mhit
	jr nc,notneg
	neg
notneg	cp 8

	jr nc, nomhit
	ret
nomhit	xor a
	ret
mhit	ld a,1
	ret


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
	neg

chk1	cp 8
	jr nc,nocol1

	;ld a,5		;There has been a collision. Do appropriate stuff (border change for now)
	;call ROMBDR
	ret

nocol1	ld de,DATABLKSZ	;no collision so move onto next alien
	add ix,de
	djnz colloop
	ret

shoot_alien_bomb
	push hl		  ;hl points to the alien data block
	ld a,(a1fire)	  ; check if missile1 is already active
	cp 1
	jr z, tryam2x	  ; missile1 is already moving so try missile 2
	ld ix,a1missile	  
			  ;set x pos of missile to aliens position
	ld de,a1fire
	jr setxposx
tryam2x	ld a,(a2fire)	  ;check if missile2 is already active
	cp 1
	jp z, tryam3x	  ; missile2 is already moving so try missile 3
	ld de,a2fire
	ld ix,a2missile
	jr setxposx
tryam3x	ld a,(a3fire)	
	cp 1
	jr z, notfx 	  ; missile3 is already moving so exit
	ld de,a3fire
	ld ix,a3missile
setxposx
	pop hl
	ld a,(hl)
	ld (ix+0),a
	inc hl
	ld a,(hl)
	ld (ix+1),a
	inc hl
	inc hl
	ld a,(hl)	;set y pos of missile
	ld (ix+3),a
	ld a,(ix+5)
	or SPR_VISIBLE
	ld (ix+5),a
	ld a,1
	ld (de),a
notfx	pop af
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
l_swarm_table	defb 0,1, -2,-2, -2,-2, -2,-2, -2,-2, -2,-2, -2,0, -2,2, -1,2, -1,2, 0,2, -1,2, 0,2, -1,2, -1,2 ,-1,2, -1,2, -1,2, -1,2, 0,2, 1,2, 1,2, 2,3, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2
		defb 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2
		defb 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,1, 2,1, 1,1, 1,1, 0,1, 0,1, -1,2, -1, 2
		defb -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2
		defb -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2 ,0,0

r_swarm_table	defb 0,1, 2,-2, 2,-2, 2,-2, 2,-2, 2,-2, 2,0, 2,2, 1,2, 1,2, 0,2, 1,2, 0,2, 1,2, 1,2 ,1,2, 1,2, 1,2, 1,2, 0,2, 1,2, 1,2, 2,3, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2
		defb 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2
		defb 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,2, 2,1, 2,1, 1,1, 1,1, 0,1, 0,1, -1,2, -1, 2
		defb -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2
		defb -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2, -2,2 ,0,0

swarm_start_pointer	
		dw swarm_start_table

swarm_start_table
		;defb 0,0,0,0,0,0,0,0,2
		;defb 1,1,1,1,1,1,1,1,2
		defb 0,1,0,1,1,0,1,0,0,1,0,0,1,0,0,1,1,0,1,0,1,1,2

swarmkey	defb 0



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

gun1:	db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3			;PLAYERS GUN
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
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

token:	db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3			;PLAYER Lives Left Token
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3
        db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E0, $E0, $E3, $E3, $E3, $E3, $E3, $E3, $E3
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

		ld d,10			; 10 sprites to read data for
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
		ld hl,04090h
sprloop2	ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l		;shadow y
		ld a, FLAGS
		ld (ix+4),a		;flags
		xor a
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld (ix+13),30		;convoy score
		ld (ix+14),60		;swarm score
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

		ld hl,05454h
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
		ld (ix+13),40		;convoy score
		ld (ix+14),80		;swarm score
		ld bc,DATABLKSZ		; move onto next sprite
		add ix,bc		
		push de
		ld de,1400h
		add hl,de
		pop de
		dec e
		jr nz, sprloop3

		ld hl,06842h
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
		ld (ix+13),50		;convoy score
		ld (ix+14),100		;swarm score
		ld bc,DATABLKSZ		; move onto next sprite
		add ix,bc		
		push de
		ld de,1400h
		add hl,de
		pop de
		dec e
		jr nz, sprloop4

		ld hl,07c31h
		ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,4
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld (ix+13),60		;convoy score
		ld (ix+14),200		;swarm score
		ld bc,DATABLKSZ	
		add ix,bc		; miss out 2 sprites on row 6 - they dont really exist
		add ix,bc
		add ix,bc

		ld hl,0B831h
		ld (ix+1),h		;x
		ld (ix+9),h
		ld (ix+3),l		;y
		ld (ix+11),l
		ld a, FLAGS
		ld (ix+4),a		;flags
		ld a,4
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld (ix+13),60		;convoy score
		ld (ix+14),200		;swarm score
		ld bc,DATABLKSZ	

		add ix,bc		;Set PLAYER 1 positions
		ld hl, 090E0h
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
		OR SPR_VISIBLE	
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

		ld bc,DATABLKSZ	
		add ix,bc		;Set lives left token positions
		ld hl, 010F2h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,9
		or SPR_VISIBLE
		ld (ix+5),a

		ld bc,DATABLKSZ	
		add ix,bc		;Set lives left token positions
		ld hl, 020F2h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,9
		or SPR_VISIBLE
		ld (ix+5),a

		ld bc,DATABLKSZ	
		add ix,bc		;Set lives left token positions
		ld hl, 030F2h
		ld (ix+1),h
		ld (ix+3),l
		ld a,FLAGS
		ld (ix+4),a
		ld a,9
		or SPR_VISIBLE
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


;This stores the current score as ascii chars so they are easier to display


uscor  ld a,(hl)           ; current value of digit.
       add a,b             ; add points to this digit.
       ld (hl),a           ; place new digit back in string.
       cp 58               ; more than ASCII value '9'?
       ret c               ; no - relax.
       sub 10              ; subtract 10.
       ld (hl),a           ; put new character back in string.
uscor0 dec hl              ; previous character in string.
       inc (hl)            ; up this by one.
       ld a,(hl)           ; what's the new value?
       cp 58               ; gone past ASCII nine?
       ret c               ; no, scoring done.
       sub 10              ; down by ten.
       ld (hl),a           ; put it back
       jp uscor0           ; go round again.

;To use this we point hl at the digit we would like to increase, place the amount we want to add in the b register, then call uscor. 
;For example, to add 250 to the score requires 6 lines:
;; Add 250 to the score.

       ld hl,score+3       ; point to hundreds column.
       ld b,2              ; 2 hundreds = 200.
       call uscor          ; increment the score.
       ld hl,score+4       ; point to tens column.
       ld b,5              ; 5 tens = 50.
       call uscor          ; up the score.


displayscore
	ld a,2              ; upper screen
        call 5633           ; open channel
        ld de,string        ; address of string
        ld bc,eostr-string  ; length of string to print
        call 8252           ; print our string
        ret

updatescore
	ld a,2              ; upper screen
        call 5633           ; open channel
        ld de,scorestr        ; address of string
        ld bc,eostr-scorestr  ; length of string to print
        call 8252           ; print our string
        ret

displaygameover
	ld a,2              ; upper screen
        call 5633           ; open channel
        ld de,gameoverstring        ; address of string
        ld bc,goeostr-gameoverstring  ; length of string to print
        call 8252           ; print our string
        ret	

resetscore
	ld hl,score
	ld b,6
	ld a,'0'
resetl	ld (hl),a
	inc hl
	djnz resetl
	ret


string		defb 22,0,11,'HIGH SCORE'
		defb 22,0,5,'1UP'
		defb 16,2
		defb 22,1,13
		defb '000000'
scorestr	defb 22,1,3
		defb 16,2
score		defb '000000'
eostr		equ $

gameoverstring	defb 22,16,11,'GAME OVER!'
goeostr		equ $


		defb 'NUM OF SPRITES:'
numsprites	db 56			;number of sprites (inc player & missile & alien missiles)

spritesperrow	db 10, 10 ,10, 8, 6, 4	;last was 4? number of sprites on each row

		defb 'SPRITE DATA BLOCKS - 8 bytes per block:'
spdata
		
		;dw 0			;x pos of sprite 0
		;dw 0			;y pos
		;db 0			;flags
		;db 0			;pattern
		;db 0			;counter (used for explosions)
		;db 0			;swarm counter (+7)
		;dw 0			;shadow x (+8 +9)
		;dw 0			;shadow y (+10 +11)
		;db 0			;swarm direction (+12)
		;db 0			;convoy score (+13)
		;db 0			;swarm score (+14)

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

token1		ds DATABLKSZ
token2		ds DATABLKSZ
token3		ds DATABLKSZ


		db 'END OF SPRITE DATA'

hitcount
	db 0
		;Numbers start at fontdata+128
		;Letters start at fontdata+264
       		ORG fontdata
       		INCBIN "shortsfuse-font.bin"

END start