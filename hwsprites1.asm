	   org 6000H
ATTRP	EQU 5C8DH	    ;SYSTEM PAPER 
ROMCLS	EQU 0D6BH	    ;ROM CLS Routine
ROMBDR	EQU 229BH	    ;ROM BORDER Routine
VISIBLE	EQU 2		    ;OLD FORMAT SPRITES FLAGS - 1=visible, 0=X_MSB
FLAGS	EQU 0		    ;NEW FORMAT SPRITES FLAGS - 0-7=palette_offset, 3=x_mirror, 2=y_mirror, 1=rotate, 0=X_MSB
			    ;	
SPR_VISIBLE EQU 128	    ;NEW FORMAT SPRITES - VISIBLE ATTRIBUTE GOES WITH PATTERN
start   ld hl,ATTRP	    ;Set the PAPER and BORDER to Black, ink to bright white
	ld (hl),71
	ld a,3
	call ROMBDR
	call ROMCLS

	ld a,2              ; upper screen
        call 5633           ; open channel
	call newfont	    ; Define new font
        ld de,string        ; address of string
        ld bc,eostr-string  ; length of string to print
        call 8252           ; print ;our string

        call loadspimages   ; load the sprite image data
        call initspdata     ; initialise the sprite info blocks to 0
        call startspdata    ; set start positions of sprites (OLD FORMAT SPRITES)
       ;call newstartspdata ; set start positions of sprites (NEW FORMAT SPRITES)
        call displaysp	    ; display the sprites

loop	ld ix,spdata
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
	ld bc,6		   ;now deal with sprite 1
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

mp1	add ix,bc	   ;now deal with PLAYER 1
	ld bc,64510
	in a,(c)
	rra
	jr c,notq	   ;Q not pressed so jump
	ld a,(ix+3)
	dec a
	cp 96
	jr c,notq	   ;Cant move any higher so jump
	ld (ix+3),a
notq
	ld bc,65022
	in a,(c)
	rra
	jr c,nota	   ;A not pressed so jump
	ld a,(ix+3)
	inc a
	cp 147
	jr z,nota	   ;cant move any lower so jump
	ld (ix+3),a
nota	
	ld bc,57342
	in a,(c)
	rra
	push af
	jr c, notp	  ;P not pressed so jump
	ld a,(ix+1)
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
	cp 50
	jr c,noto	  ;cant move more left so jump
	ld (ix+1),a
noto
	call displaysp
	halt
	jp loop




        ret
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
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
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


galax1	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 03eh, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 0FAh, 018h, 0FAh, 018h, 0e3h, 0e3h, 03eh, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 018h, 018h, 018h, 018h, 018h, 0e3h, 0e3h, 03eh, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 03h, 018h, 018h, 018h, 018h, 018h, 03h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 018h, 018h, 018h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h, 018h, 0e3h, 0e3h, 0e3h, 03h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 03h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 03h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h
	defb 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h, 0e3h

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

		ld bc,85		;read 255 bytes of image data and write to port 85
		ld e,0ffh
sploop1		ld a,(hl)
		out (c),a
		inc hl
		dec e
		jr nz, sploop1
		ld a,(hl)		;read 256th byte and write to port 85
		out (c),a
		ret

initspdata	ld a,(numsprites)	; write 6 bytes of 0 for each sprite 
		ld hl,spdata
		ld de,spdata+1
inloop1		ld bc,6
		push af
		xor a
		ld (hl),a
		ldir
		pop af
		dec a
		jr nz, inloop1
		ret

startspdata	ld ix,spdata		;set the starting params for each sprite
		ld a,10
		ld (ix+1),a		;x
		ld (ix+3),a		;y
		ld a, VISIBLE
		ld (ix+4),a		;flags
		xor a
		ld (ix+5),a		;pattern
		ld bc,6			; move onto next sprite
		add ix,bc
		ld hl,304
		ld (ix+0),h
		ld (ix+1),l
		ld a,208
		ld (ix+3),a
		ld a,VISIBLE
		ld (ix+4),a
		ld a,16
		ld (ix+5),a
		add ix,bc		; do sprite 2
		ld a,VISIBLE
		ld (ix+4),a
		ld a,50
		ld (ix+3),a
		ld a,40
		ld (ix+5),a	
		add ix,bc
		ld hl,304		; do sprite 3
		ld (ix+0),h
		ld (ix+1),l
		ld a,208
		ld (ix+3),a
		ld a,12
		ld (ix+5),a
		ld a,VISIBLE
		ld (ix+4),a
		add ix,bc		;Set PLAYER 1 positions
		ld a,140
		ld (ix+1),a
		ld a,120
		ld (ix+3),a
		ld a,VISIBLE
		ld (ix+4),a
		ld a,3
		ld (ix+5),a
		ret

newstartspdata	ld ix,spdata		;set the starting params for each sprite - NEW FORMAT SPRITES
		ld a,10
		ld (ix+1),a		;x
		ld (ix+3),a		;y
		ld a, FLAGS
		ld (ix+4),a		;flags
		xor a
		or SPR_VISIBLE
		ld (ix+5),a		;pattern
		ld bc,6			; move onto next sprite
		add ix,bc
		ld hl,304
		ld (ix+0),h
		ld (ix+1),l
		ld a,208
		ld (ix+3),a
		ld a,FLAGS
		ld (ix+4),a
		ld a,16
		or SPR_VISIBLE
		ld (ix+5),a
		add ix,bc		; do sprite 2
		ld a,FLAGS
		ld (ix+4),a
		ld a,50
		ld (ix+3),a
		ld a,40
		or SPR_VISIBLE
		ld (ix+5),a	
		add ix,bc
		ld hl,304		; do sprite 3
		ld (ix+0),h
		ld (ix+1),l
		ld a,208
		ld (ix+3),a
		ld a,12
		or SPR_VISIBLE
		ld (ix+5),a
		ld a,FLAGS
		ld (ix+4),a
		add ix,bc		;Set PLAYER 1 positions
		ld a,140
		ld (ix+1),a
		ld a,120
		ld (ix+3),a
		ld a,FLAGS
		ld (ix+4),a
		ld a,3
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
		ld bc,6
		add ix,bc
		dec e
		ld a,d
		sub e
		cp d
		jr nz,dloop1
		ret


numsprites	db 5			;number of sprites

spdata		dw 0			;x pos of sprite 0
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

END start