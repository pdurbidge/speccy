	   org 6000H
ATTRP	EQU 5C8DH	    ;SYSTEM PAPER 
ROMCLS	EQU 0D6BH	    ;ROM CLS Routine
ROMBDR	EQU 229BH	    ;ROM BORDER Routine

start   ld hl,ATTRP	    ;Set the PAPER and BORDER to Black
	ld (hl),0
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
        ret

string  defb 22,10,10	    ; @ 10,13
	defb 16, 3	    ; Magenta Ink
	defb 'Phil is cool'
	defb 22,14,10
	defb 16, 5
	defb 'ANYA WAS HERE'
eostr  equ $

fontdata 	EQU 60000
newfont:    ld hl,15616             ; ROM font.
      		ld de,fontdata      ; address of our font.
       		ld bc,768           ; 96 chars * 8 rows to alter.
font1  		ld a,(hl)           ; get bitmap.
       		rlca                ; rotate it left.
       		or (hl)             ; combine 2 images.
       		ld (de),a           ; write to new font.
       		inc hl              ; next byte of old.
       		inc de              ; next byte of new.
       		dec bc              ; decrement counter.
       		ld a,b              ; high byte.
       		or c                ; combine with low byte.
       		jr nz,font1         ; repeat until bc=zero.
       		ld hl,fontdata-256     ; font minus 32*8.
       		ld (23606),hl       ; point to new font.
       		ret

END start