	   org 6000H


start  ld a,2              ; upper screen
       call 5633           ; open channel
	   call newfont		   ; Define new font
loop   ld de,string        ; address of string
       ld bc,eostr-string  ; length of string to print
       call 8252           ; print our string
       jp loop             ; repeat until screen is full

string defb 'Phil is coolish'
	   defb 13
eostr  equ $

fontdata 	EQU 60000
newfont:    ld hl,15616         ; ROM font.
      		ld de,fontdata         ; address of our font.
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