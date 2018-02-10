		; Hyper Olympic / Track and Field - Hello World!
		; Hello World program by Shoestring
		; make sure you speciy the -r switch to set the reset/irq vector
		

		org $e000 ; socket 5a is mapped from 0xe000 - 0xffff
		clr   $1087  ; clear irq mask
		lds   #$1F80 ; init the stack pointer
		
		jsr clrvram ; clears video ram
 		jsr clrcram ; clears color ram
 		
		 
		leax hello,pcr
		leax a,x 
		ldy #$3101 ; screen position
		jsr printText

finished:
		sta $1000 ; write watchdog 
		jmp finished	
		
;---- most text of what we need is in the desired order, except for some tiles		 
printText:
		lda ,x+
		cmpa #$110  ; use this as a string terminator
		beq textdone
		cmpa #$20   ; if it's a space
		beq doSpace ; set index tile to 0x10 
		cmpa #$21   ; exclamation mark
		beq doExcl 
		; add your own tile indexes here if you want!
		suba #48    ; default - convert ascii code to tile index.
done:		 
		sta  ,y+
		jmp printText
doSpace:
		lda #$10
		jmp done
doExcl:
		lda #$68
		jmp done
textdone:
		clra
		rts

;CLEAR SCREEN	
clrvram:		
		ldx   #$3000
		lda   #$10 
loopvram:
		sta   ,X+
		sta   $1000 ; watchdog
		cmpx  #$3800
		bne   loopvram
		rts
		
;RESET color RAM	
clrcram:		
		ldx   #$3800
		lda   #$00
loopcram:
		sta   ,X+
		sta   $1000 ; watchdog
		cmpx  #$4000
		bne   loopcram
		rts		
	
hello	fcc     "SAY HI TO THE KONAMI1!"
		fcb     $110 ;string terminator
		