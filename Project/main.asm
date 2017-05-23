; COMP2121 17S1 PROJECT
; AUTHOR: JAMES LIN & ANDRE RONG
; GROUP: J11
; LAB: THURSDAY 9-11AM
; ELECTRICAL ENGINEERING BUILDING 233

;---------------------------------------------------------------------------------------------------
; BOARD SET-UP
;---------------------------------------------------------------------------------------------------

;SEE USER MANUAL

;---------------------------------------------------------------------------------------------------
; REGISTERS
;---------------------------------------------------------------------------------------------------
.def enableTimer2 = r11
.def enableButtons = r12
.def enablePot = r13
.def keypadInput = r14
.def enableKeypad = r15
.def temp1 = r16
.def temp2 = r17
.def debounce = r18
.def row = r19
.def col = r20
.def rmask = r21
.def cmask = r22

;---------------------------------------------------------------------------------------------------
; CONSTANTS
;---------------------------------------------------------------------------------------------------
;LCD
.equ LCD_RS = 7			;Register Select
.equ LCD_E = 6			;Enable
.equ LCD_RW = 5			;Read/Write
.equ F_CPU = 16000000	;16MHz
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
;KEYPAD
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F
;---------------------------------------------------------------------------------------------------
; MACROS
;---------------------------------------------------------------------------------------------------
; Clear Data Memory
.macro clear
	ldi YL, low(@0)
	ldi YH, high(@0)
	clr temp1
	st Y+, temp1
	st Y, temp1
.endmacro

;Perform LCD COMMAND
.macro do_lcd_command
	ldi temp1, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

;WRITE TO LCD
.macro do_lcd_data
	ldi temp1, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

;SET BIT
.macro lcd_set
	sbi PORTA, @0
.endmacro

;CLEAR BIT
.macro lcd_clr
	cbi PORTA, @0
.endmacro

;PRINT ASCII VALUE TO LCD
.macro print_digit
	mov temp2, @0
	ldi temp1, 48		;ASCII conversion
	add temp1, temp2
	rcall lcd_data		;Print, increment
	rcall lcd_wait	
.endmacro
;---------------------------------------------------------------------------------------------------
; PROGRAM ADDRESSES
;---------------------------------------------------------------------------------------------------
.org 0x0000
	jmp RESET
.org OVF2addr
	jmp TIMER2OVF
.org OVF0addr
	jmp TIMER0OVF
;---------------------------------------------------------------------------------------------------
; DATA MEMORY ALLOCATIONS
;---------------------------------------------------------------------------------------------------
.dseg
DebounceCounter:
	.byte 2
ThreeSecondCounter:
	.byte 2
.cseg
;---------------------------------------------------------------------------------------------------
; RESET//SET-UP
;---------------------------------------------------------------------------------------------------
; 1. CLEAR VARIABLES
; 2. INITIALISE STACK POINTER
; 3. SET-UP TIMER0
; 4. SET-UP LCD
; 5. SET-UP LED
; 6. SET-UP KEYPAD
RESET:
	; 1
	clr enableTimer2
	clr enableButtons
	clr enablePot
	clr keypadInput
	clr enableKeypad
	clr temp1
	clr temp2
	clr debounce
	clr row
	clr col
	clr rmask
	clr cmask

	; 2
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	; 3
	; TIMER 0 - Overflow Timer - Prescale 8 (128ms)
	ldi temp1, 0x00	
	out TCCR0A, temp1
	ldi temp1, 0x02
	out TCCR0B, temp1
	ldi temp1, 1<<TOIE0
	sts TIMSK0, temp1

	; TIMER 2 - Overflow Timer - Prescale 8 (128ms)
	ldi temp1, 0x00
	sts TCCR2A, temp1
	ldi temp1, 0x02
	sts TCCR2B, temp1
	ldi temp1, 1<<TOIE2
	sts TIMSK2, temp1

	; 4
	ser temp1
	out DDRF, temp1
	out DDRA, temp1
	clr temp1
	out PORTF, temp1
	out PORTA, temp1

	;2X5X7, INCREMENT, CURSOR OFF, BAR OFF, NO BLINK
	do_lcd_command 0b00111000
	rcall sleep_5ms
	do_lcd_command 0b00111000
	rcall sleep_1ms
	do_lcd_command 0b00111000
	do_lcd_command 0b00111000
	do_lcd_command 0b00001000 ; Display off
	do_lcd_command 0b00000001 ; Clear display
	do_lcd_command 0b00000110 ; Entry Mode set: increment, no display shift
	do_lcd_command 0b00001100 ; Display on: Cursor on, bar, no blink
	
	; 5
	ser temp1
	out DDRC, temp1
	out DDRG, temp1
	out PORTC, temp1
	out PORTG, temp1

	; 6
	ldi temp1, PORTLDIR
	sts DDRL, temp1
	ser temp1
;---------------------------------------------------------------------------------------------------
; MAIN//BODY
;---------------------------------------------------------------------------------------------------
main:
	rcall startScreen
loop1:
	ldi temp1, 1
	mov enableKeypad, temp1
	mov enableTimer2, temp1
	;rcall checkKeypad
	clr temp1
	cp enableTimer2, temp1
	breq showSelectScreen
	rjmp loop1

showSelectScreen:
	rcall selectScreen
end:
	rjmp end
;---------------------------------------------------------------------------------------------------
; INTERRUPTS
;---------------------------------------------------------------------------------------------------
TIMER0OVF:
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24

	lds r24, DebounceCounter
	lds r25, DebounceCounter+1
	adiw r25:r24, 1
	cpi r24, low(1953)			;250ms
	ldi temp1, high(1953)
	cpc r25, temp1
	brne notDebounce

debounced:
	clear DebounceCounter
	clr debounce
	rjmp timer0Epilogue

notDebounce:
	sts DebounceCounter, r24
	sts DebounceCounter+1, r25
	rjmp timer0Epilogue

timer0Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	reti

TIMER2OVF:
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24

	ldi temp1, 0
	cp enableTimer2, temp1
	brne startTimer2
	rjmp timer2Epilogue

startTimer2:
	lds r24, ThreeSecondCounter
	lds r25, ThreeSecondCounter+1
	adiw r25:r24, 1
	cpi r24, low(23436)			;3s
	ldi temp1, high(23436)
	cpc r25, temp1
	brne notThreeSecond

threeSecond:
	clear ThreeSecondCounter
	clr enableTimer2
	rjmp timer2Epilogue

notThreeSecond:
	sts ThreeSecondCounter, r24
	sts ThreeSecondCounter+1, r25
	rjmp timer2Epilogue

timer2Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	reti
;---------------------------------------------------------------------------------------------------
; FUNCTIONS
;---------------------------------------------------------------------------------------------------
lcd_command:
	out PORTF, temp1	;Output PORTF with data
	rcall sleep_1ms
	lcd_set LCD_E		;Set Enable bit
	rcall sleep_1ms
	lcd_clr LCD_E		;Clear Enable bit
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, temp1	;Output PORTF with data
	lcd_set LCD_RS		;RS = 1; RW = 0 to write data
	rcall sleep_1ms		
	lcd_set LCD_E		;Set Enable bit
	rcall sleep_1ms
	lcd_clr LCD_E		;Clear Enable bit
	rcall sleep_1ms
	lcd_clr LCD_RS		;RS = 0; RW = 0
	ret

lcd_wait:				;Busy flag
	push temp1
	clr temp1
	out DDRF, temp1		;Port F as output
	out PORTF, temp1
	lcd_set LCD_RW		;RS = 0; RW = 1 (needed for busy mode)
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E		;Set Enable bit
	rcall sleep_1ms
	in temp1, PINF		;Retrive data from PINF
	lcd_clr LCD_E		;Clear Enable bit
	sbrc temp1, 7         ;Check busy flag
	rjmp lcd_wait_loop
	lcd_clr LCD_RW		;Clear LCD_RW
	ser temp1
	out DDRF, temp1		;Set Port F to output
	pop temp1
	ret

; 4 cycles per iteration - setup/call-return overhead
sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1	
	brne delayloop_1ms
	pop r25
	pop r24
	ret
sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret
sleep_25ms:
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	ret
sleep_100ms:
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	ret
sleep_500ms:
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	ret

; DISPLAY MESSAGE ON LCD FOR 3 SECONDS OR UNTIL KEYPAD INPUT (INCOMPLETE)
startScreen:
	do_lcd_command 0b00000001 ; Clear display
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data '1'
	do_lcd_data '7'
	do_lcd_data 's'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data 'J'
	do_lcd_data '1'
	do_lcd_data '1'
	do_lcd_command 0b11000000		;Next Line
	do_lcd_data 'V'
	do_lcd_data 'e'
	do_lcd_data 'n'
	do_lcd_data 'd'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'M'
	do_lcd_data 'a'
	do_lcd_data 'c'
	do_lcd_data 'h'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'e'
	ret

selectScreen:
	do_lcd_command 0b00000001 ; Clear display
	do_lcd_data 'S'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'e'
	do_lcd_data 'c'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'I'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'm'
	ret

; SCAN KEYPAD AND RETURN INPUT TO KEYPADINPUT
checkKeypad:
	clr r0
	cp enableKeypad, r0
	brne checkKeypadDebounce
	ret

checkKeypadDebounce:
	cpi debounce, 0
	breq startCheckKeypad
	ret

startCheckKeyPad:
	ldi debounce, 1

	ldi cmask, INITCOLMASK
	clr col
	colloop:
		cpi col, 4
		breq startCheckKeypad
		sts PORTL, cmask
		ldi temp1, 0xFF
	delayCheck:
		dec temp1
		brne delayCheck
		lds temp1, PINL
	handleKey:
		andi temp1, ROWMASK
		cpi temp1, 0xF
		breq nextcol
		ldi rmask, INITROWMASK
		clr row
	rowloop:
		cpi row, 4
		breq nextcol
		mov temp2, temp1
		and temp2, rmask
		breq convert
		inc row
		lsl rmask
		jmp rowloop
	nextcol:
		lsl cmask
		inc col
		jmp colloop
	convert:
		cpi col, 3
		breq letters
		cpi row, 3
		breq symbols
		mov temp1, row
		lsl temp1
		add temp1, row
		add temp1, col
		subi temp1, -1
		jmp finishCheckKeypad
	letters:
		ldi temp1, 0x0A
		add temp1, row
		jmp finishCheckKeypad
	symbols:
		cpi col, 0
		breq star
		cpi col, 1
		breq zero
		ldi temp1, 0x0F
		jmp finishCheckKeypad
	star:
		ldi temp1, 0x0E
		jmp finishCheckKeypad
	zero:
		clr temp1
		jmp finishCheckKeypad
	finishCheckKeypad:
		mov keypadInput, temp1
		ret

