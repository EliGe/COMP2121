; COMP2121 17S1 PROJECT
; AUTHOR: JAMES LIN & ANDRE RONG
; GROUP: J11
; LAB: THURSDAY 9-11AM
; ELECTRICAL ENGINEERING BUILDING 233

;---------------------------------------------------------------------------------------------------
; BOARD SET-UP
;---------------------------------------------------------------------------------------------------

;SEE USER MANUAL
.include "m2560def.inc"

;---------------------------------------------------------------------------------------------------
; REGISTERS
;---------------------------------------------------------------------------------------------------
.def potValueL = r4
.def potValueH = r5
.def isPressed0 = r6
.def isPressed1 = r7
.def inventoryCount = r8
.def inventoryCost = r9
.def enableTimer1 = r10
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
.def moduleSelector = r23
.def currentModule = r24

;---------------------------------------------------------------------------------------------------
; CONSTANTS
;---------------------------------------------------------------------------------------------------
;LCD
.equ LCD_RS = 7			;Register Select
.equ LCD_E = 6			;Enable
.equ LCD_RW = 5			;Read/Write
.equ F_CPU = 16000000	;16MHz
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4

;TIMER (Prescale 8)
.equ tenthSecond = 3		;16-bit
.equ quarterSecond = 1953	;8-bit
.equ oneSecond = 7812		;8-bit
.equ oneHalfSecond = 46		;16-bit
.equ twoSeconds = 15624		;8-bit
.equ threeSeconds = 23436	;8-bit

;KEYPAD
.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F

;MODULES [To store in Module Selector]
;If Module Selector = 0, it will return from function
;If Module Selector = {1-6} it will jump to that module instead of returning
.equ startScreenM = 1
.equ selectScreenM = 2
.equ emptyScreenM = 3
.equ adminModeM = 4
.equ coinScreenM = 5
.equ deliveryScreenM = 6

;ITEMS - STARTING INVENTORY
.equ costOdd = 1
.equ costEven = 2
.equ inventory1 = 1
.equ inventory2 = 2
.equ inventory3 = 3
.equ inventory4 = 4
.equ inventory5 = 5
.equ inventory6 = 6
.equ inventory7 = 7
.equ inventory8 = 8
.equ inventory9 = 0
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
.org INT0addr
	jmp EXT_INT0
.org INT1addr
	jmp EXT_INT1
.org OVF2addr
	jmp TIMER2OVF
.org OVF1addr
	jmp TIMER1OVF
.org OVF0addr
	jmp TIMER0OVF
.org 0x003A			;ADC addr
	jmp EXT_POT
.org OVF3Addr
	jmp TIMER3OVF
;---------------------------------------------------------------------------------------------------
; DATA MEMORY ALLOCATIONS
;---------------------------------------------------------------------------------------------------
.dseg
DebounceCounter:
	.byte 2
OneHalfSecondCounter:
	.byte 2
ThreeSecondCounter:
	.byte 2
PotCounter:
	.byte 2

; ITEMS: 1 byte for CAPACITY, 1 byte for PRICE
Item1:
	.byte 2
Item2:
	.byte 2
Item3:
	.byte 2
Item4:
	.byte 2
Item5:
	.byte 2
Item6:
	.byte 2
Item7:
	.byte 2
Item8:
	.byte 2
Item9:
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
; 7. SET_UP EXTERNAL INTERRUPTS
; 8. SET_UP POTENTIOMETER
; 9. SET-UP STARTING INVENTORY
RESET:
	; 1
	clr potValueL
	clr potValueH
	clr isPressed0
	clr isPressed1
	clr inventoryCount
	clr inventoryCost
	clr enableTimer1
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
	clr moduleSelector
	clr currentModule

	; 2
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	; 3: TIMER 0,1,2,3 - Overflow Timer - Prescale 8 (128ms)
	ldi temp1, 0x00
	out TCCR0A, temp1
	sts TCCR1A, temp1
	sts TCCR2A, temp1
	sts TCCR3A, temp1
	ldi temp1, 0x02
	out TCCR0B, temp1
	sts TCCR1B, temp1
	sts TCCR2B, temp1
	sts TCCR3B, temp1
	ldi temp1, (1<<TOIE0)
	sts TIMSK0, temp1
	ldi temp1, (1<<TOIE1)
	sts TIMSK1, temp1
	ldi temp1, (1<<TOIE2)
	sts TIMSK2, temp1
	ldi temp1, (1<<TOIE3)
	sts TIMSK3, temp1

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

	; 7 PB0 - EXT_INT0; PB1 - EXT_INT1
	ldi temp1, (1<<ISC11) | (1<<ISC01) ;Set Falling Edge
	sts EICRA, temp1
	in temp1, EIMSK
	ori temp1, (1<<INT1) | (1<<INT0)
	out EIMSK, temp1

	; 8
	; ADEN -> Enable Bit
	; ADPS = 6 -> ADC Prescaler 128 (Ideally between 80-320 for max resolution)
	; ADLAR -> 0 means ADCH has 2 bits; ADCL has 8 bits
	; ADIE -> Interrupt Enable
	; REFS0 = 3 -> 2.56V with capacitor
	; MUX -> Single ended input ADC8
	; ADSC -> ADC Start Conversion

	; Operation: Interrupts Occurs when ADIE = 1 and I (in SREG) = 1, by triggering ADIF
	; (ADC Interrupt Flag)
	; To repeat the routine, set 1<<ADSC

	ldi temp1, (3<<REFS0) | (0<<ADLAR) | (0<<MUX0)
	sts ADMUX, temp1
	ldi temp1, (1<<MUX5)
	sts ADCSRB, temp1
	ldi temp1, (1<<ADEN) | (1<<ADSC) | (1<<ADIE) | (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0)
	sts ADCSRA, temp1

	; 9 - Create Starting Inventory for Items 1 - 9
	ldi temp1, inventory1
	ldi temp2, costOdd
	sts Item1, temp1
	sts Item1+1, temp2

	ldi temp1, inventory2
	ldi temp2, costEven
	sts Item2, temp1
	sts Item2+1, temp2

	ldi temp1, inventory3
	ldi temp2, costOdd
	sts Item3, temp1
	sts Item3+1, temp2

	ldi temp1, inventory4
	ldi temp2, costEven
	sts Item4, temp1
	sts Item4+1, temp2

	ldi temp1, inventory5
	ldi temp2, costOdd
	sts Item5, temp1
	sts Item5+1, temp2

	ldi temp1, inventory6
	ldi temp2, costEven
	sts Item6, temp1
	sts Item6+1, temp2

	ldi temp1, inventory7
	ldi temp2, costOdd
	sts Item7, temp1
	sts Item7+1, temp2

	ldi temp1, inventory8
	ldi temp2, costEven
	sts Item8, temp1
	sts Item8+1, temp2

	ldi temp1, inventory9
	ldi temp2, costOdd
	sts Item9, temp1
	sts Item9+1, temp2

;---------------------------------------------------------------------------------------------------
; MAIN//BODY
;---------------------------------------------------------------------------------------------------
start:	;LOAD UP START SCREEN, AND SWITCH TO SELECT SCREEN
	rcall startScreen
startToSelect:
	inc enableKeypad
	inc enableTimer2
	ldi moduleSelector, selectScreenM
	rcall checkKeypad
	rcall ledOff

select:
	rcall selectScreen
	inc enableKeypad
	ldi moduleSelector, 0
	rcall checkKeypad
	
	; IF(0 < keypadInput < 10), continue
	; Otherwise, go back to select
	ldi temp1, 10
	cp keypadInput, temp1
	brsh select
	ldi temp1, 1
	cp keypadInput, temp1
	brlo select

	; IF(KEYPADINPUT -> INVENTORY ITEM == 0), GO TO EMPTY SCREEN
	; OTHERWISE, GO TO COIN SCREEN
	rcall getInventoryInfo
	clr temp1
	cp inventoryCount, temp1
	breq empty
	jmp coin

empty:
	rcall emptyScreen
	jmp select

coin:
	rcall coinScreen
	jmp select

	rjmp select

end:
	rjmp end
;---------------------------------------------------------------------------------------------------
; INTERRUPTS
;---------------------------------------------------------------------------------------------------
EXT_INT0:
	push temp1
	push temp2
	in temp1, SREG
	push temp1
checkEnableButton0:
	clr r0
	cp enableButtons, r0
	brne checkButton0Debounce
	rjmp returnI
checkButton0Debounce:
	cpi debounce, 0
	breq startButtonPress0
	rjmp returnI
	ldi debounce, 1
startButtonPress0:
	inc isPressed0
	clr enableButtons
	rjmp returnI

EXT_INT1:
	push temp1
	push temp2
	in temp1, SREG
	push temp1
checkEnableButton1:
	clr r0
	cp enableButtons, r0
	brne checkButton1Debounce
	rjmp returnI
checkButton1Debounce:
	cpi debounce, 0
	breq startButtonPress1
	rjmp returnI
	ldi debounce, 1
startButtonPress1:
	inc isPressed1
	clr enableButtons
	rjmp returnI
returnI:
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti

TIMER0OVF:
	push temp1
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24

	lds r24, DebounceCounter
	lds r25, DebounceCounter+1
	adiw r25:r24, 1
	cpi r24, low(quarterSecond)			;250ms
	ldi temp1, high(quarterSecond)
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
	pop temp1
	reti

TIMER1OVF:
	push temp1
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24

	; IF ENABLETIMER1 = 1, START THE TIMER
	ldi temp1, 1
	cp enableTimer1, temp1
	breq startTimer1
	rjmp timer1Epilogue

startTimer1:
	lds r24, OneHalfSecondCounter
	lds r25, OneHalfSecondCounter+1
	adiw r25:r24, 1
	cpi r24, low(oneHalfSecond)			;65536*8/16000000 = 32.768ms (per interrupt)
	ldi temp1, high(oneHalfSecond)		;1.5s
	cpc r25, temp1
	brne notOneHalfSecond

;WHEN 3 SECONDS PASS, THEN WE SET ENABLETIMER1 = 2
oneHalfSeconds:
	clear OneHalfSecondCounter
	ldi temp1, 2
	mov enableTimer1, temp1	
	rjmp timer1Epilogue

notOneHalfSecond:
	sts OneHalfSecondCounter, r24
	sts OneHalfSecondCounter+1, r25
	rjmp timer1Epilogue

timer1Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	pop temp1
	reti

TIMER2OVF:
	push temp1
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24

	; IF ENABLETIMER2 = 1, START THE TIMER
	ldi temp1, 1
	cp enableTimer2, temp1
	breq startTimer2
	rjmp timer2Epilogue

startTimer2:
	lds r24, ThreeSecondCounter
	lds r25, ThreeSecondCounter+1
	adiw r25:r24, 1
	cpi r24, low(threeSeconds)			;3s
	ldi temp1, high(threeSeconds)
	cpc r25, temp1
	brne notThreeSecond

;WHEN 3 SECONDS PASS, THEN WE SET ENABLETIMER2 = 2
threeSecond:
	clear ThreeSecondCounter
	ldi temp1, 2
	mov enableTimer2, temp1	
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
	pop temp1
	reti

TIMER3OVF:
;	push temp1
;	in temp1, SREG
;	push temp1
;	push YH
;	push YL
;	push r25
;	push r24
;
;startTimer3:
;	lds r24, PotCounter
;	lds r25, PotCounter+1
;	adiw r25:r24, 1
;	cpi r24, low(tenthSecond)			;100ms
;	ldi temp1, high(tenthSecond)
;	cpc r25, temp1
;	brne notTenthSecond
;
;;WHEN 100MS PASS
;tenthSeconds:
;	clear PotCounter
;
;	ldi temp1, (3<<REFS0) | (0<<ADLAR) | (0<<MUX0)
;	sts ADMUX, temp1
;	ldi temp1, (1<<MUX5)
;	sts ADCSRB, temp1
;	ldi temp1, (1<<ADEN) | (1<<ADSC) | (1<<ADIE) | (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0)
;	sts ADCSRA, temp1
;	
;	lds potValueL, ADCL
;	lds potValueH, ADCH	
;
;	out PORTC, potValueL
;	out PORTG, potValueH
;
;	clr temp1
;	sts ADCL, temp1
;	sts ADCH, temp1
;		
;	rjmp timer3Epilogue
;
;notTenthSecond:
;	sts OneHalfSecondCounter, r24
;	sts OneHalfSecondCounter+1, r25
;	rjmp timer3Epilogue
;
;timer3Epilogue:
;	pop r24
;	pop r25
;	pop YL
;	pop YH
;	pop temp1
;	out SREG, temp1
;	pop temp1
	reti

EXT_POT:
	push temp1
	in temp1, SREG
	push temp1
	
	lds potValueH, ADCH
	lds potValueL, ADCL

	out PORTC, potValueL
	out PORTG, potValueH

	ldi temp1, (1<<ADEN) | (1<<ADSC) | (1<<ADIE) | (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0)
	sts ADCSRA, temp1

	pop temp1
	out SREG, temp1
	pop temp1
	reti
;---------------------------------------------------------------------------------------------------
; FUNCTIONS
;---------------------------------------------------------------------------------------------------

; DISPLAY MESSAGE ON LCD
startScreen:
	ldi currentModule, startScreenM

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
	ldi currentModule, selectScreenM

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

emptyScreen:
	push temp1
	push temp2
	in temp1, SREG
	push temp1

	ldi currentModule, emptyScreenM

	do_lcd_command 0b00000001 ; Clear display
	do_lcd_data 'O'
	do_lcd_data 'u'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'o'
	do_lcd_data 'f'
	do_lcd_data ' '
	do_lcd_data 'S'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'c'
	do_lcd_data 'k'
	do_lcd_command 0b11000000		;Next Line
	print_digit keypadInput

	inc enableButtons
	inc enableTimer1
	
	;LOOP UNTIL 1.5 SECONDS PASSED (LED ON), OR EITHER BUTTON PRESSED
	emptyScreenLoop1:
		rcall ledOn
		ldi temp1, 2
		cp enableTimer1, temp1
		breq emptyScreenLoop2Preq
		ldi temp1, 1
		cp isPressed0, temp1
		breq emptyScreenEnd
		cp isPressed1, temp1
		breq emptyScreenEnd
		jmp emptyScreenLoop1

	;LOOP UNTIL 1.5 SECONDS PASSED (LED OFF), OR EITHER BUTTON PRESSED
	emptyScreenLoop2Preq:
		ldi temp1, 1
		mov enableTimer1, temp1
		emptyScreenLoop2:
			rcall ledOff
			ldi temp1, 2
			cp enableTimer1, temp1
			breq emptyScreenEnd
			ldi temp1, 1
			cp isPressed0, temp1
			breq emptyScreenEnd
			cp isPressed1, temp1
			breq emptyScreenEnd
			jmp emptyScreenLoop2
	emptyScreenEnd:
		rcall ledOff
		clr enableTimer1
		clr enableButtons
		clr isPressed0
		clr isPressed1

		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		ret

coinScreen:
	ldi currentModule, coinScreenM

	do_lcd_command 0b00000001 ; Clear display
	do_lcd_data 'I'
	do_lcd_data 'n'
	do_lcd_data 's'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'c'
	do_lcd_data 'o'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 's'
	do_lcd_command 0b11000000		;Next Line
	print_digit inventoryCost

	; 1. SCAN KEYPAD INPUT
	; 2. SCAN POTENTIOMETER INPUT
	; 3. CHECK COINS -> DISPLAY ON LED
	coinScreenLoop:

		;out PORTC, potValueL
		;out PORTG, potValueH

		;clr temp1
		;clr potValueL
		;clr potValueH
		
		;sts ADCH, temp1
		;sts ADCL, temp1
		;sts ADMUX, temp1
		;sts ADCSRB, temp1
		;sts ADCSRA, temp1
		rcall sleep_500ms
		rcall sleep_500ms
		rcall ledOff
	ret

; SCAN KEYPAD AND RETURN INPUT TO KEYPADINPUT
checkKeypad:
	push temp1
	push temp2
	in temp1, SREG
	push temp1
checkEnableKeypad:
	clr r0
	cp enableKeypad, r0
	brne checkKeypadDebounce
	rjmp return

checkKeypadDebounce:
	cpi debounce, 0
	breq startCheckKeypad
	rjmp return

	ldi debounce, 1

	startCheckKeyPad:
	ldi cmask, INITCOLMASK
	clr col
	colloop:
		ldi temp1, 2			;IF ENABLETIMER2 = 2, THEN 3 SECONDS HAVE PASSED
		cp enableTimer2, temp1
		breq checkKeypadEpilogue

		cpi col, 4				;USUAL COL/ROW LOOPING
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
	checkKeypadEpilogue:
		clr enableTimer2		;RESET/DISABLE TIMER2
		clear ThreeSecondCounter
		clr enableKeypad		;DISABLE KEYPAD

		jmp checkModule
	checkModule:
		cpi moduleSelector, 0	
		breq return	
		cpi moduleSelector, startScreenM
		breq jumpStartScreen
		cpi moduleSelector, selectScreenM
		breq jumpSelectScreen
		;ADD REST LATER

	return:
		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		ret
	jumpStartScreen:
		clr moduleSelector
		rcall startScreen
		jmp return

	jumpSelectScreen:
		clr moduleSelector
		rcall selectScreen
		jmp return
		
; INPUT: KEYPADINPUT (1-9) FOR ITEM
; OUTPUT: ITEM INVENTORY COUNT -> inventoryCount
;         ITEM INVENTORY COST -> inventoryCost
getInventoryInfo:
	push temp1
	push temp2
	in temp1, SREG
	push temp1

	ldi temp2, 1

	cp keypadInput, temp2
	breq inventoryCountItem1
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem2
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem3
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem4
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem5
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem6
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem7
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem8
	inc temp2
	cp keypadInput, temp2
	breq inventoryCountItem9

inventoryCountItem1:
	lds inventoryCount, Item1
	lds inventoryCost, Item1+1
	jmp inventoryCountEnd
inventoryCountItem2:
	lds inventoryCount, Item2
	lds inventoryCost, Item2+1
	jmp inventoryCountEnd
inventoryCountItem3:
	lds inventoryCount, Item3
	lds inventoryCost, Item3+1
	jmp inventoryCountEnd
inventoryCountItem4:
	lds inventoryCount, Item4
	lds inventoryCost, Item4+1
	jmp inventoryCountEnd
inventoryCountItem5:
	lds inventoryCount, Item5
	lds inventoryCost, Item5+1
	jmp inventoryCountEnd
inventoryCountItem6:
	lds inventoryCount, Item6
	lds inventoryCost, Item6+1
	jmp inventoryCountEnd
inventoryCountItem7:
	lds inventoryCount, Item7
	lds inventoryCost, Item7+1
	jmp inventoryCountEnd
inventoryCountItem8:
	lds inventoryCount, Item8
	lds inventoryCost, Item8+1
	jmp inventoryCountEnd
inventoryCountItem9:
	lds inventoryCount, Item9
	lds inventoryCost, Item9+1
	jmp inventoryCountEnd
inventoryCountEnd:
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	ret

ledOn:
	push temp1
	in temp1, SREG
	push temp1

	ser temp1
	out PORTC, temp1
	out PORTG, temp1

	pop temp1
	out SREG, temp1
	pop temp1
	ret
ledOff:
	push temp1
	in temp1, SREG
	push temp1

	clr temp1
	out PORTC, temp1
	out PORTG, temp1

	pop temp1
	out SREG, temp1
	pop temp1
	ret

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