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
.def coinCount = r2
.def enableMotor = r3
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

;MODULES
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
.equ inventory9 = 9
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
.org 0x003A			;ADC ADDR
	jmp EXT_POT
.org OVF3addr
	jmp TIMER3OVF
.org OVF4addr
	jmp TIMER4OVF
;---------------------------------------------------------------------------------------------------
; DATA MEMORY ALLOCATIONS
;---------------------------------------------------------------------------------------------------
.dseg
DebounceCounter:		;250ms
	.byte 2
OneHalfSecondCounter:	;1.5s
	.byte 2
ThreeSecondCounter:		;3s
	.byte 2
PotCounter:				;100ms
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
; 3. SET-UP TIMERS
; 4. SET-UP LCD
; 5. SET-UP LED
; 6. SET-UP KEYPAD
; 7. SET_UP EXTERNAL INTERRUPTS
; 8. SET_UP POTENTIOMETER
; 9. SET-UP STARTING INVENTORY
RESET:
	; 1 - CLEAR REGISTERS
	clr coinCount
	clr enableMotor
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

	; 2 - STACK POINTER
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	; 3 - TIMERS
	; TIMER 0,1,2,4 - OVERFLOW TIMER - PRESCALE 8 (128MS)
	ldi temp1, 0x00
	out TCCR0A, temp1
	sts TCCR1A, temp1
	sts TCCR2A, temp1
	sts TCCR4A, temp1
	ldi temp1, 0x02
	out TCCR0B, temp1
	sts TCCR1B, temp1
	sts TCCR2B, temp1
	sts TCCR4B, temp1
	ldi temp1, (1<<TOIE0)
	sts TIMSK0, temp1
	ldi temp1, (1<<TOIE1)
	sts TIMSK1, temp1
	ldi temp1, (1<<TOIE2)
	sts TIMSK2, temp1
	ldi temp1, (1<<TOIE4)
	sts TIMSK4, temp1

	; TIMER 3 - PWM TIMER: PHASE CORRECT, PRESCALE 256
	ser temp1				
	out DDRE, temp1

	ldi temp1, (1<<WGM30) | (1<<COM3B1)
	sts TCCR3A, temp1
	ldi temp1, 0b00000100
	sts TCCR3B, temp1
	ldi temp1, 1<<OCIE3B
	sts TIMSK3, temp1

	; 4 - LCD
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
	
	; 5 - LED
	ser temp1
	out DDRC, temp1
	out DDRG, temp1
	out PORTC, temp1
	out PORTG, temp1

	; 6 - KEYPAD
	ldi temp1, PORTLDIR
	sts DDRL, temp1
	ser temp1

	; 7 - PUSH BUTTONS
	; PB0 - EXT_INT0; PB1 - EXT_INT1
	ldi temp1, (1<<ISC11) | (1<<ISC01) ;Set Falling Edge
	sts EICRA, temp1
	in temp1, EIMSK
	ori temp1, (1<<INT1) | (1<<INT0)
	out EIMSK, temp1

	; 8 - POTENTIOMETER | ADC
	; ADEN -> Enable Bit
	; ADPS = 6 -> ADC Prescaler 128 (Ideally between 80-320 for max resolution)
	; ADLAR -> 0 means ADCH has 2 bits; ADCL has 8 bits
	; ADIE -> Interrupt Enable
	; REFS0 = 3 -> 2.56V with capacitor
	; MUX -> Single ended input ADC8
	; ADSC -> ADC Start Conversion

	; To repeat the routine, set 1<<ADSC
	ldi temp1, (3<<REFS0) | (0<<ADLAR) | (0<<MUX0)
	sts ADMUX, temp1
	ldi temp1, (1<<MUX5)
	sts ADCSRB, temp1
	ldi temp1, (1<<ADEN) | (1<<ADSC) | (1<<ADIE) | (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0)
	sts ADCSRA, temp1

	; 9 - CREATE STARTING INVENTORY
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
; SIMPLY LOAD UP START SCREEN
start:
	rcall startScreen
; TRANSITION FROM START SCREEN TO SELECT SCREEN
startToSelect:
	; "Enable bits" for the checkKeypad Function
	inc enableKeypad
	inc enableTimer2
	ldi moduleSelector, selectScreenM

	rcall checkKeypad
	rcall ledOff
; 1. LOAD UP SELECT SCREEN
; 2. SCAN FOR KEYPAD INPUT [1-9 INCLUSIVE]
; 3. CHECK IF INVENTORY IS EMPTY OR NOT EMPTY
select:
	; 1
	rcall selectScreen
	; 2
	inc enableKeypad
	ldi moduleSelector, 0
	rcall checkKeypad
	ldi temp1, 10
	cp keypadInput, temp1
	brsh select
	ldi temp1, 1
	cp keypadInput, temp1
	brlo select
	; 3
	rcall getInventoryInfo
	clr temp1
	cp inventoryCount, temp1
	breq empty
	jmp coin
; EMPTY INVENTORY
empty:
	rcall emptyScreen
	jmp select
; NON-EMPTY INVENTORY
coin:
	rcall coinScreen
	jmp select
; TECHNICALLY, THE CODE SHOULD NEVER REACH THIS POINT
end:
	rjmp end
;---------------------------------------------------------------------------------------------------
; INTERRUPTS
;---------------------------------------------------------------------------------------------------
; PUSH BUTTON PB0
; 1. PUSH
; 2. CHECK ENABLE BUTTON REGISTER
; 3. CHECK DEBOUNCING
; 4. CHANGE STATE TO "PRESSED"
; 5. POP
EXT_INT0:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	 ; 2
	 ; IF(ENABLEBUTTONS == 0), JUMP TO END
	 ; IF(ENABLEBUTTONS != 0), CONTINUE ON
checkEnableButton0:
	clr r0
	cp enableButtons, r0
	brne checkButton0Debounce
	rjmp returnI
	; 3
	; IF(DEBOUNCE == 0), JUMP TO END
	; IF(DEBOUNCE != 0), CONTINUE ON
checkButton0Debounce:
	cpi debounce, 0
	breq startButtonPress0
	rjmp returnI
	ldi debounce, 1
	; 4
	; SIMPLY STATE THAT BUTTON IS PRESSED
startButtonPress0:
	inc isPressed0
	clr enableButtons
	rjmp returnI

; PUSH BUTTON PB1
; SAME PROCEDURE AS EXT_INT0
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
; 5
returnI:
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti

; TIMER0 - USED FOR DEBOUNCING AT 250MS
; 1. PUSH
; 2. LOAD DATA INTO REGISTER
; 3. CHECK IF 250MS HAS PASSED
; 4. POP
TIMER0OVF:
	; 1
	push temp1
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24
	; 2
	lds r24, DebounceCounter
	lds r25, DebounceCounter+1
	adiw r25:r24, 1
	; 3
	cpi r24, low(quarterSecond)
	ldi temp1, high(quarterSecond)
	cpc r25, temp1
	brne notDebounce
; 250MS PASSED
; CLEAR COUNTER
; CLEAR DEBOUNCE REGISTER
debounced:
	clear DebounceCounter
	clr debounce
	rjmp timer0Epilogue
; 250MS NOT PASSED
; INCREMENT COUNTER
notDebounce:
	sts DebounceCounter, r24
	sts DebounceCounter+1, r25
	rjmp timer0Epilogue
; 4
timer0Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	pop temp1
	reti

; TIMER 1 - USED FOR TIMING 1.5s
; MOSTLY THE SAME PROCEDURE AS TIMER0OVF
; ENABLETIMER1 BIT DETERMINES WHETHER TIMER WILL OPERATE
; ENABLETIMER1 == 0, DISABLE TIMER1
; ENABLETIMER1 == 1, START TIMER1
; ENABLETIMER1 == 2, 1.5s HAS BEEN COUNTED
TIMER1OVF:
	; 1
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
	; 2
	lds r24, OneHalfSecondCounter
	lds r25, OneHalfSecondCounter+1
	adiw r25:r24, 1
	; 3
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
; 4
timer1Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	pop temp1
	reti

; TIMER 2 - USED FOR TIMING 3s
; OPERATES EXACTLY THE SAME AS TIMER1OVF
TIMER2OVF:
	; 1
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
	; 2
	lds r24, ThreeSecondCounter
	lds r25, ThreeSecondCounter+1
	adiw r25:r24, 1
	; 3
	cpi r24, low(threeSeconds)
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
; 4
timer2Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	pop temp1
	reti

; TIMER 3 - PWM TIMER FOR MOTOR
; 1. PUSH
; 2. CHECK STATE OF ENABLEMOTOR
; 3. POP
TIMER3OVF:
	; 1
	push temp1
	in temp1, SREG
	push temp1
; 2
; IF(ENABLEMOTOR == 1), RUN MOTOR AT FULL SPEED
; IF(ENABLEMOTOR == 0), STOP MOTOR
checkEnableMotor:
	clr r0
	cp enableMotor, r0
	brne motorGo
	jmp motorStop
motorGo:
	ser temp1
	jmp timer3Epilogue
motorStop:
	clr temp1
	jmp timer3Epilogue
; 3
timer3Epilogue:
	sts OCR3BL, temp1
	pop temp1
	out SREG, temp1
	pop temp1
	reti

; TIMER 4 - POTENTIOMETER UPDATE EVERY 100MS
; SIMILAR TO TIMER1/2
; ENABLEPOT = 0 -> DISABLE TIMER4
; ENABLEPOT = 1 -> STAGE 1: GO TO 0x0000 (POTSTAGEMIN)
; ENABLEPOT = 2 -> STAGE 2: GO TO 0x03FF (POTSTAGEMAX)
; ENABLEPOT = 3 -> STAGE 3: GO TO 0x0000 (POTSTAGEMIN)
; ENABLEPOT = 4 -> STAGE 4: COIN INSERTION COMPLETE, GO BACK TO STAGE 1 (POTSTAGEFINAL)
TIMER4OVF:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24
checkPot:
	clr r0
	cp enablePot, r0
	breq timer4Epilogue
startTimer4:
	; 2
	lds r24, PotCounter
	lds r25, PotCounter+1
	adiw r25:r24, 1
	; 3
	cpi r24, low(tenthSecond)
	ldi temp1, high(tenthSecond)
	cpc r25, temp1
	brne notTenthSecond
; WHEN 100MS PASS
; 1. RETRIVE POTENTIOMETER VALUE
; 2. CHECK STAGE, AND BRANCH ACCORDINGLY
tenthSeconds:
	clear PotCounter
	lds potValueL, ADCL
	lds potValueH, ADCH	
	inc r0
	cp enablePot, r0
	breq potStageMin
	inc r0
	cp enablePot, r0
	breq potStageMax
	inc r0
	cp enablePot, r0
	breq potStageMin
	inc r0
	cp enablePot, r0
	breq potStageFinal
; WHEN POTENTIOMETER = 0X0000 (FULLY ANTICLOCKWISE), INCREMENT
potStageMin:
	ldi temp1, low(0x0000)
	ldi temp2, high(0x0000)
	cp potValueL, temp1
	cpc potValueH, temp2
	breq incPot
	jmp timer4Epilogue
; WHEN POTENTIOMETER = 0X03FF (FULLY CLOCKWISE), INCREMENT
potStageMax:
	ldi temp1, low(0x03FF)
	ldi temp2, high(0x03FF)
	cp potValueL, temp1
	cpc potValueH, temp2
	breq incPot
	jmp timer4Epilogue
; WHEN PROCESS COMPLETE, INCREMENT COIN COUNT, DECREASE INVENTORY COST
potStageFinal:
	inc coinCount
	dec inventoryCost
	ldi temp1, 1
	mov enablePot, temp1
	jmp timer4Epilogue
; NEXT STAGE
incPot:
	inc enablePot
	jmp timer4Epilogue
notTenthSecond:
	sts PotCounter, r24
	sts PotCounter+1, r25
	rjmp timer4Epilogue
; 4
timer4Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti

; POTENTIOMETER INTERRUPT ADC
; USED IN CONJUNCTION WITH TIMER4
; 1. PUSH
; 2. STORE (1<<ADSC) INTO ADCSRA
; 3. POP
EXT_POT:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	; 2
	lds temp1, ADCSRA
	ori temp1, (1<<ADSC)
	sts ADCSRA, temp1
	; 3
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti
;---------------------------------------------------------------------------------------------------
; FUNCTIONS
;---------------------------------------------------------------------------------------------------

; DISPLAY START SCREEN MESSAGE ON LCD
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

; DISPLAY SELECT SCREEN MESSAGE ON LCD
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

; EMPTY SCREEN PROCEDURE
; 1. PUSH
; 2. UPDATE MODULE
; 3. EMPTY SCREEN MSG ON LCD
; 4. LOOP
; 5. DISABLE/RESET VARIABLES
; 6. POP
emptyScreen:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	; 2
	ldi currentModule, emptyScreenM
	; 3
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
	; 4
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
		; 5
		rcall ledOff
		clr enableTimer1
		clr enableButtons
		clr isPressed0
		clr isPressed1
		; 6
		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		ret
; COIN SCREEN PROCEDURE
; 1. PUSH
; 2. UPDATE MODULE
; 3. INITIAL COIN SCREEN MSG ON LCD (UPDATED IN CHECKKEYPAD)
; 4. CHECK FOR KEYPAD OR POTENTIOMETER INPUT
; 5. RESET VARIABLES AND POP
coinScreen:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	; 2
	ldi currentModule, coinScreenM
	; 3
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

	; 4

	; 4.1 SCAN KEYPAD INPUT
	; 4.2 SCAN POTENTIOMETER INPUT (DONE IN TIMER4)
	; 4.3 CHECK IF ITEM IS PURCHASED (DONE IN CHECKKEYPAD)
	; 4.4 UPDATE INVENTORY IF ITEM IS PURCHASED
	; 4.5 FINISH LOOP IF "#" IS PRESSED
	mov temp2, keypadInput
	; 4.2
	inc enablePot
	coinScreenLoop:
		; 4.1 & 4.3
		inc enableKeypad
		ldi moduleSelector, 0
		rcall checkKeypad
		; 4.4
		clr r0
		cp inventoryCost, r0
		breq updateInventory
		; 4.5
		ldi temp1, 0x0F
		cp keypadInput, temp1
		breq coinScreenLoopEnd
		jmp coinScreenLoop
	; REMOVE 1 ITEM FROM THE INVENTORY
	updateInventory:
		mov keypadInput, temp2
		rcall removeItem
		jmp coinScreenEpilogue
	; IF(COINCOUNT != 0), SPIN MOTOR
	; IF NOT, DO NOTHING
	coinScreenLoopEnd:
		clr r0
		cp coinCount, r0
		breq coinScreenEpilogue
		inc enableMotor
		rcall sleep_100ms
		rcall sleep_100ms
		rcall sleep_25ms
		rcall sleep_25ms
		clr enableMotor
		rcall sleep_100ms
		rcall sleep_100ms
		rcall sleep_25ms
		rcall sleep_25ms
	; 5
	coinScreenEpilogue:
		rcall ledOff
		clr coinCount
		clr enablePot
		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		ret

; DELIVERY SCREEN PROCEDURE
; 1. PUSH
; 2. UPDATE MODULE
; 3. DELIVERY SCREEN MSG ON LCD
; 4. SPIN MOTOR/FLASH LED
; 5. POP
deliveryScreen:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	; 2
	ldi currentModule, deliveryScreenM
	; 3
	do_lcd_command 0b00000001 ; Clear display
	do_lcd_data 'D'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'i'
	do_lcd_data 'v'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'm'
	; 4
	inc enableMotor
	ser temp1
	out PORTC, temp1
	out PORTG, temp1
	rcall sleep_500ms
	rcall sleep_500ms
	rcall sleep_500ms
	clr temp1
	out PORTC, temp1
	out PORTG, temp1
	rcall sleep_500ms
	rcall sleep_500ms
	rcall sleep_500ms
	clr enableMotor
	; 5
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	ret
; SCAN KEYPAD AND RETURN INPUT TO KEYPADINPUT
; LOTS OF PROCESSES INSIDE THIS FUNCTION, SO LETS TAKE THINGS SLOW
checkKeypad:
	; PUSH
	push temp1
	push temp2
	in temp1, SREG
	push temp1
; IF(ENABLEKEYPAD == 0), RETURN
; IF(ENABLEKEYPAD == 1), CONTINUE
checkEnableKeypad:
	clr r0
	cp enableKeypad, r0
	brne checkKeypadDebounce
	rjmp return
; IF(DEBOUNCE == 0), CONTINUE
; OTHERWISE, RETURN
checkKeypadDebounce:
	cpi debounce, 0
	breq startCheckKeypad
	rjmp return
	ldi debounce, 1
; BEGIN ACTUAL FUNCTION
startCheckKeyPad:
	ldi cmask, INITCOLMASK
	clr col
	colloop:
		; IF ENABLETIMER2 = 2, THEN 3 SECONDS HAVE PASSED, JUMP TO END (FOR STARTSCREEN -> SELECTSCREEN)
		; OTHERWISE, CONTINUE
		ldi temp1, 2
		cp enableTimer2, temp1
		brne colloopContinue1
		jmp checkKeypadEpilogue
	colloopContinue1:
		; IF CURRENT MODULE IS COIN SCREEN MODULE, JUMP TO COINSCREENUPDATE
		ldi temp1, coinScreenM
		cp currentModule, temp1
		breq coinScreenUpdate
		jmp colloopContinue2

	; 1. UPDATE LCD TO INVENTORYCOST
	; 2. UPDATE LED TO COINCOUNT
	; 3. CHECK IF PURCHASED
	coinScreenUpdate:
		; 1
		do_lcd_command 0b11000000		; forces cursor to the next line
		print_digit inventoryCost
		; 2
		mov temp1, coinCount
		out PORTC, temp1
		; 3
		clr r0
		cp inventoryCost, r0
		breq toDelivery
		jmp colloopContinue2
	; DISABLE DEVICES, RESET COIN COUNTER
	; JUMP TO END (TO DELIVERY SCREEN)
	toDelivery:
		ldi moduleSelector, deliveryScreenM
		clr enablePot
		clr coinCount
		jmp checkKeypadEpilogue
	; CHECK KEYPAD INPUT
	colloopContinue2:
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
	; CONVERT KEYPAD INPUT
	; 0-9 -> 0-9
	; A-D -> 0X0A-0X0D
	; * -> 0X0E
	; # -> 0X0F
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
	; CHECK IF ANY JUMPS NEED TO BE MADE TO ANOTHER MODULE
	checkModule:
		cpi moduleSelector, 0	
		breq return	
		cpi moduleSelector, selectScreenM
		breq jumpSelectScreen
		cpi moduleSelector, deliveryScreenM
		breq jumpDeliveryScreen
	return:
		; POP
		pop temp1
		out SREG, temp1
		pop temp2
		pop temp1
		ret
	; JUMP TO SELECT SCREEN
	jumpSelectScreen:
		clr moduleSelector
		rcall selectScreen
		jmp return
	; JUMP TO DELIVERY SCREEN
	jumpDeliveryScreen:
		clr moduleSelector
		rcall deliveryScreen
		jmp return
		
; INPUT: KEYPADINPUT (1-9) FOR ITEM
; OUTPUT: ITEM INVENTORY COUNT -> INVENTORYCOUNT
;         ITEM INVENTORY COST  -> INVENTORYCOST
getInventoryInfo:
	push temp1
	push temp2
	in temp1, SREG
	push temp1

	ldi temp2, 1
	; CHECK ITEM DEPENDING ON KEYPADINPUT
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
; UPDATE INFO
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

; TAKE 1 AWAY FROM INVENTORYCOUNT
removeItem:
	push temp1
	push temp2
	in temp1, SREG
	push temp1

	ldi temp2, 1
	; CHECK ITEM DEPENDING ON KEYPADINPUT
	cp keypadInput, temp2
	breq removeItem1
	inc temp2
	cp keypadInput, temp2
	breq removeItem2
	inc temp2
	cp keypadInput, temp2
	breq removeItem3
	inc temp2
	cp keypadInput, temp2
	breq removeItem4
	inc temp2
	cp keypadInput, temp2
	breq removeItem5
	inc temp2
	cp keypadInput, temp2
	breq removeItem6
	inc temp2
	cp keypadInput, temp2
	breq removeItem7
	inc temp2
	cp keypadInput, temp2
	breq removeItem8
	inc temp2
	cp keypadInput, temp2
	breq removeItem9
; REMOVE 1 ITEM
removeItem1:
	lds inventoryCount, Item1
	dec inventoryCount
	sts Item1, inventoryCount
	jmp removeItemEnd
removeItem2:
	lds inventoryCount, Item2
	dec inventoryCount
	sts Item2, inventoryCount
	jmp removeItemEnd
removeItem3:
	lds inventoryCount, Item3
	dec inventoryCount
	sts Item3, inventoryCount
	jmp removeItemEnd
removeItem4:
	lds inventoryCount, Item4
	dec inventoryCount
	sts Item4, inventoryCount
	jmp removeItemEnd
removeItem5:
	lds inventoryCount, Item5
	dec inventoryCount
	sts Item5, inventoryCount
	jmp removeItemEnd
removeItem6:
	lds inventoryCount, Item6
	dec inventoryCount
	sts Item6, inventoryCount
	jmp removeItemEnd
removeItem7:
	lds inventoryCount, Item7
	dec inventoryCount
	sts Item7, inventoryCount
	jmp removeItemEnd
removeItem8:
	lds inventoryCount, Item8
	dec inventoryCount
	sts Item8, inventoryCount
	jmp removeItemEnd
removeItem9:
	lds inventoryCount, Item9
	dec inventoryCount
	sts Item9, inventoryCount
	jmp removeItemEnd
removeItemEnd:
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	ret
; TURNS ALL LED ON
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
; TURNS ALL LED OFF
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
; WRITE LCD COMMANDS
lcd_command:
	out PORTF, temp1	;Output PORTF with data
	rcall sleep_1ms
	lcd_set LCD_E		;Set Enable bit
	rcall sleep_1ms
	lcd_clr LCD_E		;Clear Enable bit
	rcall sleep_1ms
	ret
; WRITE CHARACTERS TO LCD
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
; BUSY FLAG
lcd_wait:
	push temp1
	clr temp1
	out DDRF, temp1
	out PORTF, temp1
	lcd_set LCD_RW		;RS = 0; RW = 1 (needed for busy mode)
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in temp1, PINF	
	lcd_clr LCD_E
	sbrc temp1, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser temp1
	out DDRF, temp1
	pop temp1
	ret

; DELAY 1MS
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
; DELAY 5MS
sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret
; DELAY 25MS
sleep_25ms:
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	ret
; DELAY 100MS
sleep_100ms:
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	ret
; DELAY 500MS
sleep_500ms:
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	ret