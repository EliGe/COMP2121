; Task: Fading brightness of LED each second

; SET-UP BOARD
; Connect
; LED0 (TOP LED) ~ PE2 (OC3B)

.include "m2560def.inc"

; REGISTERS
.def temp1 = r16
.def intensity = r17

; CONSTANTS

; MACROS

; ADDRESSES
.org 0x0000
	jmp RESET

.org OVF3addr
	jmp TIMER3OVF

; SETUP//RESET
; WE ARE USING TIMER3 + LED0
; WE REQUIRE: PWM TIMER
 RESET:
	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	clr temp1
	clr intensity

	; Setting up PWM Timer
	ser temp1 ; PE2 as Output
	out DDRE, temp1		  ; Bit 3 will function as OC3B

	; Mode for PWN -> Phase Correct
	ldi temp1, (1<<WGM30) | (1<<COM3C1) | (1<<COM3B1); WGM -> Phase Correct; (TOP 0xFF)
										; COM -> Set OC3A on compare match when down-counting, 
										;		 Clear OC3A on compare match when up-counting
	sts TCCR3A, temp1

	ldi temp1, 0b00000100	; Prescaler 256
	sts TCCR3B, temp1
	ldi temp1, (1<<OCIE3C) | (1<<OCIE3B)	; turns overflow interrupt bit on
	sts TIMSK3, temp1

end:
	rjmp end

TIMER3OVF:
	dec intensity
	sts OCR3CL, intensity
	sts OCR3BL, intensity
	reti
; FUNCTIONS
