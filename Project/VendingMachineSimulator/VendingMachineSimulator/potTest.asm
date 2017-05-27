.include "m2560def.inc"

.def temp1 = r16
.def temp2 = r17
.def potValueL = r18
.def potValueH = r19

.org 0x0000
	jmp RESET
.org OVF0addr
	jmp TIMER0OVF
.org 0x003A
	jmp EXT_POT

; Clear Data Memory
.macro clear
	ldi YL, low(@0)
	ldi YH, high(@0)
	clr temp1
	st Y+, temp1
	st Y, temp1
.endmacro

.dseg
DebounceCounter:
	.byte 2
.cseg

RESET:
	clr temp1
	clr temp2
	clr potValueL
	clr potValueH

	ldi temp1, low(RAMEND)
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	ser temp1
	out DDRC, temp1
	out DDRG, temp1
	out PORTC, temp1
	out PORTG, temp1

	ldi temp1, (3<<REFS0) | (0<<ADLAR) | (0<<MUX0)
	sts ADMUX, temp1
	ldi temp1, (1<<MUX5)
	sts ADCSRB, temp1
	ldi temp1, (1<<ADEN) | (1<<ADSC) | (1<<ADIE) | (1<<ADPS2) | (0<<ADPS1) | (1<<ADPS0)
	sts ADCSRA, temp1

	ldi temp1, 0x00
	out TCCR0A, temp1
	ldi temp1, 0x02
	out TCCR0B, temp1
	ldi temp1, (1<<TOIE0)
	sts TIMSK0, temp1

main:

end:
	jmp end
	
EXT_POT:
	push temp1
	push temp2
	in temp1, SREG
	push temp1

	lds temp1, ADCSRA
	ori temp1, (1<<ADSC)
	sts ADCSRA, temp1

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

startTimer0:
	lds r24, DebounceCounter
	lds r25, DebounceCounter+1
	adiw r25:r24, 1
	cpi r24, low(390)			;100ms
	ldi temp1, high(390)
	cpc r25, temp1
	brne notTenthSecond

;WHEN 100MS PASS
tenthSeconds:
	clear DebounceCounter
	
	lds potValueL, ADCL
	lds potValueH, ADCH	

	out PORTC, potValueL
	out PORTG, potValueH
		
	rjmp timer0Epilogue

notTenthSecond:
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