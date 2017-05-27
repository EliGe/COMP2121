

;LCD
.equ LCD_RS = 7			;Register Select
.equ LCD_E = 6			;Enable
.equ LCD_RW = 5			;Read/Write
.equ F_CPU = 16000000	;16MHz
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4

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