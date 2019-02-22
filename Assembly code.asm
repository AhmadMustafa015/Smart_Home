;Ahmad Mustafa 0157454
;Abdullah Abuzaid	0150524
#include P16F877A.inc
	Org 0X00
	Cblock 0X20
	    TEMP
	    TEMPL
	    TEMPH
	    ROOM_LOCK
	    LIGHT
	    LED
	    FAN
	    SENSOR_STATE
	    CELSIUS
	    TENS
	    UNITS
	    TMR1_COUNT
	    LDR1H
	    LDR1L
	    MOTION
	    NO_MOT60
	    LIGHT150
	    msd			
	    lsd			
	    Row_Index	
	    Col_Index	
	    temp0		
	    Keypad		
	    Correct
	    Pass_Count	

	Endc
	Goto START0
	Org 0X04
	Goto ISR
;**********************************************************************************************************	
Table
	ADDWF PCL, F
	RETLW A'1'
	RETLW A'2'
	RETLW A'3'
	RETLW A'A'
	RETLW A'4'
	RETLW A'5'
	RETLW A'6'
	RETLW A'B'
	RETLW A'7'
	RETLW A'8' 
	RETLW A'9'
	RETLW A'C'
	RETLW A'*'
	RETLW A'0'
	RETLW A'#'
	RETLW A'D'
;--------------------------------------------------------------------------------------
START0

	Call INITIAL	
;Locked
loop1
	movlw	0x08
	subwf	Pass_Count,W
	btfsc	STATUS,Z
	goto	first_digit
	goto	loop1
loop2
	BANKSEL TMR1_COUNT
	MOVLW 	.60
	subwf  	TMR1_COUNT , W
	BTFSS	STATUS , C
	GOTO 	0X00
	movlw	0x06
	subwf	Pass_Count,W
	btfsc	STATUS,Z
	goto	second_digit
	goto	loop2
loop3
	BANKSEL TMR1_COUNT
	MOVLW 	.60
	subwf  	TMR1_COUNT , W
	BTFSS	STATUS , C
	GOTO 	0X00
	movlw	0x04
	subwf	Pass_Count,W
	btfsc	STATUS,Z
	goto	third_digit
	goto	loop3
loop4
	BANKSEL TMR1_COUNT
	MOVLW 	.60
	subwf  	TMR1_COUNT , W
	BTFSS	STATUS , C
	GOTO 	0X00
	movlw	0x02
	subwf	Pass_Count,W
	btfsc	STATUS,Z
	goto	fourth_digit
	goto	loop4

first_digit
	CALL 	TIMER1
	movlw	0x01
	call 	Send_cmd
	movlw	0x86
	call	Send_cmd
	movf	Keypad,W
	call 	Send_char
	movlw	A'2'
	subwf	Keypad,W
	btfss	STATUS,Z
	bsf	Correct,0			;If password correct, Correct = 0
	goto	loop2
second_digit
	movlw	0x86
	call	Send_cmd
	movlw	A'*'
	call	Send_char
	movf	Keypad,W
	call 	Send_char
	movlw	A'0'
	subwf	Keypad,W
	btfss	STATUS,Z
	bsf		Correct,1		
	goto	loop3
third_digit
	movlw	0x87
	call	Send_cmd
	movlw	A'*'
	call	Send_char
	movf	Keypad,W
	call 	Send_char
	movlw	A'1'
	subwf	Keypad,W
	btfss	STATUS,Z
	bsf		Correct,2		
	goto	loop4
fourth_digit
	movlw	0x88
	call	Send_cmd
	movlw	A'*'
	call	Send_char
	movf	Keypad,W
	call 	Send_char
	movlw	A'8'
	subwf	Keypad,W
	btfss	STATUS,Z
	bsf	Correct,3		
	movlw	0x09
	movwf	Pass_Count

	movf	Correct,F
	btfsc	STATUS,Z
	goto	Room_Unlocked
	call	Room_Locked
	clrf	Correct
	goto	loop1
	
	
Room_Unlocked
	Movlw 	0x01  			;clear display 
	Call	Send_cmd
	Movlw	0x80			;Cursor at address 00	
	Call	Send_cmd

	Movlw	'R'
	Call	Send_char
	Movlw	'o'
	Call	Send_char
	Movlw	'o'
	Call	Send_char
	Movlw	'm'
	Call	Send_char
	Movlw	' '
	Call	Send_char
	Movlw	'i'
	Call	Send_char
	Movlw	's'
	Call	Send_char
	Movlw	' '
	Call	Send_char
	Movlw	'U'
	Call	Send_char
	Movlw	'n'
	Call	Send_char
	Movlw	'l'
	Call	Send_char
	Movlw	'o'
	Call	Send_char
	Movlw	'c'
	Call	Send_char
	Movlw	'k'
	Call	Send_char
	Movlw	'e'
	Call	Send_char
	Movlw	'd'
	Call	Send_char
	
	Call 	Temp_write
	banksel ROOM_LOCK
	movlw 0xff
	movwf ROOM_LOCK
	BANKSEL 	TRISB 
	BSF 	TRISB , 0
	BANKSEL INTCON 
	BCF	INTCON , RBIE
	BSF	INTCON , INTE
	BANKSEL		T1CON 
	BCF 	T1CON , TMR1ON   ; disable timer 1
	BANKSEL TMR1_COUNT
	MOVLW 0X78
	MOVWF TMR1_COUNT
START	
	Call TEMP_CONTROL
	CALL	LIGHT_CONTROL
	Goto START
;**********************************************************************************************************	
INITIAL
	;PORT CONFIGURATION
	Banksel TRISA
	Clrf TRISC	    ;Set port c as output
	Movlw 0X07
	Movwf TRISA
	CLRF TRISD
	BANKSEL TRISB
	MOVLW	B'11110000'
	MOVWF	TRISB
	Banksel OPTION_REG
	bcf OPTION_REG, 7
	
	;AD configuration
	Banksel ADCON1
	Movlw 0X80     
	Movwf ADCON1	    ;ad converter is right justfied | Fosc/8 | RA0-7 are analog

	;INTERRUPT 	
	Banksel INTCON
	bcf	INTCON,RBIF
	;BSF	INTCON , T0IE
	BSF	INTCON,RBIE
	Bsf INTCON , GIE
	Bsf INTCON , PEIE 
	;BSF INTCON , INTE
	Banksel PIE1
	Bsf PIE1 , ADIE	    ;ad interrupt enable
	Bsf PIE1 , TMR1IE   ;timer1 interruot enable 

	;INITIAL VALUE
	BANKSEL TMR1_COUNT
	MOVLW 0X78
	MOVWF TMR1_COUNT
	CLRF PORTB
	CLRF PORTC
	CLRF CELSIUS 
	CLRF TENS
	CLRF UNITS
	CLRF	PORTD
	MOVF	PORTB,W			;KEYPAD	CONFIGURATION
	BANKSEL	INTCON
	Banksel	Correct
	clrf	Correct
	;CLRF	PASS_DIGIT
	movlw	0x09
	movwf	Pass_Count
	
	;LCD
	
	Movlw   0x38			;8-bit mode, 2-line display, 5x7 dot format 
	Call	Send_cmd
	Movlw 	0x0e			;Display on, Cursor Underline on, Blink off 
	Call	Send_cmd	
	Movlw	0x01			;Cursor at address 01	
	Call	Send_cmd
	Movlw 	0x81  			;clear display 
	Call	Send_cmd
	Call	Draw_Centigrade
	Call	Room_Locked		;Writes "Room is Locked.." on LCD

RETURN 
;**********************************************************************************************************	
ISR
	Banksel PIR1
	Btfsc PIR1 , ADIF	;check what is the interrupt source
	Goto AD_INT
	Btfsc PIR1 , TMR1IF	; AQQ TIME TACKE 19.2 us while sampling time tack 9.8 us
	GOTO TMR1IFS
	BANKSEL INTCON
	btfsc INTCON , RBIF	;portb on change 
	goto RB_CHANGE
;PIR SENSOR
EXTERNAL_INERRUPT
	BANKSEL MOTION
	CLRF NO_MOT60
	MOVLW 0XFF
	MOVWF MOTION		;MOTION DETECT
	BANKSEL TMR1_COUNT	;RESET COUNTER
	MOVLW 0X78
	MOVWF TMR1_COUNT
	CALL TIMER1
	BCF INTCON , INTF
	RETFIE
TMR1IFS
	BANKSEL T1CON
	BCF T1CON , TMR1ON	;TURN OFF TIMER 1
	BCF PIR1 , TMR1IF	;CLEAR INTERRUPT FLAG
	DECFSZ TMR1_COUNT , F	;CHECK IF WE REACH TO 120
	GOTO TMR1_SKIP
	BANKSEL TMR1_COUNT	;RESET COUNTER
	MOVLW 0X78
	MOVWF TMR1_COUNT
	MOVLW 0XFF
	MOVWF NO_MOT60
	RETFIE
TMR1_SKIP
	CALL TIMER1
	RETFIE
	
AD_INT
	BTFSC SENSOR_STATE , 1	;CHECK WHICH SENSOR IS IT VALUE UPDATED 
	GOTO TEMP_UPDATE	;00 LDR
	
LDR_SENSOR
	BANKSEL ADRESL		;MOVE THE RESULT OF CONVERISION TO 
	MOVF ADRESL , W		;TEMPL AND TEMPH
	BANKSEL TEMPL
	MOVWF LDR1L
	BANKSEL ADRESH	
	MOVF ADRESH , W
	MOVF ADRESH , 0
	BANKSEL TEMPL
	MOVWF LDR1H
	BANKSEL PIR1
	BCF PIR1 , ADIF		; CLEAR INTERRUPT FLAG
	BANKSEL LDR1H
	RETFIE
TEMP_UPDATE
	BANKSEL ADRESL		;MOVE THE RESULT OF CONVERISION TO 
	MOVF ADRESL , W		;TEMPL AND TEMPH
	BANKSEL TEMPL
	MOVWF TEMPL
	BANKSEL ADRESH	
	MOVF ADRESH , W
	BANKSEL TEMPL
	MOVWF TEMPH
	BANKSEL TENS
	CLRF TENS
	CLRF CELSIUS
	CLRF UNITS
	CLRF TEMP
	CALL BCD_TEMP		;CONVERT TEMP TO CELSIUS
	MOVLW 0x30
	ADDWF UNITS ,F 
	ADDWF TENS , F
	BANKSEL PIR1
	BCF PIR1 , ADIF		; CLEAR INTERRUPT FLAG
	Call 	Temp_write
	RETFIE
RB_CHANGE 
	Banksel	PORTB
	movf	PORTB,W
	movwf	Row_Index		;Read row number
	Banksel	TRISB
	movlw	B'00001110'
	movwf	TRISB
	Banksel	PORTB
	clrf	PORTB
	BANKSEL TRISC
	BSF	TRISC , 0
	BANKSEL PORTC
	MOVLW 	0X01
	ANDWF PORTC  ,W
	MOVWF TEMP
	movf	PORTB ,	W
	IORWF	TEMP , W
	movwf	Col_Index		;Read column number
	call	Convert	

Reset_PORTB
	Banksel	TRISB
	BCF TRISC , 0
	movlw	B'11110000'
	movwf	TRISB
	Banksel	PORTB
	clrf	PORTB
	movf	PORTB,W
	Banksel	INTCON
	bcf		INTCON,0

	decf	Pass_Count,F

	retfie
		
;**********************************************************************************************************	

Room_Locked
	Movlw 	0x01  			;clear display 
	Call	Send_cmd
	Movlw	0x81			;Cursor at address 01	
	Call	Send_cmd

	Movlw	'R'
	Call	Send_char
	Movlw	'o'
	Call	Send_char
	Movlw	'o'
	Call	Send_char
	Movlw	'm'
	Call	Send_char
	Movlw	' '
	Call	Send_char
	Movlw	'i'
	Call	Send_char
	Movlw	's'
	Call	Send_char
	Movlw	' '
	Call	Send_char
	Movlw	'L'
	Call	Send_char
	Movlw	'o'
	Call	Send_char
	Movlw	'c'
	Call	Send_char
	Movlw	'k'
	Call	Send_char
	Movlw	'e'
	Call	Send_char
	Movlw	'd'
	Call	Send_char
	Call TEMP_CONTROL
	Call 	Temp_write
	Return
;--------------------------------------------------------------------------------------
Temp_write
	Movlw	0x0C
	Call	Send_cmd
	Movlw	0xC3			;Cursor at address 0x43
	Call	Send_cmd
	Movlw	'T'
	Call	Send_char
	Movlw	'e'
	Call	Send_char
	Movlw	'm'
	Call	Send_char
	Movlw	'p'
	Call	Send_char
	Movlw	' '
	Call	Send_char
	Movlw	'='
	Call	Send_char
	Movlw	' '
	Call	Send_char
	MOVF	TENS , W
	Call	Send_char
	MOVF	UNITS , W
	Call	Send_char
	Movlw	0x00
	Call	Send_char
	
	RETURN
;--------------------------------------------------------------------------------------
Convert
	btfss	Col_Index,3
	movlw	0
	btfss	Col_Index,2
	movlw	1
	btfss	Col_Index,1
	movlw	2
	btfss	Col_Index,0
	movlw	3
	movwf	Col_Index

	btfss	Row_Index,7
	movlw	0
	btfss	Row_Index,6
	movlw	1
	btfss	Row_Index,5
	movlw	2
	btfss	Row_Index,4
	movlw	3
	movwf	Row_Index

	movf	Row_Index,W		;Key # = Row_Index*4 + Col_Index
	addwf	Row_Index,W
	addwf	Row_Index,W
	addwf	Row_Index,W
	addwf	Col_Index,W

	movwf	temp0
	movlw	D'16'
	subwf	temp0,W
	btfsc	STATUS,C
	goto	skip
	movf	temp0,W
	;movlw	D'1'
	call	Table
	movwf	Keypad
skip
	return

;--------------------------------------------------------------------------------------

Send_cmd 
	Movwf  	PORTD 
	Bcf  	PORTC, 5 
	Bsf 	PORTC, 7 
	Nop 
	Bcf 	PORTC, 7 
	Bcf 	PORTC, 6  
	Call 	delay 
	Return
;--------------------------------------------------------------------------------------
Send_char 
	Movwf  	PORTD 
	Bsf  	PORTC, 5 
	Bsf 	PORTC, 7 
	Nop 
	Bcf 	PORTC, 7 
	Bcf 	PORTC, 6  
	Call 	delay
	Return
;--------------------------------------------------------------------------------------
Draw_Centigrade
	Movlw	0x40
	Call	Send_cmd
	Movlw	B'00010010'
	Call	Send_char
	Movlw	B'00000101'
	Call	Send_char
	Movlw	B'00001000'
	Call	Send_char
	Movlw	B'00001000'
	Call	Send_char
	Movlw	B'00001000'
	Call	Send_char
	Movlw	B'00000101'
	Call	Send_char
	Movlw	B'00000010'
	Call	Send_char
	Movlw	B'00000000'
	Call	Send_char
	Return

;--------------------------------------------------------------------------------------


delay
	movlw 0x80
	movwf msd
	clrf lsd
loop02
	decfsz lsd,f
	goto loop02
	decfsz msd,f 
endLcd
	goto loop02 
	return

;**********************************************************************************************************	
BCD_TEMP
;To calculat the temperature in dicemal we must do the following
;first the temperature range of LM35 is -55>c>150 also 10mv/c
;we divide range (150+55)/1024 lets call it R
;R/10m then we will have 2.048 c/LSB
	BANKSEL CELSIUS
gen_celsius
	MOVLW .2		;DIVIDE ON 2
	SUBWF TEMPL , 0
	BTFSS STATUS,C 
	GOTO  TESTT
CONTE
	MOVWF TEMPL 
;	MOVLW .255
;	SUBWF CELSIUS , W
;	BTFSC STATUS , Z
;	GOTO INCH
	INCF CELSIUS , F
	GOTO gen_celsius 
;INCH
;	INCF CELSIUSH , F
;	GOTO gen_celsius 
gen_tens
	MOVLW .10		;THE TENS 
	SUBWF TEMP,0
	BTFSS STATUS,C 
	GOTO gen_ones
;CONTEE
	MOVWF TEMP
	INCF TENS,1 
	GOTO gen_tens 
gen_ones
	MOVF TEMP,W		;ONES
	MOVWF UNITS
	RETURN
TESTT
	MOVF CELSIUS ,W
	MOVWF TEMP
	MOVF TEMPH , F
	BTFSC STATUS , Z
	GOTO gen_tens
	decf TEMPH ,F
	MOVLW 0XFF
	MOVWF TEMPL
	GOTO CONTE
;**********************************************************************************************************	
TIMER1			    ;IN HERE WE WANT TO CONFIGURE TIMER1 TO HAVE .5S COUNET 
	MOVLW 0x0B	    ;THEN USE ISR TO INCREMENT COUNTER 30 TIME THAT GIVE US 60s
	MOVWF TMR1H
	MOVLW 0XDC
	MOVWF TMR1L	    ; initialize TMR1L
	MOVLW 0x30
	MOVWF T1CON	    ; initialize T1CON
	BSF T1CON , TMR1ON   ; enable timer 1
	RETURN
	DELAY10us
	Movlw D'2'
        Movwf 0x30	    ; counter for delay loop
more    Nop
	Decfsz 0x30,1
	Goto more
Return
;**********************************************************************************************************	
LIGHT_CONTROL
;LDR SENSOR
LDR1	
	BANKSEL ADCON0
	MOVLW 0X11		;CHANNEL AN2
	MOVWF ADCON0
	CALL DELAY10us
	BSF ADCON0 , GO		;START CONVERISION
	BANKSEL SENSOR_STATE
	CLRF SENSOR_STATE	;ANALOGE RESULT IS FOR LDR
	
CONTROLLING
	NOP 
	NOP
	NOP
	NOP
	NOP
	NOP
	Btfss ROOM_LOCK ,0	;FIRST TEST IF THE ROOM IS LOCK
	Goto SKIP_LIGHT		
	BTFSS MOTION , 1	;SECOND IF THERE MOTION
	GOTO SKIP_LIGHT
	BTFSC NO_MOT60 , 1	;THIRED IF THE MOTION STOP FOR 60 S
	GOTO SKIP_LIGHT_MOTION
	MOVF LDR1H , F
	BTFSS STATUS , Z	;CHECK IF THE LDR READ LESS THAN 200 THEN THERE IS LIGHT 
	GOTO 	TURNON
	MOVLW 0XFF
	MOVWF LIGHT150
	SUBWF LDR1L , W
	BTFSS STATUS , C	;CHECK IF THE VALUE LESS THAN 150
	GOTO SKIP_LIGHT
TURNON
	BANKSEL PORTC
	BSF PORTC , 3		;TURN ON LED
	RETURN
	
SKIP_LIGHT
	BANKSEL MOTION
	BCF PORTC , 3		;TURN OFF LED
	RETURN
SKIP_LIGHT_MOTION
	BANKSEL MOTION
	BCF PORTC , 3
	CLRF MOTION
	RETURN
	
;**********************************************************************************************************	
TEMP_CONTROL
	Banksel ADCON0		;THIS CONFIGURATION FOR TEMP_SENSOR
	Movlw 0X01
	Movwf ADCON0		;select AN0 and enabling sampling
	Call DELAY10us
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	bsf ADCON0 , 2		;start converision
	BANKSEL SENSOR_STATE
	MOVLW 0XFF
	MOVWF SENSOR_STATE
	BANKSEL ROOM_LOCK
	BTFSS ROOM_LOCK , 0	;CHECK IF ROOM LOCK
	GOTO SKIP0
	MOVLW .30		;CHECK IF TEMP MORE THAN 30c
	SUBWF CELSIUS , W
	BTFSS STATUS , C
	GOTO FAN_OFF
	BSF PORTC , 4		;TURN ON FAN
SKIP0
	RETURN
FAN_OFF 
	BCF PORTC , 4		;TURN OFF FAN
	RETURN
   END
