;***********************************************************
;*	This is the final project template for ECE375 Winter 2021
;***********************************************************
;*	 Author: Mitchell Radford
;*   Date: 3/12/2021
;***********************************************************

.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;***********************************************************
.def	mpr = r19				; Multipurpose register 
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
; Variavbles for calculations and macros
.def	A = r3					; A variable
.def	B = r4					; Another variable
; Define variables for every other register
; OPERAND 1 (general)
.def	C = r5
.def	D = r6
.def	E = r7
.def	F = r8
.def	G = r9
; OPERAND 2 (general)
.def	H = r10
.def	I = r11
.def	J = r12
.def	K = r13
.def	L = r14
.def	M = r15
.def	N = r16
; RESULT (general)
.def	O = R20
.def	P = R21
.def	Q = R22
.def	R = R23
.def	S = R24
.def	T = R25

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter

;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0100						; data memory allocation for operands
operand1:		.byte 10			; allocate 10 bytes for a variable named op1
; RADIUS AND GRAVITY IN MEMORY
.org	$0110
GM:				.byte 4
RAD:			.byte 2
; MULTIPLICATION OPERANDS AND RESULTS
.org	$0120
mul48_op1:		.byte 6
mul48_op2:		.byte 6
mul48_res:		.byte 12

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

; Moves a result to
; A different set of registers
; OPERAND 1: 0-6
; DESTINATION: 7-13
; 56 bit support
.MACRO	MOVERESULT
		mov		@7, @0
		mov		@8, @1
		mov		@9, @2
		mov		@10, @3
		mov		@11, @4
		mov		@12, @5
		mov		@13, @6
.ENDMACRO


; Adds in little endian form
; Stores into operand 1
; OPERAND 1: 0-6
; OPERAND 2: 7-13
.MACRO	SUB64
		sub		@0, @7
		sbc		@1, @8
		sbc		@2, @9
		sbc		@3, @10
		sbc		@4, @11
		sbc		@5, @12
		sbc		@6, @13
.ENDMACRO

; Adds in little endian form
; Stores into operand 1
; OPERAND 1: 0-6
; OPERAND 2: 7-13
.MACRO	ADD64
		add		@0, @7
		adc		@1, @8
		adc		@2, @9
		adc		@3, @10
		adc		@4, @11
		adc		@5, @12
		adc		@6, @13
.ENDMACRO

; divides in little endian
; when operands are HIGH-LOW
; 56 bit support
.MACRO	DIVBY2
		lsr		@6
		ror		@5
		ror		@4
		ror		@3
		ror		@2
		ror		@1
		ror		@0
.ENDMACRO

; multiplies in little endian
; when operands are HIGH-LOW
; 56 bit support
.MACRO	MULBY2
		lsl		@0
		rol		@1
		rol		@2
		rol		@3
		rol		@4
		rol		@5
		rol		@6
.ENDMACRO

; Compares in little endian form
; Start with the highest, work your way down
; Proide operands as: HI-LO
.MACRO	COMP64
		; CHECK FOR LESS (run often, in loops)
		cp		@0, @7
		cpc		@1, @8
		cpc		@2, @9
		cpc		@3, @10
		cpc		@4, @11
		cpc		@5, @12
		cpc		@6, @13
		brsh	NEXTCOMP
		rjmp	LESS
	NEXTCOMP:
		; CHECK FOR GREATER (only run in a few cases, and only once at any given time
		cp		@6, @13
		cpc		@5, @12
		cpc		@4, @11
		cpc		@3, @10
		cpc		@2, @9
		cpc		@1, @8
		cpc		@0, @7
		breq	EQUAL
		rjmp	GREATER
	; If OP1 less than OP2, store a 3
	LESS:
		ldi		mpr, $03
		rjmp	DONECOMP
	; If OP1 greater than OP2, store a 2
	GREATER:
		ldi		mpr, $02
		rjmp	DONECOMP
	; If OP1 equal to OP2, store a 1
	EQUAL:
		ldi		mpr, $01
	DONECOMP:
.ENDMACRO

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt
.org	$0046					; End of Interrupt Vectors
;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:	; The initialization routine
	; Initialize stack
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND
		clr		zero

		; Clear the registers
		clr		A									; clear all registers
		clr		B
		clr		C
		clr		D
		clr		E
		clr		F
		clr		G
		clr		H
		clr		I
		clr		J
		clr		K
		clr		L
		clr		M
		clr		N
		clr		O
		clr		P
		clr		Q
		clr		R
		clr		S
		clr		T
		clr		XL

		; CLEAR THE MEMORY PT1
		ldi		ZL, low(GM)							; get the memory location of
		ldi		ZH, high(GM)						; the GM
		ldi		mpr, $00							; set the next 6 bytes
		st		Z+, mpr								; to ZERO
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr

		; CLEAR THE MEMORY PT2
		ldi		ZL, low(mul48_op1)					; get the memory location of 
		ldi		ZH, high(mul48_op1)					; the multiplication operands
		ldi		mpr, $00							; set the next
		st		Z+, mpr								; 24 bytes to ZERO
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr
		st		Z+, mpr

		; check which GM to use
		rcall	PlanetChoice

		; Save the ORBITAL RADIUS to memory in $0114
		ldi		ZL, low(OrbitalRadius<<1)			; get the location of the radius
		ldi		ZH, high(OrbitalRadius<<1)			; into ZH:ZL
		ldi		YL, low(RAD)						; get the memory location of RAD
		ldi		YH, high(RAD)						; into YH:YL
		lpm		mpr, Z+								; load the first byte into mpr
		st		Y+, mpr								; store it
		mov		A, mpr								; save it to A
		lpm		mpr, Z								; load the second byte into mpr
		st		Y, mpr								; store it
		mov		B, mpr								; save it to B

		rcall	CHECKCASES1AND2
		; PROBLEM A: VELOCITY
		; Clear the registers
		clr		A
		clr		B
		clr		C
		clr		D
		clr		E
		clr		F
		clr		G
		clr		H
		clr		I
		clr		J
		clr		K
		clr		L
		clr		M
		clr		N
		clr		O
		clr		P
		clr		Q
		clr		R
		clr		S
		clr		T
		clr		XL
		; LOAD THE GM AS DIVISION OP1
		ldi		ZL, low(GM)
		ldi		ZH, high(GM)
		ld		A, Z+
		ld		B, Z+
		ld		C, Z+
		ld		D, Z+
		; LOAD THE RADIUS AS DIVISION OP2
		ld		H, Z+
		ld		I, Z+

		; DIVIDE GM by RADIUS
		rcall	BIGDIV		; Divide GM by RAD

		; MOVE THE RESULT INTO A-G
		MOVERESULT	O, P, Q, R, S, T, XL, A, B, C, D, E, F, G

		; STORE THE RESULT INTO MEMORY AT QUOTIENT
		ldi		ZL, low(Quotient)		; Get the memory location
		ldi		ZH, high(Quotient)		; of the quotient
		st		Z+, A					; only save A-C
		st		Z+, B
		st		Z+, C

		; GET THE SQUARE ROOT
		rcall	SQRT					; Square root the answer

		; CHECK THE RESULT TO SEE IF IT IS 0
		rcall	CHECKCASE3

		; STORE THE VELOCITY INTO MEMORY
		ldi		ZL, low(Velocity)		; Get the memory location
		ldi		ZH, high(Velocity)		; for velocity
		st		Z+, O					; only save O and P
		st		Z+, P

		; PROBLEM B: PERIOD
		; Clear the registers
		clr		A
		clr		B
		clr		C
		clr		D
		clr		E
		clr		F
		clr		G
		clr		H
		clr		I
		clr		J
		clr		K
		clr		L
		clr		M
		clr		N
		clr		O
		clr		P
		clr		Q
		clr		R
		clr		S
		clr		T
		clr		XL

		; LOAD THE ORBITAL RADIUS INTO A:B
		ldi		ZL, low(RAD)			; GET THE OPERAND 1 LOCATION
		ldi		ZH, high(RAD)			; INTO Z
		ld		A, Z+
		ld		B, Z

		; SET THE OPERANDS OF MULT TO BE ORBITAL RADIUS
		ldi		ZL, low(mul48_op1)			; GET THE OPERAND 1 LOCATION
		ldi		ZH, high(mul48_op1)			; INTO Z
		st		Z+, A
		st		Z+, B
		st		Z+, C
		st		Z+, D
		st		Z+, E
		st		Z+, F
		; SET OPERAND 2 AS THE RADIUS
		st		Z+, A
		st		Z+, B
		st		Z+, C
		st		Z+, D
		st		Z+, E
		st		Z+, F

		; CLEAR THE MULTIPLY RESULT
		ST		Z+, zero					; i do not
		ST		Z+, zero					; load a new memory
		ST		Z+, zero					; location because i
		ST		Z+, zero					; am already there
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z, zero

		; SQUARE THE RADIUS
		rcall	MUL48

		; SAVE THE RESULT INTO REGISTERS
		ldi		ZL, low(mul48_res)			; load the location
		ldi		ZH, high(mul48_res)			; of mult result
		ld		A, Z+
		ld		B, Z+
		ld		C, Z+
		ld		D, Z

		; STORE THE RESULT AS OPERAND 1
		ldi		ZL, low(mul48_op1)
		ldi		ZH, high(mul48_op1)
		st		Z+, A
		st		Z+, B
		st		Z+, C
		st		Z, D

		; CLEAR THE MULTIPLY RESULT
		ldi		ZL, low(mul48_res)			; load the location
		ldi		ZH, high(mul48_res)			; of mult result
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z, zero

		; CUBE THE RADIUS
		rcall	MUL48

		; SAVE THE RESULT INTO REGISTERS
		ldi		ZL, low(mul48_res)			; load the location
		ldi		ZH, high(mul48_res)			; of mult result
		ld		A, Z+
		ld		B, Z+
		ld		C, Z+
		ld		D, Z+
		ld		E, Z+
		ld		F, Z


		; STORE THE RESULT AS OPERAND 1
		ldi		ZL, low(mul48_op1)			; load the location of
		ldi		ZH, high(mul48_op1)			; operand 1
		st		Z+, A						; store registers
		st		Z+, B						; A-F
		st		Z+, C
		st		Z+, D
		st		Z+, E
		st		Z, F

		; CLEAR THE MULTIPLY RESULT
		ldi		ZL, low(mul48_res)			; load the location
		ldi		ZH, high(mul48_res)			; of mult result
		ST		Z+, zero					; set all 12 bits
		ST		Z+, zero					; to 00
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z, zero

		; SET OP2 to 4*pi^2 (40 dec, 28 hex)
		ldi		ZL, low(mul48_op2)			; load operand 2
		ldi		ZH, high(mul48_op2)			; memory location
		ldi		mpr, $28					; load 28 into A
		st		Z+, mpr						; set operand 2 as
		st		Z+, zero					; 28 00 00 00 00 00
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z, zero

		; multiply the result by 4 pi squared
		rcall	mul48

		; LOAD THE RESULT
		ldi		ZL, low(mul48_res)			; load the location
		ldi		ZH, high(mul48_res)			; of mult result
		ld		A, Z+						; get the 7 byte
		ld		B, Z+						; result from
		ld		C, Z+						; memory and put it
		ld		D, Z+						; into A-G
		ld		E, Z+
		ld		F, Z+
		ld		G, Z

		; SAVE THE PRODUCT INTO MEMORY
		ldi		ZL, low(product)			; load the product
		ldi		ZH, high(product)			; memory location
		st		Z+, A						; store the contents of
		st		Z+, B						; A-G into
		st		Z+, C						; the product location
		st		Z+, D
		st		Z+, E
		st		Z+, F
		st		Z, G

		; PUT THE GM INTO H-N
		ldi		ZL, low(GM)			; load the product
		ldi		ZH, high(GM)			; memory location
		ld		H, Z+					; Get the GM
		ld		I, Z+					; And store it into
		ld		J, Z+					; H-K
		ld		K, Z					; clear registers
		clr		L						; L-N, as
		clr		M						; the GM is limited to
		clr		N						; four bytes
		clr		XL

		; Divide the product by the GM
		rcall	BIGDIV

		; MOVE THE DIVISION RESULT INTO A-G
		MOVERESULT	O, P, Q, R, S, T, XL, A, B, C, D, E, F, G

		; Clear MULT OPERANDS and RESULT
		ldi		ZL, low(mul48_op1)			; load operand 1
		ldi		ZH, high(mul48_op1)			; memory location
		; 0-5: OP 1
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		; 6-11: OP 2
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		; 12-23: the RESULT
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero

		; Square root the result
		rcall		SQRT

		; Check if the result is 0
		rcall	CHECKCASE4

		; STORE THE RESULT INTO MEMORY AT PERIOD
		ldi		ZL, low(period)			; load the location
		ldi		ZH, high(period)		; of the period
		st		Z+, O					; store O-P
		st		Z+, P					; into PERIOD
		st		Z, Q					; as it is restricted to 3 bytes

		jmp		Grading					; this should be the very last instruction of your code

;-----------------------------------------------------------
;	Procedures and Subroutines
;-----------------------------------------------------------

;-------------------------------------------------
; Chooses a planet's GM based on the selected value
;-------------------------------------------------
PlanetChoice:
		clr		oloop
		ldi		oloop, $04					; Load 4 into OLOOP
		ldi		ZL, low(SelectedPlanet<<1)	; Load the low byte of SelectedPlanet
		ldi		ZH, high(SelectedPlanet<<1)	; Load the high byte of SelectedPlanet
		lpm		mpr, Z						; Load the value of SelectedPlanet
		cpi		mpr, $00					; Compare to 0
		brne	CHECK1						; Check for a different value
		ldi		ZL, low(MercuryGM<<1)		; Load the low byte of Mercury
		ldi		ZH, high(MercuryGM<<1)		; Load the high byte of Mercury
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK1:
		cpi		mpr, $01					; Compare to 1
		brne	CHECK2						; Check for a different value
		ldi		ZL, low(VenusGM<<1)			; Load the low byte of Venus
		ldi		ZH, high(VenusGM<<1)		; Load the high byte of Venus
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK2:
		cpi		mpr, $02					; Compare to 2
		brne	CHECK3						; Check for a different value
		ldi		ZL, low(EarthGM<<1)			; Load the low byte of Earth
		ldi		ZH, high(EarthGM<<1)		; Load the high byte of Earth
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK3:
		cpi		mpr, $03					; Compare to 3
		brne	CHECK4						; Check for a different value
		ldi		ZL, low(MarsGM<<1)			; Load the low byte of Mars
		ldi		ZH, high(MarsGM<<1)			; Load the high byte of Mars
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK4:
		cpi		mpr, $04					; Compare to 4
		brne	CHECK5						; Check for a different value
		ldi		ZL, low(JupiterGM<<1)		; Load the low byte of Jupiter
		ldi		ZH, high(JupiterGM<<1)		; Load the high byte of Jupiter
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK5:
		cpi		mpr, $05					; Compare to 5
		brne	CHECK6						; Check for a different value
		ldi		ZL, low(SaturnGM<<1)		; Load the low byte of Saturn
		ldi		ZH, high(SaturnGM<<1)		; Load the high byte of Saturn
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK6:
		cpi		mpr, $06					; Compare to 6
		brne	CHECK7						; Check for a different value
		ldi		ZL, low(UranusGM<<1)		; Load the low byte of Uranus
		ldi		ZH, high(UranusGM<<1)		; Load the high byte of Uranus
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK7:
		cpi		mpr, $07					; Compare to 7
		brne	CHECK8						; Check for a different value
		ldi		ZL, low(NeptuneGM<<1)		; Load the low byte of Neptune
		ldi		ZH, high(NeptuneGM<<1)		; Load the high byte of Neptune
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	CHECK8:
		cpi		mpr, $08					; Compare to 8
		brne	NOCHOICE						; Check for a different value
		ldi		ZL, low(FinalGM<<1)			; Load the low byte of Final (MAX)
		ldi		ZH, high(FinalGM<<1)		; Load the high byte of Final (MAX)
		lpm		mpr, Z						; Load value
		ldi		YL, low(GM)					; Load $0200 into Y
		ldi		YH, high(GM)
		st		Y, mpr						; Store the value into memory at Y
		rjmp	CHOSENLOOP					; Store the rest of the values
	; A loop to be executed 3 times
	CHOSENLOOP:
		lpm		mpr, Z+						; get the value
		st		Y+, mpr						; store the new value
		dec		oloop						; Decrement OLOOP
		cpi		oloop, $00					; check if we have looped 3 times
		brne	CHOSENLOOP					; Loop again
	NOCHOICE:
		ret									; this is done

;-------------------------------------------------
; Checks if the GM or orbital radius are
; less than 1001
;-------------------------------------------------
CHECKCASES1AND2:
; SPECIAL CASE 1: is RAD <= 1000
		ldi		YL, $E8								; Load 03E8 into 
		ldi		YH, $03								; YH:YL (value 1000)
		COMP64	A, B, zero, zero, zero, zero, zero, YL, YH, zero, zero, zero, zero, zero
		cpi		mpr, $02							; If the value in mpr is 02
		breq	NOCASEONE							; we continue
		ldi		ZL, low(velocity)					; otherwise, prepare to
		ldi		ZH, high(velocity)					; store -1 into VELOCITY
		ldi		XL, -1								; load -1 into X
		st		Z+, XL								; store the value
		ldi		XL, $FF								; get FF
		st		Z, XL								; store into byte 2
		rjmp	Grading								; go to grading
	NOCASEONE:
		; SPECIAL CASE 2: is GM <= 1000
		ldi		ZL, low(GM)							; load the memory
		ldi		ZH, high(GM)						; location of GM
		ld		A, Z+								; load byte 1
		ld		B, Z+								; load byte 2
		ld		C, Z+								; load byte 3
		ld		D, Z+								; load byte 4
		ldi		YL, $E8								; Load 03E8 into 
		ldi		YH, $03								; YH:YL (value 1000)
		; Compare GM to 1000
		COMP64	A, B, C, D, zero, zero, zero, YL, YH, zero, zero, zero, zero, zero
		cpi		mpr, $02							; If the value in mpr is 02
		breq	NOCASETWO							; we continue
		ldi		ZL, low(period)						; load the period location
		ldi		ZH, high(period)					; into ZH:ZLs
		ldi		XL, -1								; load -1 into X
		st		Z+, XL								; store the value
		ldi		XL, $FF								; get FF
		st		Z+, XL								; store into byte 2
		st		Z, XL								; store into byte 3
		rjmp	Grading								; go to grading
	NOCASETWO:
		ret

;-------------------------------------------------
; Checks if the velocity is zero
;-------------------------------------------------
CHECKCASE3:
		; Compare the result from sqrt(GM/R) to 0
		ldi		mpr, $00							; load 0 into mpr
		mov		zero, mpr							; load 0 into ZERO
		comp64	O, P, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero
		cpi		mpr, $01							; if mpr is not 02, it is not greater
		breq	CASETHREE							; store and stop
		ret											; Onward!
	CASETHREE:
		ldi		ZL, low (velocity)					; load the memory
		ldi		ZH, high(velocity)					; location of velocity
		ldi		XL, -2								; load -2 int X
		st		Z+, XL								; store X into velocity
		ldi		XL, $FF								; set the rest of the digits into
		st		Z, XL								; velocity as FF
		rjmp	Grading								; go to grading


CHECKCASE4:
		; Compare the result from sqrt(R^3 * 4pi^2/R GM) to 0
		ldi		mpr, $00							; load 0 into mpr
		mov		zero, mpr							; load 0 into ZERO
		ldi		mpr, $19							; load 25 into mpr
		comp64	O, P, Q, zero, zero, zero, zero, mpr, zero, zero, zero, zero, zero, zero
		cpi		mpr, $02							; if mpr is not 02, it is not greater
		brne	CASEFOUR							; store and stop
		ret											; Onward!
	CASEFOUR:
		ldi		ZL, low (period)					; load the memory
		ldi		ZH, high(period)					; location of period
		ldi		XL, -2								; load -2 int X
		st		Z+, XL								; store X into period
		ldi		XL, $FF								; set the rest of the digits into
		st		Z+, XL								; period as FF
		st		Z, XL
		rjmp	Grading								; go to grading


; FINDS THE SQUARE ROOT OF THE INPUT
; OPERAND 1: A, B, C, D, E, F
; COMP SPOT: H, I, J, K, L, M
; RESULT: O, P, Q, R, S, T, XL
SQRT:
		ldi		XH, $01										; Keep XH as 1
		; SET THE RESULT TO BE 1
		ldi		O, $01
		clr		P
		clr		Q
		clr		R
		clr		S
		clr		T
		clr		XL
	SQRTLOOP:
		ldi		XH, $01										; Keep XH as 1
		; GET THE CURRENT OPERAND
		ldi		ZL, low(mul48_op1)
		ldi		ZH, high(mul48_op1)
		ld		O, Z+
		ld		P, Z+
		ld		Q, Z+
		ld		R, Z+
		ld		S, Z+
		ld		T, Z+
		ld		XL, Z+
		; ADD ONE TO THE VALUE
		ADD64	O, P, Q, R, S, T, XL, XH, zero, zero, zero, zero, zero, zero	; increment by 1
		; SAVE IT BACK TO OPERAND 1
		ldi		ZL, low(mul48_op1)
		ldi		ZH, high(mul48_op1)
		ST		Z+, O
		ST		Z+, P
		ST		Z+, Q
		ST		Z+, R
		ST		Z+, S
		ST		Z+, T
		; SETUP MUL48 OPERAND 2
		ST		Z+, O
		ST		Z+, P
		ST		Z+, Q
		ST		Z+, R
		ST		Z+, S
		ST		Z+, T
		; CLEAR THE MULTIPLY RESULT
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		ST		Z+, zero
		; MULTIPLY
		rcall	MUL48										; Square the value
		; GET THE RESULT TO COMPARE WITH
		ldi		ZL, low(mul48_res)							; Load the result
		ldi		ZH, high(mul48_res)							; from memory
		ld		H, Z+
		ld		I, Z+
		ld		J, Z+
		ld		K, Z+
		ld		L, Z+
		ld		M, Z+
		ld		N, Z+
		; COMPARE THE RESULT WITH THE EXPECTATION
		COMP64  A, B, C, D, E, F, G, H, I, J, K, L, M, N	; Compare the results
		cpi		mpr, $03									; If greater than, go to subtract
		breq	DONESQRT
		cpi		mpr, $02									; otherwise if not less, we must be equal
		brne	DONTSUBSQRT									; stop completely
		rjmp	SQRTLOOP
	DONESQRT:
		; GET THE RESULT
		ldi		ZL, low(mul48_op1)							; get the memory location
		ldi		ZH, high(mul48_op1)							; of operand 1
		ld		O, Z+										; and load the values into O-XL
		ld		P, Z+
		ld		Q, Z+
		ld		R, Z+
		ld		S, Z+
		ld		T, Z+
		ld		XL, Z+
		ldi		XH, $01															; Keep XH as 1
		SUB64	O, P, Q, R, S, T, XL, XH, zero, zero, zero, zero, zero, zero	; Lower by 1
		ret																		; WE ARE DONE HERE
	DONTSUBSQRT:																
		; GET THE RESULT
		ldi		ZL, low(mul48_op1)							; get the memory location
		ldi		ZH, high(mul48_op1)							; of operand 1
		ld		O, Z+										; and load the values into O-XL
		ld		P, Z+
		ld		Q, Z+
		ld		R, Z+
		ld		S, Z+
		ld		T, Z+
		ld		XL, Z+
		ret													; WE ARE DONE HERE


; OPERAND 1: A, B, C, D, E, F, G
; OPERAND 2: H, I, J, K, L, M, N
; RESULT: O, P, Q, R, S, T, XL
BIGDIV:
	;---------- SUBTRACTION LOOP ----------
	DIVLOOP2:
		ldi		XH, $01
		ldi		mpr, $00
		mov		zero, mpr
		clr		mpr
		;mulby2	A, B, C, D, E, F, G								; restore the operand 1 by doubling
	; The inner loop, to prevent repeated multiplication
	INDIVLOOP2:
 		COMP64	A, B, C, D, E, F, G, H, I, J, K, L, M, N	; Check the new value compared to denominator
		cpi		mpr, $03									; if we cannot subtract another denominator
		breq	DIVDONE										; We are all done!
		
		; Otherwise, continue on!
		SUB64	A, B, C, D, E, F, G, H, I, J, K, L, M, N						; Subtract the denominator from the numerator
		ADD64	O, P, Q, R, S, T, XL, XH, zero, zero, zero, zero, zero, zero	; Add one to the result
		rjmp	INDIVLOOP2														; loop again

	; finished location
	DIVDONE:
		; divide the denominator by 2
		DIVBY2	H, I, J, K, L, M, N												; If the remainder is less
		COMP64	A, B, C, D, E, F, G, H, I, J, K, L, M, N						; than half the denominator
		cpi		mpr, $03														; finish up as is
		breq	DIVFINISH														; Otherwise, add 1 to the result
		ADD64	O, P, Q, R, S, T, XL, XH, zero, zero, zero, zero, zero, zero	; Add one to the result
	DIVFINISH:
		ret

;-------------------------------------------------
; Multiplies 2 6-byte numbers
; for a 12-byte result (max)
; Operands are fetched from memory
;-------------------------------------------------
MUL48:
		clr		zero					; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(mul48_op1)		; Load low byte
		ldi		YH, high(mul48_op1)		; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(mul48_res)		; Load low byte
		ldi		ZH, high(mul48_res)		; Load high byte

		; Begin outer for loop
		ldi		oloop, 6		; Load counter
	MUL48_OLOOP:
			; Set X to beginning address of A
			ldi		XL, low(mul48_op2)	; Load low byte
			ldi		XH, high(mul48_op2)	; Load high byte

			; Begin inner for loop
			ldi		iloop, 6		; Load counter
		MUL48_ILOOP:
				; Multiplication
				ld		O, X+			; Get byte of A operand
				ld		P, Y			; Get byte of B operand
				mul		O, P				; Multiply A and B
				ld		O, Z+			; Get a result byte from memory
				ld		P, Z+			; Get the next result byte from memory
				ld		Q, Z+			; Get byte 3
				ld		R, Z+			; Get byte 4
				ld		S, Z+			; Get byte 5
				ld		T, Z+			; Get byte 6
				; Load new values
				add		rlo, O			; rlo <= rlo + A
				adc		rhi, P			; rhi <= rhi + B + carry
				adc		Q, zero			; add a carry to bit 3
				adc		R, zero			; add a carry to bit 4
				adc		S, zero			; add a carry to bit 3
				adc		T, zero			; add a carry to bit 4
				; Get the carry bit and save it
				ld		O, Z			; Get a 5th byte from the result
				adc		O, zero			; Add carry to A
				st		Z, O			; Store 5th bit carry
				; Save the high and low value
				st		-Z, T			; Store byte 6
				st		-Z, S			; Store byte 5
				st		-Z, R			; Store byte 4
				st		-Z, Q			; Store byte 3
				st		-Z, rhi			; Store second byte to memory
				st		-Z, rlo			; Store first byte to memory
				; Increment Z
				adiw	ZH:ZL, 1		; Z <= Z + 1
				dec		iloop			; Decrement counter
				brne	MUL48_ILOOP		; Loop if iLoop != 0
				; End inner for loop

			; Get the carry bit and save it
			sbiw	ZH:ZL, 5			; Z <= Z - 5
			adiw	YH:YL, 1			; Y <= Y + 1
			dec		oloop				; Decrement counter
			brne	MUL48_OLOOP			; Loop if oLoop != 0
			; End outer for loop
		ret						; End a function with RET

;***********************************************************
;*	Custom stored data
;*	(feel free to edit these or add others)
;***********************************************************
SomeConstant:	.DB	0x86, 0xA4



;***end of your code***end of your code***end of your code***end of your code***end of your code***
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************

Grading:
		nop					; Check the results in data memory begining at address $0E00 (The TA will set a breakpoint here)
rjmp Grading


;***********************************************************
;*	Stored program data that you cannot change
;***********************************************************

; Contents of program memory will be changed during testing
; The label names (OrbitalRadius, SelectedPlanet, PlanetInfo, MercuryGM, etc) are not changed
; NOTE: All values are provided using the little-endian convention.
OrbitalRadius:	.DB	0x64, 0x19				; the radius that should be used during computations (in kilometers)
											; in this example, the value is 6,500 kilometers
											; the radius will be provided as a 16 bit unsigned value (unless you are
											; completing the extra credit, in which case the radius is an unsigned 24 bit value)

SelectedPlanet:	.DB	0x02, 0x00				; This is how your program knows which GM value should be used.
											; SelectedPlanet is an unsigned 8 bit value that provides you with the
											; index of the planet (and hence, tells you which GM value to use).
											; Note: only the first byte is used. The second byte is just for padding.
											; In this example, the value is 2. If we check the planet at index 2, (from the data below)
											; that corresponds to Earth.
											; if the value was 7, that would correspond to the planet Neptune

PlanetInfo:									; Note that these values will be changed during testing!
MercuryGM:		.DB	0x0E, 0x56, 0x00, 0x00	; Gravitational parameters will be provided as unsigned 32 bit integers (little-endian)
VenusGM:		.DB	0x24, 0xF5, 0x04, 0x00	; the units are in: (km * km * km)/(sec * sec)
EarthGM:		.DB	0x08, 0x15, 0x06, 0x00	; <-- note that this is 398,600
MarsGM:			.DB	0x4E, 0xA7, 0x00, 0x00
JupiterGM:		.DB	0x30, 0x13, 0x8D, 0x07	; A word of advice... treat these like an array, where each element
SaturnGM:		.DB	0xF8, 0xC7, 0x42, 0x02	; occupies 4 bytes of memory.
UranusGM:		.DB	0xD0, 0x68, 0x58, 0x00	; Mercury is at index 0, Venus is at index 1, ...and the final planet is at index 8.
NeptuneGM:		.DB	0x38, 0x4B, 0x68, 0x00
FinalGM:		.DB	0xFF, 0xFF, 0xFF, 0xFF


;***********************************************************
;*	Data Memory Allocation for Results
;*	Your answers need to be stored into these locations (using little-endian representation)
;*	These exact variable names will be used when testing your code!
;***********************************************************
.dseg
.org	$0E00						; data memory allocation for results - Your grader only checks $0E00 - $0E14
Quotient:		.byte 3				; This is the intermediate value that is generated while you are computing the satellite's velocity.
									; It is a 24 bit unsigned value.
Velocity:		.byte 2				; This is where you will store the computed velocity. It is a 16 bit signed number.
									; The velocity value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).
Product:		.byte 7				; This is the intermediate product that is generated while you are computing the orbital period.
Period:			.byte 3				; This is where the orbital period of the satellite will be placed.
									; It is a 24 bit signed value.
									; The period value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
