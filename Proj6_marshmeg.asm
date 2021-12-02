TITLE User IO (Proj6_marshmeg.asm)

; Author: Megan Marshall
; Last Modified: December 1, 2021
; OSU email address:marshmeg@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6               Due Date: December 5, 2021
; Description: This program uses macros to support string processing and user input/output. It gathers 10 inputs from the user,
;				validates that they are integers in the correct range, sotres them in an array, and displays the results,
;				their sum, and their rounded average to the user. 

INCLUDE Irvine32.inc

; Macros!
mGetString MACRO promptMessage, userInput, inputLength, bytesRead
; Display a prompt (input parameter, by reference), then get the user’s keyboard input into a memory location (output parameter, by reference).
;You may also need to provide a count (input parameter, by value) for the length of input string you can accommodate and a provide a number of bytes read 
;(output parameter, by reference) by the macro.
ENDM

mDisplayString MACRO printTarget
;Print the string which is stored in a specified memory location (input parameter, by reference).
ENDM

; Constants. Defining bounds of valid ASCII inputs so I don't have ambiguous hex values in my code.
ASCII_MINUS		EQU		2Dh
ASCII_PLUS		EQU		2Bh
ASCII_ZERO		EQU		30h
ASCII_NINE		EQU		39h

.data
greeting		BYTE	"   Welcome to the String IO Project by Megan Marshall.",13,10
				BYTE	"-------------------------------------------------------------",13,10
				BYTE	"**EC 1: Number each line of user input and display a running subtoatl of valid inputs.",13,10,13,10
				BYTE	"This program will ask you for 10 signed decimal integers. ",13,10
				BYTE	"Each integer needs to fit in a 32 bit signed integer. Therefore, it should be no smaller ",13,10
				BYTE	"than -2,147,483,648 and no larger than 2,147,483,647.",13,10
				BYTE	"Invalid entries will not be accepted!",13,10,13,10
				BYTE	"After 10 valid entries have been provided, you will be shown all your valid inputs, their ",13,10
				BYTE	"sum, and their average value.",13,10,13,10,0
errorMessage	BYTE	"ERROR: Your entry was not valid or did not fit in a 32 byte signed integer. Please try again.",13,10,0
; String to store unvalidated user input. Assuming that the user will not enter more than 100 characters.
userInput		BYTE	100 DUP(?)
testString		BYTE	"2147483648",0 
inputLength		DWORD	10
validCount		DWORD	0
validInputs		SDWORD	10 DUP(?)
average			SDWORD	?
potentialInput	SDWORD	0
currentDigit	DWORD	0
negativeInput	DWORD	0
pushedChars		DWORD	0

.code
main PROC
	MOV		EDX, OFFSET greeting
	; TODO replace with mDisplayString
	CALL	WriteString
	CALL	ReadVal
	INVOKE  ExitProcess, 0		;exit to operating system
main ENDP

; ***************************************************************
; stuff
; effective range: -2,147,483,648 to 2,147,483,647
; ***************************************************************
ReadVal	PROC
	;TODO going to need: validCount, address of userInput, address of errorMessage, inputLength, potentialInput, currentDigit
	; Get the first 
	_getInput:
	; TODO call mGetString
	; EBX will be used to track the value as it's validated and built.
	MOV		EBX, 0	


	; TODO replace with actual parameter, stack reference
	; Use ESI for the source
	MOV		ESI, OFFSET testString
	; Replace with the number of bytes written by mGetString
	; TODO stack
	MOV		ECX, inputLength
	; If the user enters nothing (empty input), display an error and re-prompt.
	CMP		ECX, 0
	JE		_error
	JMP		_checkSign
	_error:
		; TODO offsets from the stack
		; Display error message
		; TODO replace with mDisplayString
		MOV		EDX, OFFSET errorMessage
		CALL	WriteString
		; Clear off anything we pushed onto the stack as a valid character
		; TODO use reference stack thing stuff blah;
		; TODO oops...we don't need the stack here. save the code for now, probably gonna need it for writeVal
		;MOV		ECX, pushedChars
		;CMP		ECX, 0
		;JA		_cleanUpStack
		;JMP		_startOver

		;_cleanUpStack:
		;	POP		EAX
		;	LOOP	_cleanUpStack

		; Clear out EAX, reset potentialInput, and prompt the user for new input.
		XOR		EAX, EAX
		MOV		EBX, 0
		; TODO temporary
		RET
		JMP		_getInput

	; Check if the first character is '-' or '+'
	_checkSign:
		LODSB
		CMP		AL, ASCII_MINUS
		JE		_negative
		CMP		AL, ASCII_PLUS
		JE		_positive
		JMP		_unsigned

	_negative:
		MOV		negativeInput, 1
		DEC		ECX
		JMP		_processString
	_positive:
		MOV		negativeInput, 0
		DEC		ECX
		JMP		_processString
	_unsigned:
		MOV		negativeInput, 0
		; If the first character wasn't '-' or '+', reset ESI to the first character in the string.
		MOV		ESI, OFFSET testString

	_processString:
		XOR		EAX, EAX
		LODSB
		; Validate! Is the character a sigit between 0 and 9? In ASCII this is [30h, 39h]
		CMP		AL, ASCII_ZERO
		JB		_error
		CMP		AL, ASCII_NINE
		JA		_error

		; If the character was valid, convert the ASCII value to hex value by subtracting 30h.
		SUB		AL, 30h
		; Stash this value in currentDigit for the time being
		MOV		currentDigit, EAX
		; Then multiply the existing value of EBX by 10 and add the newest digit. 
		MOV		EAX, EBX
		MOV		EBX, 10
		MUL		EBX
		; Check if we've already exceeded the size limit
		JC		_error
		ADD		EAX, currentDigit
		; Move the final value back into EBX and we're ready for the next digit.
		MOV		EBX, EAX
				
		LOOP	_processString

	; TODO if there was a negative sign, flip it
	; TODO use stack etc
	; At this point the string has been validated and built. If there was a leading negative sign, negate the value.
	MOV		EAX, negativeInput
	CMP		EAX, 1
	JE		_flipSign
	JMP		_checkSize

	_flipSign:
		; This is a cheap trick; when the input is exactly -214748368, it was overflowing the register when negated.
		; Workaround: decrement the unsigned value, negate it, then decrement it again. Restores the original value and avoids overflow.
		DEC		EBX
		NEG		EBX
		DEC		EBX

	; Does the final result fit in a 32 bit register?
	_checkSize:
		JO	_error

	;TODO Check if the number fits in a 32 bit signed integer
	;TODO increment validCount, store input, and return if the input was valid. Otherwise start over.
	MOV		potentialInput, EBX
	MOV		EAX, potentialInput
	CALL	WriteInt
	RET
ReadVal	ENDP

; ***************************************************************
; stuff
; ***************************************************************
WriteVal   PROC
	; temp, add in parameters TODO
	;MOV		EAX, printMe
	;XOR		EDX, EDX	; clear the remainder register
	;CDQ
	;MOV		ECX, 10

	
	RET
WriteVal   ENDP

; ***************************************************************
; stuff. takes array reference and number of entries to sum (for EC 1)
; ***************************************************************
ArraySum	PROC
	RET
ArraySum	ENDP


; ***************************************************************
; stuff. takes sum and number of entries
; ***************************************************************
TruncatedAverage	PROC
	RET
TruncatedAverage	ENDP

END main
