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
mGetString MACRO lineNumber, promptMessage, userInput, inputLength, bytesRead
; Display a prompt (input parameter, by reference), then get the user’s keyboard input into a memory location (output parameter, by reference).
;You may also need to provide a count (input parameter, by value) for the length of input string you can accommodate and a provide a number of bytes read 
;(output parameter, by reference) by the macro.
	; TODO add line number from WriteVal
	MOV		EDX, OFFSET	promptMessage
	CALL	WriteString
	MOV		ECX, inputLength
	MOV		EDX, OFFSET userInput
	CALL	ReadString
	; Track how many characters the user entered. Note this is +1 for null terminated.
	MOV		bytesRead, EAX		
ENDM

mDisplayString MACRO printTarget
;Print the string which is stored in a specified memory location (input parameter, by reference).
	MOV		EDX, printTarget
	CALL	WriteString
ENDM

; Constants. Defining bounds of valid ASCII inputs so I don't have ambiguous hex values in my code.
ASCII_MINUS		EQU		2Dh
ASCII_PLUS		EQU		2Bh
ASCII_ZERO		EQU		30h
ASCII_NINE		EQU		39h
ASCII_BASE		EQU		30h
MAX_POS_VALUE	EQU		2147483647

.data
greeting		BYTE	"   Welcome to the String IO Project by Megan Marshall.",13,10
				BYTE	"-------------------------------------------------------------",13,10
				BYTE	"**EC 1: Number each line of user input and display a running subtotal of valid inputs.",13,10,13,10
				BYTE	"This program will ask you for 10 signed decimal integers. ",13,10
				BYTE	"Each integer needs to fit in a 32 bit signed integer. Therefore, it should be no smaller ",13,10
				BYTE	"than -2,147,483,648 and no larger than 2,147,483,647.",13,10
				BYTE	"Invalid entries will not be accepted!",13,10,13,10
				BYTE	"After 10 valid entries have been provided, you will be shown all your valid inputs, their ",13,10
				BYTE	"sum, and their average value.",13,10,13,10,0
promptForInput	BYTE	"Please enter a signed integer: ",0
errorMessage	BYTE	"ERROR: Your entry was not valid or did not fit in a 32 byte signed integer. Please try again.",13,10,0
; String to store unvalidated user input. Assuming that the user will not enter more than 100 characters.
userInput		BYTE	100 DUP(?)
inputLength		DWORD	?

validCount		DWORD	0
validInputs		SDWORD	10 DUP(?)
average			SDWORD	?

potentialInput	SDWORD	0
currentDigit	DWORD	0
; String to store output. 
displayOutput	BYTE	100 DUP(?)
pushedChars		DWORD	0

testString		BYTE	"-2147483648",0 
testValP		SDWORD	19250387
testValN		SDWORD	-25354760
.code
main PROC
	mDisplayString	OFFSET greeting

	;CALL	ReadVal
	CALL	WriteVal
	INVOKE  ExitProcess, 0		;exit to operating system
main ENDP

; ***************************************************************
; stuff
; effective range: -2,147,483,648 to 2,147,483,647
; ***************************************************************
ReadVal	PROC
	LOCAL	negativeInput:DWORD 
	MOV		negativeInput, 0
	;TODO going to need: validCount, address of userInput, address of errorMessage, inputLength, potentialInput, currentDigit
	; Get the first 
	_getInput:
	; TODO call mGetString
	;mGetString 
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
	; Pre-check: if there are more than 11 characters, there's no way it will fit. (This allows for a sign flag)
	CMP		ECX, 11
	JA		_error
	JMP		_checkSign
	_error:
		; TODO offsets from the stack
		; Display error message
		; TODO replace with mDisplayString
		mDisplayString	OFFSET errorMessage
		CALL	WriteString

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
		; Validate! Is the character a sigit between 0 and 9?
		CMP		AL, ASCII_ZERO
		JB		_error
		CMP		AL, ASCII_NINE
		JA		_error

		; If the character was valid, convert the ASCII value to hex value by subtracting 30h.
		SUB		AL, ASCII_BASE
		; Stash this value in currentDigit for the time being
		MOV		currentDigit, EAX
		; Then multiply the existing value of EBX by 10 and add or subtrac the newest digit, depending on the sign.
		MOV		EAX, EBX
		MOV		EBX, 10
		MUL		EBX
		CMP		negativeInput, 1
		JE		_subtractNextDigit
		JMP		_addNextDigit
		
		_subtractNextDigit:
			SUB		EAX, currentDigit
			JO		_error
			JMP		_nextDigit

		_addNextDigit:
			ADD		EAX, currentDigit
			JO		_error
			JMP		_nextDigit

		; Move the final value back into EBX and we're ready for the next digit.
		_nextDigit:
		MOV		EBX, EAX
				
		LOOP	_processString

	; At this point the string has been validated and an integer equivalent built in EBX.

	_checkFinalSize:
		; Does the final result fit in a SDWORD? Clear the carry flag first to avoid weird bugs
		CLC
		ADD		potentialInput, EBX

		JO		_error		
		MOV		EAX, negativeInput
		CMP		EAX, 1
		JE		_saveResult
		CMP		EBX, MAX_POS_VALUE
		JA		_error

	;TODO increment validCount, store input, and return if the input was valid. Otherwise start over.
	_saveResult:
		MOV		EAX, potentialInput
		CALL	WriteInt
	RET
ReadVal	ENDP

; ***************************************************************
; stuff
; Approach: Divide the number by 10. The remainder is the last digit.
;		Push that onto the stack. Track how much stuff is pushed.
;		Loop. When the quotient is 0, that is the last digit.
;		Pop off each character and call mDisplayString
; needs: pushedChars, address of result string
; ***************************************************************
WriteVal   PROC
	LOCAL	negativeOutput:DWORD, outputLength:DWORD
	MOV		negativeOutput, 0
	MOV		outputLength, 0
	; temp, add in parameters TODO
	MOV		EAX, testValP
	MOV		EDI, OFFSET displayOutput
	; TODO use reference stack thing stuff blah
	; If the value is negative, the first character is '-'. 
	TEST	EAX, 80000000h
	JNE		_outputNegative
	JMP		_outputDigits

	_outputNegative:
		MOV		AL, ASCII_MINUS
		STOSB	
		INC		outputLength

	_outputDigits:
		; Opting to not display a positive sign for non-negative integers.
		; Bit tricky since both IDIV and STOSB rely in EAX.
		; STOSB takes the AL register into the EDI addressed location
		XOR		EDX, EDX	; clear the remainder register
		CDQ
		MOV		ECX, 10
		IDIV	ECX	
		; Now EAX contains the quotient and EDX contains the remainder
		; Stash EAX so we can use STOSB
		MOV		EBX, EAX
		MOV		EAX, EDX
		STOSB
		INC		outputLength
		; Recover the quotient, check if it is zero
		MOV		EAX, EBX
		CMP		EAX, 0
		JE		_readyToDisplay
		JMP		_outputDigits

	_readyToDisplay:
		mDisplayString	OFFSET displayOutput


	


	
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
