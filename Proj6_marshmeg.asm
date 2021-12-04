TITLE String IO (Proj6_marshmeg.asm)

; Author: Megan Marshall
; Last Modified: December 4, 2021
; OSU email address:marshmeg@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6               Due Date: December 5, 2021
; Description: This program uses macros to support string processing and user input/output. It gathers 10 string inputs 
;				from the user, validates that they are integers in the correct range, sotres them in an array, and
;				displays the results, their sum, and their truncated average to the user. 

INCLUDE Irvine32.inc

; Macros!
mGetString MACRO promptMessage, inputString, maxLength, bytesRead
; Display a prompt (input parameter, by reference), then get the user’s keyboard input into a memory location (output parameter, by reference).
;You may also need to provide a count (input parameter, by value) for the length of input string you can accommodate and a provide a number of bytes read 
;(output parameter, by reference) by the macro.
	
	PUSH	EAX
	PUSH	ECX
	PUSH	EDX

	MOV		EDX, promptMessage
	CALL	WriteString
	MOV		ECX, maxLength
	MOV		EDX, inputString
	CALL	ReadString
	; Track how many characters the user entered.
	MOV		EDX, bytesRead
	MOV		[EDX], EAX

	POP		EDX
	POP		ECX
	POP		EAX
ENDM

mDisplayString MACRO printTarget
;Print the string which is stored in a specified memory location (input parameter, by reference).
	PUSH	EDX
	MOV		EDX, printTarget
	CALL	WriteString
	POP		EDX
ENDM

; Constants. Defining bounds of valid ASCII inputs so I don't have ambiguous hex values in my code.
ASCII_MINUS		EQU		2Dh
ASCII_PLUS		EQU		2Bh
ASCII_ZERO		EQU		30h
ASCII_NINE		EQU		39h
ASCII_BASE		EQU		30h
MAX_POS_VALUE	EQU		2147483647
MAX_BUFFER		EQU		100
COUNTER_BASE	EQU		11

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
promptForInput	BYTE	". Please enter a signed integer: ",0
errorMessage	BYTE	"ERROR: Your entry was not valid or did not fit in a 32 byte signed integer. Please try again.",13,10,0

; String to store unvalidated user input. 
userInput		BYTE	MAX_BUFFER DUP(?)
inputLength		DWORD	?

validInputs		SDWORD	10 DUP(?)
average			SDWORD	?

potentialInput	SDWORD	0
currentDigit	DWORD	0
; String to store output. 
displayOutput	BYTE	15 DUP(?)
pushedChars		DWORD	0

testString		BYTE	"-2147483648",0 
testValP		SDWORD	25354760
testValN		SDWORD	-25354760
.code
main PROC
	mDisplayString	OFFSET greeting

	; We need to retrieve valid user input ten times
	MOV		ECX, 10
	; This is where validated numbers will be stored
	MOV		EDI, OFFSET validInputs
	_getTenNumbers:

		MOV		EDI, OFFSET validInputs
		; Provide values for current line number (EC 1)
		PUSH	ECX	; 40
		PUSH	OFFSET displayOutput
		
		PUSH	OFFSET promptForInput ; 32
		PUSH	OFFSET userInput 
		PUSH	OFFSET errorMessage ; 24
		PUSH	EDI ; put edi here lol
		PUSH	OFFSET inputLength ; 16
		PUSH	OFFSET potentialInput
		PUSH	OFFSET currentDigit ; 8

		; Ask the user for valid input
		CALL	ReadVal
		
		; loop goes here, EDI gets incremented
		LOOP	_getTenNumbers


		
	;PUSH	testValN
	;PUSH	OFFSET displayOutput
	;CALL	WriteVal
	INVOKE  ExitProcess, 0		;exit to operating system
main ENDP

; ***************************************************************
; stuff
; effective range: -2,147,483,648 to 2,147,483,647

; ***************************************************************
ReadVal	PROC	USES EAX EBX ECX ESI
	LOCAL	negativeInput:DWORD 
	MOV		negativeInput, 0

	_getInput:
		; Get user input written as a string into userInput. Size will be in inputLength.
		PUSH	[EBP + 40]
		PUSH	[EBP + 36]
		CALL	CurrentCount
		mGetString [EBP + 32], [EBP + 28], MAX_BUFFER, [EBP + 16]
	
		; TODO good to this point - mGetString does what's intended
		
		; Use ESI for the source
		MOV		ESI, [EBP + 28]

		; syntax reminder
		;MOV		EAX, [EBX]

		; Counter is the number of bytes written by mGetString
		MOV		EBX, [EBP + 16]
		MOV		ECX, [EBX]

		; EBX will be used to track the value as it's validated and built.
		MOV		EBX, 0	
		; If the user enters nothing (empty input), display an error and re-prompt.
		CMP		ECX, 0
		JE		_error
		; Pre-check: if there are more than 11 characters, there's no way it will fit. (This allows for a sign flag)
		; TODO is this legit? try without these two lines.
		CMP		ECX, 11
		JA		_error
		JMP		_checkSign

	_error:
		; Display error message
		mDisplayString	[EBP + 24]

		; Clear out EAX, reset EBX, and prompt the user for new input.
		XOR		EAX, EAX
		MOV		EBX, 0
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
		MOV		ESI, [EBP + 28]

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
		; This is a bit of a mess. When moving an over-large positive number into potentialInput, no flags were being set,
		; and the value simply overflowed to negative numbers. Workaround: Check if the value was supposed to be negative, 
		; then if not, check if unsigned EBX exceeded 2^31 -1
		MOV		EAX, negativeInput
		CMP		EAX, 1
		JE		_saveResult
		CMP		EBX, MAX_POS_VALUE
		JA		_error

	;TODO increment validCount, store input correctly in the array, and return if the input was valid. Otherwise start over.
	_saveResult:
		; TODO next mess on the horizon: how to move through the validInputs array in main and pass the address in 
		; Increase validCount by 1. Future work--find a neater way to do this
		; Look up stack input output parameter references

		
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
WriteVal   PROC USES EAX EBX ECX EDX EDI
	LOCAL	outputLength:DWORD
	MOV		outputLength, 0
	
	; Get the value to be written
	MOV		EAX, [EBP + 12]
	; Get the offset of the output string
	MOV		EDI, [EBP + 8]

	; Is the value negative?
	TEST	EAX, 80000000h
	JNE		_outputNegative
	JMP		_outputDigits

	_outputNegative:
		; If the value is negative, go ahead and add a leading '-' to the output.
		; Then get the absolute value of EAX before we start determining the number's digits.
		NEG		EAX
		; Need to use AL for a minute, so stash the value in EBX.
		MOV		EBX, EAX
		MOV		EAX, 0
		MOV		AL, ASCII_MINUS		
		STOSB
		; Note: Don't increment outputLength here, since nothing is winding up on the stack.
		MOV		EAX, EBX

	_outputDigits:
		; Opting to not display a positive sign for non-negative integers.
		XOR		EDX, EDX	; clear the remainder register
		CDQ
		MOV		ECX, 10
		IDIV	ECX	
		; Now EAX contains the quotient and EDX contains the remainder
		MOV		EBX, EAX
		MOV		EAX, EDX
		ADD		EAX, ASCII_BASE
		; Opting to push all digits onto the stack. Could also build the string directly
		; and then reverse it, but would have to play games with EAX to use STOSB and IDIV.
		PUSH	EAX
		INC		outputLength
		; Recover the quotient, check if it is zero
		MOV		EAX, EBX
		CMP		EAX, 0
		JNE		_outputDigits
		; Otherwise, move our total character count into ECX in preparation for building a string.
		MOV		ECX, outputLength

	_buildString:
		; At this point all the digits are on the stack as DWORDS
		; ECX contains the number of digits to be popped and added to the output string array
		; STOSB takes the AL register into the EDI addressed location
		POP		EAX
		STOSB
		LOOP	_buildString

	; All digits popped off of the stack. Ready to write the string.
	mDisplayString	OFFSET displayOutput
		
	RET		8
WriteVal   ENDP

; ***************************************************************
; stuff. takes array reference and number of entries to sum (for EC 1)
; ***************************************************************
ArraySum	PROC
	RET
ArraySum	ENDP

; ***************************************************************
; stuff. prints the line number (for EC 1)
; DONE TODO Comments
; ***************************************************************
CurrentCount	PROC	USES EAX EDX
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	; Get the counter value from the stack
	MOV		EAX, [EBP + 12 + 8]
	; Get the output location from the stack
	MOV		EDX, [EBP + 8 + 8]
	; Calculate the current line number
	SUB		EAX, COUNTER_BASE
	NEG		EAX
	; Pass the value and the output offset off to WriteVal
	PUSH	EAX
	PUSH	EDX
	CALL	WriteVal

	POP		EBP					; Restore EBP
	RET		8					; De-reference the 2 4-byte offsets on the stack and return
CurrentCount	ENDP
; ***************************************************************
; stuff. takes sum and number of entries
; ***************************************************************
TruncatedAverage	PROC
	RET
TruncatedAverage	ENDP

END main
