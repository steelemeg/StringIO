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
				BYTE	"This program will ask you for 10 signed decimal integers. Each integer needs to fit in",13,10
				BYTE	"a 32 bit signed integer. Invalid or non-numeric entries will not be accepted!",13,10,13,10
				BYTE	"After 10 valid entries have been provided, you will be shown all your valid inputs, their ",13,10
				BYTE	"sum, and their average value.",13,10,13,10,0
promptForInput	BYTE	". Please enter a signed integer: ",0
errorMessage	BYTE	"ERROR: Your entry was not valid or did not fit in a 32 byte signed integer. Please try again.",13,10,0
resultsMessage	BYTE	13,10,13,10,"You entered these valid numbers: ",13,10,0
finalSumMessage	BYTE	13,10,"The total sum of your valid entries is: ",0
avgMessage		BYTE	13,10,"The truncated average of your valid entries is: ",0

; String to store unvalidated user input. 
userInput		BYTE	MAX_BUFFER DUP(?)
inputLength		DWORD	?

validInputs		SDWORD	10 DUP(?)
average			SDWORD	?

testString		BYTE	"-2147483648",0 
testValP		SDWORD	2
testValN		SDWORD	-2
.code
main PROC
	mDisplayString	OFFSET greeting

	; We need to retrieve valid user input ten times
	; TODO for tsting
	;MOV		ECX, LENGTHOF validInputs
	MOV			ECX, 2
	; This is where validated numbers will be stored
	MOV		EDI, OFFSET validInputs

	_getTenNumbers:
		
		PUSH	OFFSET userInput 
		PUSH	OFFSET errorMessage ; 24
		PUSH	EDI 
		PUSH	OFFSET inputLength ; 16
		PUSH	ECX
		PUSH	OFFSET promptForInput ; 8

		; Ask the user for valid input
		CALL	ReadVal
		
		; Once valid input is written, EDI gets incremented and we repeat
		ADD		EDI, TYPE validInputs
		LOOP	_getTenNumbers

	; Show the array
	PUSH	TYPE validInputs
	PUSH	OFFSET resultsMessage
	PUSH	LENGTHOF validInputs
	PUSH	OFFSET validInputs
	CALL	ArrayDisplay	
	CALL	CRLF
	PUSH	testValN
	CALL	WriteVal
	CALL	CRLF

	PUSH	testValP
	CALL	WriteVal
	CALL	CRLF
	INVOKE  ExitProcess, 0		;exit to operating system
main ENDP

; ***************************************************************
; stuff
; effective range: -2,147,483,648 to 2,147,483,647

; ***************************************************************
ReadVal	PROC	USES EAX EBX ECX ESI
	LOCAL	negativeInput:DWORD, currentDigit:DWORD, potentialInput:SDWORD 
	MOV		negativeInput, 0

	_getInput:
		; Get user input written as a string into userInput. Size will be in inputLength.
		PUSH	[EBP + 12]
		CALL	CurrentCount
		mGetString [EBP + 8], [EBP + 28], MAX_BUFFER, [EBP + 16]
		
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
		;CMP		ECX, 11
		;JA		_error
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

	;TODO turns out my logic is still wrong
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
			;JC		_error
			JMP		_nextDigit

		_addNextDigit:
			ADD		EAX, currentDigit
			JO		_error
			JC		_error
			JMP		_nextDigit

		; Move the final value back into EBX and we're ready for the next digit.
		_nextDigit:
		MOV		EBX, EAX
				
		LOOP	_processString

	; At this point the string has been validated and an integer equivalent built in EBX.

	_checkFinalSize:
		; Does the final result fit in a SDWORD? Clear the carry flag first to avoid weird bugs
		CLC
		MOV		potentialInput, 0
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

	; Store input correctly in the array, and return.
	_saveResult:
		; [EBP + 20] is where the array starts
		MOV		EDX, [EBP + 20]
		MOV		[EDX], EBX
		; TODO all this needs to be cleaned up
		MOV		EAX, EBX
		CALL	WriteDec
		CALL	CRLF
		PUSH	EBX
		CALL	WriteVal
		CALL	Crlf
		
	RET 24
ReadVal	ENDP

; ***************************************************************
; stuff
; Approach: Divide the number by 10. The remainder is the last digit.
;		Push that onto the stack. Track how much stuff is pushed.
;		Loop. When the quotient is 0, that is the last digit.
;		Pop off each character and call mDisplayString
; needs: value to be written
; ***************************************************************
WriteVal   PROC USES EAX EBX ECX EDX EDI
	LOCAL	outputLength:DWORD, tempString[12]:BYTE
	MOV		outputLength, 0
	
	; Get the value to be written
	MOV		EAX, [EBP + 8]
	; Get the offset of the output string
	LEA		EDI, tempString

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

	; Add a null-terminator because there's all sorts of mess on the stack.
	MOV		EAX, 0
	STOSB

	; Ready to write the string.
	LEA		EBX, tempString
	mDisplayString	EBX
		
	RET		4
WriteVal   ENDP

; ***************************************************************
; stuff. takes array reference and number of entries 
; string buffer, type, message, length, first offset
; ***************************************************************
ArrayDisplay	PROC USES ECX ESI 
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	
	; Get the first element of the array
	MOV		ESI, [EBP + 8 + 8]
	; Get the number of elements in the array
	MOV		ECX, [EBP + 12 + 8]
	; Display the message
	mDisplayString	[EBP + 16 + 8]

	_showValueAtIndex:
		PUSH	[ESI]
		CALL	WriteVal
		ADD		ESI, [EBP + 20 + 8]
		LOOP	_showValueAtIndex

	POP		EBP						; Restore EBP
	RET		16						; De-reference and return
ArrayDisplay	ENDP

; ***************************************************************
; stuff. takes array reference and number of entries to sum (for EC 1)
; message, length, first offset
; ***************************************************************
ArraySum	PROC USES ECX ESI 
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	
	; Get the first element of the array
	MOV		ESI, [EBP + 8 + 8]
	; Get the number of elements in the array
	MOV		ECX, [EBP + 12 + 8]
	; Display the message
	mDisplayString	[EBP + 16 + 8]

	POP		EBP						; Restore EBP
	RET		16						; De-reference the stack and return
ArraySum	ENDP
; ***************************************************************
; stuff. prints the line number (for EC 1)
; DONE TODO Comments
; ***************************************************************
CurrentCount	PROC	USES EAX EDX
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	; Get the counter value from the stack
	MOV		EAX, [EBP + 8 + 8]

	; Calculate the current line number
	SUB		EAX, COUNTER_BASE
	NEG		EAX
	; Pass the value off to WriteVal
	PUSH	EAX
	CALL	WriteVal

	POP		EBP					; Restore EBP
	RET		4					; De-reference the 2 4-byte offsets on the stack and return
CurrentCount	ENDP
; ***************************************************************
; stuff. takes sum and number of entries
; ***************************************************************
TruncatedAverage	PROC
	RET
TruncatedAverage	ENDP

END main
