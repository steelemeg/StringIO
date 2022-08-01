TITLE String IO 

; Author: Megan Steele
; Last Modified: December 5, 2021
; Description: This program uses macros to support string processing and user input/output. It gathers 10 string inputs 
;				from the user, validates that they are integers in the correct range, sotres them in an array, and
;				displays the results, their sum, and their truncated average to the user.
;
; Note: In procedures that employ the USES directive but do not have LOCAL variables,
;		I've opted to separate the stack offsets for passed parameters from USES stack offsets.
;		I found this easier to understand and maintain, but if it is not considered correct please let me know!

INCLUDE Irvine32.inc

; ---------------------------------------------------------------------------------
; Name: mGetString
;
; Gets user's keyboard input and writes it to a memory location.
;
; Preconditions: None
;
; Postconditions: None . Restores original values of all registers used.
;
; Receives:	promptMessage = array/string address containing an appropriate message
;			inputString = array/string address where the user input should be stored
;			maxLength = value specifying the max number of bytes to accept from the user
;			bytesRead = address of the DWORD where the number of bytes entered should be stored
;
; Returns: Writes user input into the address of inputString. 
;			Writes the number of bytes entered into the memory location of bytesRead.
; ---------------------------------------------------------------------------------

mGetString MACRO promptMessage, inputString, maxLength, bytesRead
	PUSH	EAX
	PUSH	ECX
	PUSH	EDX

	mDisplayString promptMessage
	MOV		ECX, maxLength
	MOV		EDX, inputString
	CALL	ReadString
	; Track how many characters the user entered. ReadString automatically null terminates.
	MOV		EDX, bytesRead
	MOV		[EDX], EAX

	POP		EDX
	POP		ECX
	POP		EAX
ENDM

; ---------------------------------------------------------------------------------
; Name: mDisplayString
;
; Displays the string at the specified offset.
;
; Preconditions: None
;
; Postconditions: None. Restores original values of all registers used.
;
; Receives:	printTarget = array/string address containing an appropriate message
;
; Returns: Prints the string at the specified address.
; ---------------------------------------------------------------------------------
mDisplayString MACRO printTarget
	PUSH	EDX
	MOV		EDX, printTarget
	CALL	WriteString
	POP		EDX
ENDM

; Constants. Defining bounds of valid ASCII inputs so I don't have ambiguous hex values in my code.
ASCII_MINUS		=		2Dh
ASCII_PLUS		=		2Bh
ASCII_ZERO		=		30h
ASCII_NINE		=		39h
ASCII_BASE		=		30h
MAX_BUFFER		=		100
COUNTER_BASE	=		11

.data
greetingMessage		BYTE	"   Welcome to the String IO Project by Steele",13,10
					BYTE	"-------------------------------------------------------------",13,10
					BYTE	"**EC 1: Number each line of user input and display a running subtotal of valid inputs.",13,10,13,10
					BYTE	"This program will ask you for 10 signed decimal integers. Each integer needs to fit in",13,10
					BYTE	"a 32 bit signed integer. Invalid or non-numeric entries will not be accepted!",13,10,13,10
					BYTE	"After 10 valid entries have been provided, you will be shown all your valid inputs, their ",13,10
					BYTE	"sum, and their average value.",13,10,13,10,0
promptForInput		BYTE	". Please enter a signed integer: ",0
errorMessage		BYTE	"ERROR: Your entry was not valid or did not fit in a 32 byte signed integer. Please try again.",13,10,0
resultsMessage		BYTE	13,10,13,10,"You entered these valid numbers: ",13,10,0
finalSumMessage		BYTE	13,10,"The total sum of your valid entries is: ",0
avgMessage			BYTE	13,10,"The truncated average of your valid entries is: ",0
commaString			BYTE	", ",0
sumMessage			BYTE	"The current subtotal of your valid entries is: ",0
farewellMessage		BYTE	13,10,13,10,"Hope you enjoyed this program! Goodbye!",0

userInput			BYTE	MAX_BUFFER DUP(?)
inputLength			DWORD	?
validInputs			SDWORD	10 DUP(?)
sum					SDWORD	?

.code

; ---------------------------------------------------------------------------------
; Name: main
;
; Main procedure - pushes parameters onto the stack and calls other procedures within
; the program. Contains loop logic for getting 10 inputs from the user.
;
; ---------------------------------------------------------------------------------
main PROC
	; Greet the user and explain the purpose of the program
	mDisplayString	OFFSET greetingMessage

	; We need to retrieve valid user input ten times
	MOV		ECX, LENGTHOF validInputs
	; This is where validated numbers will be stored
	MOV		EDI, OFFSET validInputs
	
	; Ask the user for valid input
	_getTenNumbers:
		PUSH	OFFSET userInput 
		PUSH	OFFSET errorMessage
		PUSH	EDI 
		PUSH	OFFSET inputLength
		PUSH	ECX
		PUSH	OFFSET promptForInput
		CALL	ReadVal
		
		; Once valid input is written, get the running sum
		PUSH	TYPE validInputs
		PUSH	OFFSET sum
		PUSH	OFFSET SumMessage
		; The array length can always bee 10--it doesn't matter if the array hasn't 
		; been completed, since the "extra" values will just be zeroes.
		PUSH	LENGTHOF validInputs	
		PUSH	OFFSET validInputs
		CALL	ArraySum
		CALL	Crlf
		
		;EDI gets incremented to move through the array, and we repeat
		ADD		EDI, TYPE validInputs
		LOOP	_getTenNumbers

	; Show the user the array of valid inputs
	PUSH	OFFSET commaString
	PUSH	TYPE validInputs
	PUSH	OFFSET resultsMessage
	PUSH	LENGTHOF validInputs
	PUSH	OFFSET validInputs
	CALL	ArrayDisplay	

	; Show the final values' sum
	PUSH	TYPE validInputs
	PUSH	OFFSET sum
	PUSH	OFFSET finalSumMessage
	PUSH	LENGTHOF validInputs
	PUSH	OFFSET validInputs
	CALL	ArraySum

	; Show the truncated average
	PUSH	sum 
	PUSH	LENGTHOF validInputs
	PUSH	OFFSET avgMessage
	CALL	TruncatedAverage

	; Say goodbye to the user and exit
	mDisplayString	OFFSET farewellMessage
	INVOKE  ExitProcess, 0		
main ENDP

; ---------------------------------------------------------------------------------
; Name: ReadVal
;
; Gets user input. Validates it represents a signed 32 bit integer. 
;	If not, displays an error message and has the user try again.
;	Saves valid input to a specified memory location.
; For each entry, displays a line number. After the first input is accepted, displays
;	the running subtotal after each valid entry.
;
; Preconditions: None.
;
; Postconditions: None. Restores original value of EBP, restores registers, dereferences passed parameters.
;
; Receives:	Offset in memory of array/string where user input will be stored and read from
;			Offset in memory of array/string containing appropriate error messaging
;			Offset in memory of where to store validated input
;			Value corresponding to how many times valid input has been provided
;			Offset in memory of where to store the size of the user's input
;			Offset in memory of array/string contianing appropriate prompt messaging
;
; Returns:  Writes unvalidated user input into userInput.
;			Writes the number of bytes corresponding to the size of user input into bytesRead.
;			Writes validated inputs into the specified address in validInputs.
; 
; Future improvement: Make userInput (ebp + 28) and bytesRead (ebp + 16) local.
; ---------------------------------------------------------------------------------
ReadVal	PROC	USES EAX EBX ECX EDX ESI
	LOCAL	negativeInput:DWORD, currentDigit:DWORD, potentialInput:SDWORD
	MOV		negativeInput, 0
	_getInput:
		; Get user input written as a string into userInput. Size will be written into inputLength.
		PUSH	[EBP + 12]
		CALL	CurrentCount
		mGetString [EBP + 8], [EBP + 28], MAX_BUFFER, [EBP + 16]
		
		; Use ESI for the source
		MOV		ESI, [EBP + 28]

		; Counter is the number of bytes written by mGetString
		MOV		EBX, [EBP + 16]
		MOV		ECX, [EBX]

		; EBX will be used to track the value as it's validated and built.
		MOV		EBX, 0	
		; If the user enters nothing (empty input), display an error and re-prompt.
		CMP		ECX, 0
		JE		_error
		JMP		_checkSign

	_error:
		; Display error message
		mDisplayString	[EBP + 24]

		; Clear out EAX, reset EBX, and prompt the user for new input.
		XOR		EAX, EAX
		MOV		EBX, 0
		JMP		_getInput

	; Check if the first character is '-' or '+'.
	_checkSign:
		LODSB
		CMP		AL, ASCII_MINUS
		JE		_negative
		CMP		AL, ASCII_PLUS
		JE		_positive
		JMP		_unsigned

	_negative:
		; If it's '-', set the flag and move to the next index.
		MOV		negativeInput, 1
		DEC		ECX
		JMP		_processString
	_positive:
		; If it's '+', set the flag and move to the next index.
		MOV		negativeInput, 0
		DEC		ECX
		JMP		_processString
	_unsigned:
		MOV		negativeInput, 0
		; If the first character wasn't '-' or '+', reset ESI to the first character in the string.
		MOV		ESI, [EBP + 28]

	_processString:
		; Get the next character.
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
		; Then multiply the existing value of EBX by 10 and add or subtract the newest digit, depending on the sign.
		MOV		EAX, EBX
		MOV		EBX, 10
		IMUL	EBX
		JO		_error
		JC		_error
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

	; Store input correctly in the array, and return.
	_saveResult:
		; [EBP + 20] is the address of the current index in the validInputs array
		MOV		EDX, [EBP + 20]
		MOV		[EDX], EBX
		
	RET 24
ReadVal	ENDP

; ---------------------------------------------------------------------------------
; Name: WriteVal
;
; Given a value, converts it to a string and displays via mDisplayString macro.
;
; Preconditions: None.
;
; Postconditions: None. Restores original value of EBP, restores registers, dereferences passed parameters.
;
; Receives:	Value to be written passed on the stack.
;
; Returns:  String equivalent calculated and written to the console.
; ---------------------------------------------------------------------------------
WriteVal   PROC USES EAX EBX ECX EDX EDI
	LOCAL	outputLength:DWORD, tempString[12]:BYTE
	MOV		outputLength, 0
	
	; Get the value to be written
	MOV		EAX, [EBP + 8]
	; Get the offset of the output string -- needed to use LEA with the local variable
	LEA		EDI, tempString

	; Is the value negative?
	TEST	EAX, 80000000h
	JNE		_outputNegative
	JMP		_outputDigits

	_outputNegative:
		; If the value is negative, go ahead and add a leading '-' to the output.
		; Then get the absolute value of EAX before we start determining the number's digits.
		NEG		EAX
		; Need to use AL for a minute, so stash the value on the stack.
		PUSH	EAX
		MOV		EAX, 0
		MOV		AL, ASCII_MINUS		
		STOSB
		; Note: Don't increment outputLength here, since nothing is winding up on the stack.
		POP		EAX

	_outputDigits:
		; Opting to not display a positive sign for non-negative integers.
		XOR		EDX, EDX	; clear the remainder register
		; Note: Originally used IDIV/CDQ but this caused problems for edge cases. Since the sign
		; has already been parsed, this can be treated as unsigned math, so DIV works better.
		MOV		ECX, 10
		DIV		ECX	
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

; ---------------------------------------------------------------------------------
; Name: ArrayDisplay
;
; Given an array of numbers, displays their string equivalents to the user.
; Relies on WriteVal to convert the numbers to strings and display.
;
; Preconditions: None.
;
; Postconditions: None. Restores original value of EBP, restores registers, dereferences passed parameters.
;
; Receives:	Offset in memory of array/string containing comma/space delimiter.
;			Value corresponding to the bytes per data type in the source array.
;			Offset in memory of array/string containing appropriate user messaging.
;			Value corresponding to the number of items in the source array.
;			Offset in memory of the source array containing inputs. 
;
; Returns:  String equivalents calculated and written to the console.
; ---------------------------------------------------------------------------------
ArrayDisplay	PROC USES ECX ESI 
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	
	; Get the first element of the array
	MOV		ESI, [EBP + 8 + 8]
	; Get the number of elements in the array
	MOV		ECX, [EBP + 12 + 8]
	; Handle the last element separately since it doesn't need a trailing comma
	DEC		ECX
	; Display the message
	mDisplayString	[EBP + 16 + 8]

	_showValueAtIndex:
		PUSH	[ESI]
		CALL	WriteVal
		ADD		ESI, [EBP + 20 + 8]
		; Print trailing comma and space
		mDisplayString	[EBP + 24 + 8]
		LOOP	_showValueAtIndex
	
	PUSH	[ESI]
	CALL	WriteVal
	POP		EBP						; Restore EBP
	RET		24						; De-reference and return
ArrayDisplay	ENDP

; ---------------------------------------------------------------------------------
; Name: ArraySum
;
; Given an array of numbers, displays the sum of the contained values.
; Relies on WriteVal to convert the sum to a string and display.
;
; Preconditions: None.
;
; Postconditions: None. Restores original value of EBP, restores registers, dereferences passed parameters.
;
; Receives:	Value corresponding to the bytes per data type in the source array.
;			Offset in memory of location to store the final sum.
;			Offset in memory of array/string containing appropriate user messaging.
;			Value corresponding to the number of items in the source array.
;			Offset in memory of the source array containing inputs. 
;
; Returns:  Sum of values calculated and written to the console.
; ---------------------------------------------------------------------------------
ArraySum	PROC USES EAX ECX ESI 
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	
	; Get the first element of the array
	MOV		ESI, [EBP + 8 + 12]
	; Get the number of elements in the array
	MOV		ECX, [EBP + 12 + 12]
	; Display the message
	mDisplayString	[EBP + 16 + 12]

	; Step through the array, storing the summed values in EAX.
	MOV		EAX, 0
	_sumLoop:
		ADD		EAX, [ESI]
		ADD		ESI, [EBP + 24 + 12]
		LOOP	_sumLoop
	; Print the total result.
	PUSH	EAX
	CALL	WriteVal
	; Store the result -- we'll need it for finding the average
	MOV		EDX, [EBP + 20 + 12]
	MOV		[EDX], EAX

	POP		EBP						; Restore EBP
	RET		20						; De-reference the stack and return
ArraySum	ENDP

; ---------------------------------------------------------------------------------
; Name: CurrentCount
;
; Determines the current line number and writes it to the console.
; Relies on WriteVal to convert the number to a string and display.
; Uses ECX from the main loop; subtracts it from 11 to get the current line number.
;
; Preconditions: None.
;
; Postconditions: None. Restores original value of EBP, restores registers, dereferences passed parameters.
;
; Receives:	Value corresponding to how many times valid input has been provided.
;
; Returns:  Line number calculated and written to the console.
; ---------------------------------------------------------------------------------
CurrentCount	PROC	USES EAX EDX
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	; Get the counter value from the stack
	MOV		EAX, [EBP + 8 + 8]

	; Calculate the current line number by subtracting the loop counter from 11
	SUB		EAX, COUNTER_BASE
	NEG		EAX
	; Pass the value off to WriteVal
	PUSH	EAX
	CALL	WriteVal

	POP		EBP					; Restore EBP
	RET		4					; De-reference the 2 4-byte offsets on the stack and return
CurrentCount	ENDP

; ---------------------------------------------------------------------------------
; Name: Truncated Average
;
; Determines the truncated average and writes it to the console.
; Any fractional part is rounded down and ignored. So 1.2 => 1, 1.9 => 1, etc.
; Relies on WriteVal to convert the average to a string and display.
;
; Preconditions: None.
;
; Postconditions: None. Restores original value of EBP, restores registers, dereferences passed parameters.
;
; Receives:	Value of the sum (i.e. the dividend when calculating the average)
;			Number of data points (i.e. the divisor)
;			Offset in memory of array/string containing appropriate user messaging.
;
; Returns:  Truncated average calculated and written to the console.
; ---------------------------------------------------------------------------------
TruncatedAverage	PROC	USES EAX EBX EDX
	PUSH	EBP						; Preserve EBP
	MOV		EBP, ESP
	; Get the sum value
	MOV		EAX, [EBP + 16 + 12]
	CDQ
	; Get the number of inputs
	MOV		EBX, [EBP + 12 + 12]
	; Find the truncated average -- just ignore the remainder
	IDIV	EBX
	; Display the message and truncated average!
	mDisplayString [EBP + 8 + 12]
	PUSH	EAX
	CALL	WriteVal

	POP		EBP					; Restore EBP
	RET		12					; De-reference and return
TruncatedAverage	ENDP

END main
