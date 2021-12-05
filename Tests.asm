TITLE Squares Array Filler     (SquareArrayFiller.asm)

; Author: Redfield
; CS271 Example Program        
; Description:  This program asks the user how many squares are required
;	and puts that many squares into an array so they can be displayed
;	in reverse order.

; Implementation note: Parameters are passed on the system stack.

INCLUDE Irvine32.inc

MAXSIZE	= 100

.data
squareArray DWORD   MAXSIZE DUP(?)
count       DWORD   ?
prompt1     BYTE    "How many squares? ",0

.code
main PROC
	PUSH    OFFSET count
	CALL    getCount            ;Get the user's number
	PUSH    OFFSET squareArray
	PUSH    count
	CALL    fillArray           ;Put that many squares into the array
	MOV		EDX, [squareArray]
	CALL	WriteDec
	CALL	WaitMsg
	PUSH    OFFSET squareArray
	PUSH    count
	CALL    showRevList         ;Print the array in reverse order
	INVOKE  ExitProcess, 0		;exit to operating system
main ENDP

; ***************************************************************
; Procedure to get the user's input. Note: input not validated.
; receives: address of count on system stack
; returns: user input in global count
; preconditions: none
; registers changed: eax, ebx, edx
; ***************************************************************
getCount	PROC
	PUSH    ebp
	MOV     ebp,esp
	MOV     edx,OFFSET prompt1
	CALL    WriteString		;prompt user
	CALL    ReadInt			;get user's number
	MOV     ebx,[ebp+8]		;address of count in ebx
	MOV     [ebx],eax			;store in global variable
	POP     ebp
	RET     4
getCount	ENDP

; ***************************************************************
; Procedure to put count squares into the array.
; receives: address of array and value of count on system stack
; returns: first count elements of array contain consecutive squares
; preconditions: count is initialized, 1 <= count <= 100
; registers changed: eax, ebx, ecx, edi
; ***************************************************************
fillArray   PROC
	PUSH    ebp
	MOV     ebp,esp
	MOV     ecx,[ebp+8]     ;count in ecx
	MOV     edi,[ebp+12]    ;address of array in edi
	
	MOV     ebx,0
_fillLoop:
	MOV     eax,ebx         ;calculate squares and store in consecutive array elements
	MUL     ebx
	MOV     [edi],eax
	ADD     edi,4
	INC     ebx
	LOOP    _fillLoop
	
	POP     ebp
	RET     8
fillArray   ENDP

; ***************************************************************
; Procedure to display array in reverse order.
; receives: address of array and value of count on system stack
; returns: first count elements of array contain consecutive squares
; preconditions: count is initialized, 1 <= count <= 100
;                and the first count elements of array initialized
; registers changed: eax, ebx, edx, esi
; ***************************************************************
showRevList	PROC
	PUSH    ebp
	MOV     ebp,esp
	MOV     edx,[ebp+8]		;count in edx
	MOV     esi,[ebp+12]		;address of array in esi
	DEC     edx				;scale edx for [count-1 down to 0]

_showElement:
	MOV     eax,edx			;display n
	CALL    WriteDec
	MOV     al,'.'
	CALL    WriteChar
	MOV     al,32
	CALL    WriteChar
	CALL    WriteChar
	MOV     eax,[esi+edx*4]	;start with last element
	CALL    WriteDec			;display n-squared
	CALL    CrLf
	DEC     edx
	CMP     edx,0
	JGE     _showElement
	
	POP     ebp
	RET     8
showRevList	ENDP

END main
