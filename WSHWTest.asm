;-----------------------------------------------------------------------------
;
;  WonderSwan Hardware Test
;         by Fredrik Ahlström, 2023
;         https://github.com/FluBBaOfWard/WSHWTest
;
;  UP/DOWN    - Choose option
;  A          - Start
;
;  Assemble with: 
;                   nasm -f bin -o WSHWTest.wsc WSHWTest.asm
;
;-----------------------------------------------------------------------------

	ORG 0x0000
	CPU 186
	BITS 16

SECTION .data
	%include "WonderSwan.inc"

	MYSEGMENT equ 0xf000
	foregroundMap equ WS_TILE_BANK - MAP_SIZE
	backgroundMap equ foregroundMap - MAP_SIZE
	spriteTable equ backgroundMap - SPR_TABLE_SIZE

	PSR_S equ 0x80
	PSR_Z equ 0x40
	PSR_P equ 0x04

SECTION .text
	;PADDING 15

initialize:
	cli
	cld

;-----------------------------------------------------------------------------
; Initialize registers and RAM
;-----------------------------------------------------------------------------
	mov ax, MYSEGMENT
	mov ds, ax
	xor ax, ax
	mov es, ax			; Set ES segment to 0x0000 (RAM).

	; Setup stack
	mov bp, ax
	mov ss, ax
	mov sp, WS_STACK

	; Clear Ram
	mov di, 0x0100
	mov cx, 0x1E80
	rep stosw

	out IO_SRAM_BANK,al

;-----------------------------------------------------------------------------
; Initialize variables
;-----------------------------------------------------------------------------
	mov word [es:globalFrameCounter], 0
	mov word [es:lfsr1], 0x0234
	mov word [es:lfsr2], 0x1234

;-----------------------------------------------------------------------------
; Initialize video
;-----------------------------------------------------------------------------
	in al, SYSTEM_CTRL2
;	or al, VMODE_4C | VMODE_CLEANINIT
	or al, VMODE_CLEANINIT
	out SYSTEM_CTRL2, al

	xor ax, ax
	mov al, BG_MAP( backgroundMap ) | FG_MAP( foregroundMap )
	out IO_SCR_AREA, al

	mov al, SPR_AREA( spriteTable )
	out IO_SPR_AREA, al

	in al, IO_LCD_IF_CTRL
	or al, LCD_ON
	out IO_LCD_IF_CTRL, al

	xor al, al
	out IO_LCD_SEG_DATA, al

;-----------------------------------------------------------------------------
; Register our interrupt handlers
;-----------------------------------------------------------------------------
	mov di, 0*4		; Division error vector
	mov word [es:di], divisionErrorHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 1*4		; Int1
	mov word [es:di], int1InstructionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 2*4		; NMI
	mov word [es:di], nmiHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 3*4		; Int3
	mov word [es:di], int3InstructionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 4*4		; BRKV
	mov word [es:di], overflowExceptionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 5*4		; CHKIND
	mov word [es:di], boundsExceptionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 6*4		; Undefined instruction vector
	mov word [es:di], undefinedInstructionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 7*4		; POLL
	mov word [es:di], pollExceptionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 0x10*4	; output char vector
	mov word [es:di], outputCharHandler
	mov word [es:di + 2], MYSEGMENT

	mov ax, INT_BASE	; 0x20
	out IO_INT_VECTOR, al

	mov di, INTVEC_VBLANK_START
	add di, ax
	shl di, 2
	mov word [es:di], vblankInterruptHandler
	mov word [es:di + 2], MYSEGMENT

	; Clear HBL & Timer
	xor ax, ax
	out IOw_H_BLANK_TIMER, ax
	out IO_TIMER_CTRL, al

	; Acknowledge all interrupts
	dec al
	out INT_CAUSE_CLEAR, al

	; Enable VBL interrupt
	mov al, INT_VBLANK_START 
	out IO_INT_ENABLE, al

	; We have finished initializing, interrupts can now fire again
	sti

;-----------------------------------------------------------------------------
; Copy font tile data into WS's tile mem
;-----------------------------------------------------------------------------
	; Copy font tile data to tile bank 1
	xor ax,ax
	mov si, MonoFont
	mov di, WS_TILE_BANK + 16*16*2
	mov cx, 8*16*6
monoFontLoop:
	lodsb
	stosw
	dec cx
	jnz monoFontLoop

;-----------------------------------------------------------------------------
; Copy font palette into WSC's palette area
;-----------------------------------------------------------------------------

	; Copy 2-colour (2 bytes per colour) font palette to 
	; beginning of palettes area (becoming palette 0)
	mov si, FontTilePalette
	mov di, WSC_PALETTES
	mov cx, 2
	rep movsw

	mov al, 0xf0
	out IO_LCD_GRAY_01, al
	mov ax, 0x0010
	out IOw_SCR_LUT_0, ax

;-----------------------------------------------------------------------------
; Make background map point to our tiles, essentially "painting" the
; background layer with our tiles, coloured as per our palettes
;-----------------------------------------------------------------------------
main:
	call clearScreen

	mov si, headLineStr
	call writeString

	mov si, menuTestAllStr
	call writeString
	mov si, menuTestLogicStr
	call writeString
	mov si, menuTestArithmeticStr
	call writeString
	mov si, menuTestRolShiftStr
	call writeString
	mov si, menuTestMiscStr
	call writeString

	; Turn on display
	mov al, BG_ON
	out IO_DISPLAY_CTRL, al

;-----------------------------------------------------------------------------
;
; BEGIN main area
;
;-----------------------------------------------------------------------------
mainLoop:
	hlt					; Wait until next interrupt

	mov al, KEYPAD_READ_ARROWS_H
	out IO_KEYPAD, al
	nop
	nop
	nop
	nop
	in al, IO_KEYPAD
	mov bl, al
	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	nop
	nop
	nop
	nop
	in al, IO_KEYPAD
	and al, 0x0F
	shl bl, 4
	or al, bl
	mov bl, [es:keysHeld]
	mov [es:keysHeld], al
	xor bl, al
	and bl, al
	mov [es:keysDown], bl

	; Check player input
;	test al, PAD_RIGHT
;	jnz speed_up

;	test al, PAD_LEFT
;	jnz speed_down

	mov cl, [es:menuYPos]
	test bl, (PAD_UP<<4)
	jz dontMoveUp
	sub cl, 1
	jns dontMoveUp
	mov cl, 0
dontMoveUp:
	test bl, (PAD_DOWN<<4)
	jz dontMoveDown
	add cl, 1
	cmp cl, 4			; Entry count
	js dontMoveDown
	mov cl, 4			; Entry count
dontMoveDown:
	mov [es:menuYPos], cl

	mov ch, cl
	add ch, 1
	mov byte [es:cursorXPos], 0
	mov [es:cursorYPos], ch
	mov al, ' '
	int 0x10
	add ch, 1
	mov byte [es:cursorXPos], 0
	mov [es:cursorYPos], ch
	mov al, '>'
	int 0x10
	add ch, 1
	mov byte [es:cursorXPos], 0
	mov [es:cursorYPos], ch
	mov al, ' '
	int 0x10

	test bl, PAD_A
	jz mainLoop
	call clearScreen

	cmp cl, 0
	jz testAll
	cmp cl, 1
	jz testLogic
	cmp cl, 2
	jz testArithmetic
	cmp cl, 3
	jz testRolShift
	cmp cl, 4
	jz testMisc
	; No input, restart main loop
	jmp mainLoop
;-----------------------------------------------------------------------------
;
; END main area
;
;-----------------------------------------------------------------------------
testAll:
	call testEqu

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testLogic:
	call testEqu

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testArithmetic:
	call testAdd8

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testRolShift:
	call testRol8

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testMisc:
	call testDaa

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
; Test equality by CMP, SUB & XOR of all byte/word values.
;-----------------------------------------------------------------------------
testEqu:
	mov si, testingEquStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	mov cl, 0
testEqu8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], cl
	mov al, cl
	cmp al, cl
	jnz equ8Failed
	sub al, cl
	jnz equ8Failed
	mov al, cl
	xor al, cl
	jnz equ8Failed
continueEqu8:
	inc cl
	jnz testEqu8Loop

	mov cl, 0
testNeq8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], cl
	mov al, cl
	inc al
	cmp al, cl
	jz neq8Failed
	sub al, cl
	jz neq8Failed
	mov al, cl
	inc al
	xor al, cl
	jz neq8Failed
continueNeq8:
	inc cl
	jnz testNeq8Loop

	hlt
	mov al, 10
	int 0x10
	mov si, test16x16InputStr
	call writeString
	mov byte [es:isTesting], 3

	mov cx, 0
testEqu16Loop:
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
	mov ax, cx
	cmp ax, cx
	jnz equ16Failed
	sub ax, cx
	jnz equ16Failed
	mov ax, cx
	xor ax, cx
	jnz equ16Failed
continueEqu16:
	inc cx
	jnz testEqu16Loop

	mov cx, 0
testNeq16Loop:
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
	mov ax, cx
	inc ax
	cmp ax, cx
	jz neq16Failed
	sub ax, cx
	jz neq16Failed
	mov ax, cx
	inc ax
	xor ax, cx
	jz neq16Failed
continueNeq16:
	inc cx
	jnz testNeq16Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
equ8Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueEqu8
	ret
neq8Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueNeq8
	ret
equ16Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueEqu16
	ret
neq16Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueNeq16
	ret

;-----------------------------------------------------------------------------
; Test ADD for all bytes & bytes values.
;-----------------------------------------------------------------------------
testAdd8:
	mov si, testingAdd8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov [es:expectedResult1], cx
testAdd8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcAdd8Result
	call testAdd8Single
	xor al, 0
	jnz stopAdd8Test
continueAdd8:
	inc word [es:expectedResult1]
	inc cl
	jnz testAdd8Loop
	mov word [es:expectedResult1], 0
	inc ch
	mov [es:expectedResult1], ch
	jnz testAdd8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopAdd8Test:
	call checkKeyInput
	xor al, 0
	jnz continueAdd8
	ret

;-----------------------------------------------------------------------------
testAdd8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	add bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz add8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz add8Failed

	pushf
	pop bx
	or bx, 0x78FF
	push bx
	mov [es:inputFlags], bx

	mov cl, [es:inputVal1]
	mov al, [es:inputVal2]
	popf
	add al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz add8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz add8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

add8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcAdd8Result:
	push bx
	push cx

	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	xor bl, al

	mov ax, [es:expectedResult1]
	xor bl, al
	mov cx, 0xF202
	test ah, 1
	jz add8NoC
	or cx, 0x801
add8NoC:
	test bl, 0x80
	jz add8NoOv
	xor ch, 0x08
add8NoOv:
	test bl, 0x10
	jz add8NoAC
	or cl, 0x10
add8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test ROL for all byte & 5bit values.
;-----------------------------------------------------------------------------
testRol8:
	mov si, testingRol8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRol8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRol8Result
	call testRol8Single
	xor al, 0
	jnz stopRol8Test
continueRol8:
	inc cx
	jnz testRol8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopRol8Test:
	call checkKeyInput
	xor al, 0
	jnz continueRol8
	ret

;-----------------------------------------------------------------------------
testRol8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	mov al, cl
	and al, 0x1F
	jnz rol8Normal
	test bl, 0x80
	jz rol8Normal
	or word [es:expectedFlags], 0x0800
rol8Normal:

	popf
	rol bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz rol8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rol8Failed

	mov cl, [es:inputVal1]
	mov al, cl
	and al, 0xE0
	pushf
	pop bx
	or bx, 0x78FF
	cmp al, 0x20
	jnz rol8NormalC2
	and bx, 0xFFFE
rol8NormalC2:
	cmp al, 0x30
	jnz rol8NormalV2
	and bx, 0xF7FF
rol8NormalV2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	or byte [es:expectedFlags], 0xD4
	mov ah, cl
	and ah, 0x1F
	jnz rol8Normal2
	and word [es:expectedFlags], 0xF7FF
	and bx, 0x0001
	jz rol8NormalC3
	or bx, 0x0800
rol8NormalC3:
	or [es:expectedFlags], bx
	test al, 0x80
	jz rol8Normal2
	xor word [es:expectedFlags], 0x0800
rol8Normal2:
	popf
	rol al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz rol8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rol8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

rol8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcRol8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	and bl, 0x1F
	jz rol8NoOv
rol8Loop:
	add al, al
	jnc rol8NoC
	or al, 0x01
rol8NoC:
	dec bl
	jnz rol8Loop

rol8SetRes:
	mov ah, al
	test ah, 0x01
	jz rol8NoCy
	or cl, 0x01
	xor ah, 0x80
rol8NoCy:
	test ah, 0x80
	jz rol8NoOv
	or ch, 0x08
rol8NoOv:
	mov [es:expectedResult1], al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test Decimal Adjust after Addition of all byte values & AC + CY.
;-----------------------------------------------------------------------------
testDaa:
	mov si, testingDaaStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testDaaLoop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcDaaResult
	call testDaaSingle
	xor al, 0
	jnz stopDaaTest
continueDaa:
	inc cx
	cmp cx, 0x400
	jnz testDaaLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopDaaTest:
	call checkKeyInput
	xor al, 0
	jnz continueDaa
	ret

;-----------------------------------------------------------------------------
testDaaSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bl, [es:inputVal2]
	test bl, 1
	jz daaTestNoCY
	or al, 0x01
daaTestNoCY:
	test bl, 2
	jz daaTestNoAC
	or al, 0x10
daaTestNoAC:
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov ah, al
	xor ah, 0xa5

	popf
	daa
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz daaFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz daaFailed

	pushf
	pop ax
	xor al, al
	or ax, 0x78EE
	mov bl, [es:inputVal2]
	test bl, 1
	jz daaTest2NoCY
	or al, 0x01
daaTest2NoCY:
	test bl, 2
	jz daaTest2NoAC
	or al, 0x10
daaTest2NoAC:
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov ah, al
	xor ah, 0xa5
	popf
	daa
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz daaFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz daaFailed

	xor ax, ax
	pop cx
	pop bx
	ret

daaFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcDaaResult:
	push bx
	push dx

	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	mov ah, al
	mov bh, al
	shl bh, 4

	cmp al, 0x9A
	jc daaNoCY
	or bl, 1
daaNoCY:
	test bl, 1
	jz daaNoHighAdd
	add al, 0x60
daaNoHighAdd:
	cmp bh, 0xA0
	jc daaNoAC
	or bl, 2
daaNoAC:
	test bl, 2
	jz daaNoLowAdd
	add al, 0x06
daaNoLowAdd:
daaSetRes:
	mov dx, 0xF202				; Expected flags
	test bl, 1
	jz daaSkipCY
	or dl, 0x01
daaSkipCY:
	test bl, 2
	jz daaSkipAC
	or dl, 0x10
daaSkipAC:
	cmp al, ah
	jno daaSkipOV
	or dx, 0x800
daaSkipOV:
	xor ah, 0xA5
	mov [es:expectedResult1], ax
	lea bx, PZSTable
	xlat				; Fetch Sign, Zero & Parity
	or dl, al
	mov [es:expectedFlags], dx
	pop dx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Wait for input, A continue, B cancel.
;-----------------------------------------------------------------------------
checkKeyInput:
	hlt
	in al, IO_KEYPAD
	test al, PAD_A | PAD_B
	jnz checkKeyInput		; Make sure no input is held before.
keyLoop:
	hlt
	in al, IO_KEYPAD
	test al, PAD_A
	jnz keyContinue
	test al, PAD_B
	jnz keyCancel
	jmp keyLoop
keyContinue:
	mov al, 1
	ret
keyCancel:
	xor al, al
	ret
;-----------------------------------------------------------------------------
; Gets the next number from LFSR1 in AX
;-----------------------------------------------------------------------------
getLFSR1Value:
	mov ax, [es:lfsr1]
	shr ax, 1
	jnc noTaps1
	xor ax, 0xD008
noTaps1:
	mov [es:lfsr1], ax
	ret
;-----------------------------------------------------------------------------
; Gets the next number from LFSR2 in AX
;-----------------------------------------------------------------------------
getLFSR2Value:
	mov ax, [es:lfsr2]
	shr ax, 1
	jnc noTaps2
	xor ax, 0xD008
noTaps2:
	mov [es:lfsr2], ax
	ret
;-----------------------------------------------------------------------------
; Print expected result and flags plus tested result and flags.
;-----------------------------------------------------------------------------
printFailedResult:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, inputStr
	call writeString

	mov ax, [es:inputVal2]
	call printHexW
	mov si, hexPrefixStr
	call writeString
	mov ax, [es:inputVal1]
	call printHexW
	mov si, fHexPrefixStr
	call writeString
	mov ax, [es:inputFlags]
	call printHexW
	mov al, 10
	int 0x10

	mov si, expectedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:expectedResult1]
	call printHexW
	mov si, flagsStr
	call writeString
	mov ax, [es:expectedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:expectedException]
	add al, '0'
	int 0x10

	mov si, testedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:testedResult1]
	call printHexW
	mov si, flagsStr
	call writeString
	mov ax, [es:testedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:testedException]
	add al, '0'
	int 0x10
	mov al, 10
	int 0x10

	ret

;-----------------------------------------------------------------------------
; Clear tilemap line.
;-----------------------------------------------------------------------------
clearLine:
	mov bl, [es:cursorYPos]
	and bx, 0x1f
	shl bx, 6		; ax * MAP_TWIDTH
	mov di, backgroundMap
	add di, bx
	mov cx, MAP_TWIDTH
	mov ax, BG_CHR( ' ', 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	rep stosw
	ret
;-----------------------------------------------------------------------------
; Clear foreground tilemap.
;-----------------------------------------------------------------------------
clearForegroundMap:
	mov di, foregroundMap
	jmp clearTileMap
;-----------------------------------------------------------------------------
; Clear background tilemap.
;-----------------------------------------------------------------------------
clearScreen:
	push cx
	mov di, backgroundMap
clearTileMap:
	; Clear a tilemap by writing space (0x20) to all locations.
	mov ax, BG_CHR( ' ', 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov cx, MAP_TWIDTH * MAP_THEIGHT
	rep stosw
	xor ax, ax
	mov [es:cursorPos], ax
	mov [es:bgPos], ax
	pop cx
	ret
;-----------------------------------------------------------------------------
; Write text to background. si = source
;-----------------------------------------------------------------------------
writeString:
	mov cx, SCREEN_TWIDTH * SCREEN_THEIGHT
textLoop:
	lodsb
	int 0x10
	xor al, 0
	jz endString
	dec cx
	jnz textLoop
endString:
	ret

;-----------------------------------------------------------------------------
printHexW:
	push ax
	mov al, ah
	call printHexB
	pop ax
;-----------------------------------------------------------------------------
printHexB:
	push ax
	shr al, 0x04
	call printNibble
	pop ax
	and al, 0x0f
printNibble:
	cmp al, 0x09
	jg .letter
	add al, '0'
	int 0x10
	ret
.letter:
	add al, 'a' - 0xa
	int 0x10
	ret
;-----------------------------------------------------------------------------
; Our vblank interrupt handler
; It is called automatically whenever the vblank interrupt occurs, 
; that is, every time the screen is fully drawn.
;-----------------------------------------------------------------------------
vblankInterruptHandler:
	push ax
	push bx
	push di

	; globalFrameCounter++
	mov ax, [es:globalFrameCounter]
	inc ax
	mov [es:globalFrameCounter], ax

	mov ax, [es:bgPos]
	out IO_SCR1_SCRL_X, ax
	mov ax, [es:fgPos]
	out IO_SCR2_SCRL_X, ax

	mov al, [es:isTesting]
	xor al, 0
	jz skipValuePrint
	cmp al, 1
	jnz skipValue8x8Print
	mov byte [es:cursorXPos], 17
	mov al, [es:inputVal2]
	call printHexB
	mov byte [es:cursorXPos], 23
	mov al, [es:inputVal1]
	call printHexB
	cmp byte [es:inputCarry], 0
	jz skipValuePrint
	mov byte [es:cursorXPos], 26
	mov al, 'C'
	int 0x10
	jmp skipValuePrint
skipValue8x8Print:
	cmp al, 2
	jnz skipValue16x8Print
	mov byte [es:cursorXPos], 17
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 25
	mov al, [es:inputVal1]
	call printHexB
	jmp skipValuePrint
skipValue16x8Print:
	cmp al, 3
	jnz skipValue8Print
	mov byte [es:cursorXPos], 15
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 23
	mov ax, [es:inputVal1]
	call printHexW
	jmp skipValuePrint
skipValue8Print:
	cmp al, 4
	jnz skipValuePrint
	mov byte [es:cursorXPos], 17
	mov al, [es:inputVal1]
	call printHexB
	jmp skipValuePrint
skipValuePrint:
acknowledgeVBlankInterrupt:
	mov al, INT_VBLANK_START
	out INT_CAUSE_CLEAR, al

	pop di
	pop bx
	pop ax
	iret

;-----------------------------------------------------------------------------
; The division error handler
; It is called if a division error occurs.
;-----------------------------------------------------------------------------
divisionErrorHandler:
	mov byte [es:testedException], 1
	iret
;-----------------------------------------------------------------------------
; The Int1 handler
; It is called on INT1 (Trap).
;-----------------------------------------------------------------------------
int1InstructionHandler:
	mov byte [es:testedException], 1
	iret
;-----------------------------------------------------------------------------
; The NMI handler
;-----------------------------------------------------------------------------
nmiHandler:
	mov byte [es:testedException], 2
	iret
;-----------------------------------------------------------------------------
; The Int3 handler
; It is called on INT3 (0xCC).
;-----------------------------------------------------------------------------
int3InstructionHandler:
	mov byte [es:testedException], 3
	iret
;-----------------------------------------------------------------------------
; The BRKV handler
; It is called on BRKV (0xCE).
;-----------------------------------------------------------------------------
overflowExceptionHandler:
	mov byte [es:testedException], 4
	iret
;-----------------------------------------------------------------------------
; The BOUND/CHKIND handler
; It is called on bounds exception for CHKIND (0x62).
;-----------------------------------------------------------------------------
boundsExceptionHandler:
	mov byte [es:testedException], 5
	iret

;-----------------------------------------------------------------------------
; The undefined instruction handler
; It is called if trying to execute an undefined instruction (not on V30MZ).
;-----------------------------------------------------------------------------
undefinedInstructionHandler:
	mov byte [es:testedException], 6
	iret

;-----------------------------------------------------------------------------
; The POLL exception handler
; It is called if POLL instruction gives an exception (not on V30MZ).
;-----------------------------------------------------------------------------
pollExceptionHandler:
	mov byte [es:testedException], 7
	iret
;-----------------------------------------------------------------------------
; Write a char to background. al = char
;-----------------------------------------------------------------------------
outputCharHandler:
	push bx
	push cx
	push di

	xor bh, bh
	mov bl, [es:cursorYPos]
	and bl, 0x1F
	shl bx, 5		; ax * MAP_TWIDTH
	mov cl, [es:cursorXPos]
	add bl, cl
	shl bx, 1
	mov di, backgroundMap
	add di, bx
	xor al, 0
	jz endOutput
	cmp al, 10
	jz newLine
	stosb
	inc di
	inc cl
	cmp cl, 28
	jnz endOutput
newLine:
	mov bl, [es:cursorYPos]
	inc bl
	mov al, bl
	sub al, SCREEN_THEIGHT-1
	jle notAtEnd
	and bl, 0x1F
	or bl, 0x40
	shl al, 3
	mov [es:bgYPos], al
notAtEnd:
	mov [es:cursorYPos], bl
	call clearLine
	xor cl, cl
endOutput:
	mov [es:cursorXPos], cl
	pop di
	pop cx
	pop bx
	iret

;-----------------------------------------------------------------------------
; Constants area
;-----------------------------------------------------------------------------

	align 2

PZSTable:
	db PSR_Z|PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0 ,PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S

FontTilePalette:
	dw 0xFFF, 0x000

MonoFont:
	db 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x18,0x08,0x10,0x00,0x18,0x00
	db 0x6C,0x6C,0x24,0x48,0x00,0x00,0x00,0x00,0x14,0x14,0xFE,0x28,0xFE,0x50,0x50,0x00
	db 0x10,0x7C,0x90,0x7C,0x12,0xFC,0x10,0x00,0x42,0xA4,0xA8,0x54,0x2A,0x4A,0x84,0x00
	db 0x30,0x48,0x38,0x62,0x94,0x88,0x76,0x00,0x18,0x18,0x08,0x10,0x00,0x00,0x00,0x00
	db 0x08,0x10,0x20,0x20,0x20,0x10,0x08,0x00,0x20,0x10,0x08,0x08,0x08,0x10,0x20,0x00
	db 0x10,0x92,0x54,0x38,0x38,0x54,0x92,0x00,0x10,0x10,0x10,0xFE,0x10,0x10,0x10,0x00
	db 0x00,0x00,0x00,0x30,0x30,0x10,0x20,0x00,0x00,0x00,0x00,0xFE,0x00,0x00,0x00,0x00
	db 0x00,0x00,0x00,0x00,0x00,0x60,0x60,0x00,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x00

	db 0x3C,0x42,0x46,0x5A,0x62,0x42,0x3C,0x00,0x08,0x38,0x08,0x08,0x08,0x08,0x08,0x00
	db 0x3C,0x42,0x42,0x0C,0x30,0x40,0x7E,0x00,0x3C,0x42,0x02,0x1C,0x02,0x42,0x3C,0x00
	db 0x0C,0x14,0x24,0x44,0x7E,0x04,0x04,0x00,0x7E,0x40,0x7C,0x02,0x02,0x42,0x3C,0x00
	db 0x3C,0x40,0x7C,0x42,0x42,0x42,0x3C,0x00,0x7E,0x02,0x04,0x08,0x08,0x10,0x10,0x00
	db 0x3C,0x42,0x42,0x3C,0x42,0x42,0x3C,0x00,0x3C,0x42,0x42,0x42,0x3E,0x02,0x3C,0x00
	db 0x00,0x18,0x18,0x00,0x18,0x18,0x00,0x00,0x00,0x18,0x18,0x00,0x18,0x08,0x10,0x00
	db 0x00,0x08,0x10,0x20,0x10,0x08,0x00,0x00,0x00,0x00,0x3C,0x00,0x3C,0x00,0x00,0x00
	db 0x00,0x10,0x08,0x04,0x08,0x10,0x00,0x00,0x3C,0x62,0x62,0x0C,0x18,0x00,0x18,0x00

	db 0x7C,0x82,0xBA,0xA2,0xBA,0x82,0x7C,0x00,0x10,0x28,0x28,0x44,0x7C,0x82,0x82,0x00
	db 0x7C,0x42,0x42,0x7C,0x42,0x42,0x7C,0x00,0x1C,0x22,0x40,0x40,0x40,0x22,0x1C,0x00
	db 0x78,0x44,0x42,0x42,0x42,0x44,0x78,0x00,0x7E,0x40,0x40,0x7E,0x40,0x40,0x7E,0x00
	db 0x7E,0x40,0x40,0x7C,0x40,0x40,0x40,0x00,0x3C,0x42,0x80,0x9E,0x82,0x46,0x3A,0x00
	db 0x42,0x42,0x42,0x7E,0x42,0x42,0x42,0x00,0x10,0x10,0x10,0x10,0x10,0x10,0x10,0x00
	db 0x02,0x02,0x02,0x02,0x42,0x42,0x3C,0x00,0x42,0x44,0x48,0x50,0x68,0x44,0x42,0x00
	db 0x40,0x40,0x40,0x40,0x40,0x40,0x7E,0x00,0x82,0xC6,0xAA,0x92,0x82,0x82,0x82,0x00
	db 0x42,0x62,0x52,0x4A,0x46,0x42,0x42,0x00,0x38,0x44,0x82,0x82,0x82,0x44,0x38,0x00

	db 0x7C,0x42,0x42,0x7C,0x40,0x40,0x40,0x00,0x38,0x44,0x82,0x82,0x8A,0x44,0x3A,0x00
	db 0x7C,0x42,0x42,0x7C,0x48,0x44,0x42,0x00,0x3C,0x42,0x40,0x3C,0x02,0x42,0x3C,0x00
	db 0xFE,0x10,0x10,0x10,0x10,0x10,0x10,0x00,0x42,0x42,0x42,0x42,0x42,0x42,0x3C,0x00
	db 0x82,0x82,0x44,0x44,0x28,0x28,0x10,0x00,0x82,0x92,0x92,0xAA,0xAA,0x44,0x44,0x00
	db 0x82,0x44,0x28,0x10,0x28,0x44,0x82,0x00,0x82,0x44,0x28,0x10,0x10,0x10,0x10,0x00
	db 0x7E,0x04,0x08,0x10,0x20,0x40,0x7E,0x00,0x18,0x10,0x10,0x10,0x10,0x10,0x18,0x00
	db 0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x00,0x18,0x08,0x08,0x08,0x08,0x08,0x18,0x00
	db 0x10,0x28,0x44,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xFE,0x00

	db 0x08,0x10,0x18,0x18,0x00,0x00,0x00,0x00,0x00,0x78,0x04,0x7C,0x84,0x84,0x7E,0x00
	db 0x40,0x40,0x7C,0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x3C,0x42,0x40,0x42,0x3C,0x00
	db 0x02,0x02,0x3E,0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x3C,0x42,0x7E,0x40,0x3E,0x00
	db 0x0C,0x10,0x3E,0x10,0x10,0x10,0x10,0x00,0x00,0x3C,0x42,0x42,0x3E,0x02,0x7C,0x00
	db 0x40,0x40,0x7C,0x42,0x42,0x42,0x42,0x00,0x18,0x18,0x00,0x08,0x08,0x08,0x08,0x00
	db 0x06,0x06,0x00,0x02,0x42,0x42,0x3C,0x00,0x20,0x20,0x26,0x28,0x30,0x28,0x26,0x00
	db 0x30,0x10,0x10,0x10,0x10,0x10,0x10,0x00,0x00,0x80,0xEC,0x92,0x92,0x92,0x92,0x00
	db 0x00,0x40,0x78,0x44,0x44,0x44,0x44,0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x3C,0x00

	db 0x00,0x3C,0x42,0x42,0x7C,0x40,0x40,0x00,0x00,0x78,0x84,0x84,0x7C,0x04,0x06,0x00
	db 0x00,0x00,0x5C,0x62,0x40,0x40,0x40,0x00,0x00,0x00,0x3E,0x40,0x3C,0x02,0x7C,0x00
	db 0x00,0x10,0x7C,0x10,0x10,0x10,0x0E,0x00,0x00,0x00,0x42,0x42,0x42,0x42,0x3F,0x00
	db 0x00,0x00,0x42,0x42,0x24,0x24,0x18,0x00,0x00,0x00,0x92,0x92,0x92,0x92,0x6C,0x00
	db 0x00,0x00,0x42,0x24,0x18,0x24,0x42,0x00,0x00,0x00,0x42,0x42,0x3E,0x02,0x7C,0x00
	db 0x00,0x00,0x7E,0x02,0x3C,0x40,0x7E,0x00,0x08,0x10,0x10,0x20,0x10,0x10,0x08,0x00
	db 0x10,0x10,0x10,0x00,0x10,0x10,0x10,0x00,0x20,0x10,0x10,0x08,0x10,0x10,0x20,0x00
	db 0x00,0x00,0x60,0x92,0x0C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00

prepareData:
	dw 0xF0AB, 0x570D

alphabet: db "ABCDEFGHIJKLMNOPQRSTUVWXYZ!", 10, 0
alphabet2: db "abcdefghijklmnopqrstuvwxyz.,", 10, 0

headLineStr: db " WonderSwan HW Test 20230129",10 , 0

menuTestAllStr: db "  Test All.",10 , 0
menuTestLogicStr: db "  Test Logic.",10 , 0
menuTestArithmeticStr: db "  Test Arithmetic.",10 , 0
menuTestRolShiftStr: db "  Test Rol & Shift.",10 , 0
menuTestMiscStr: db "  Test Misc.",10 , 0

testingEquStr: db "Equal by CMP, SUB & XOR", 10, 0

testingAdd8Str: db "ADD bytes", 10, 0

testingRol8Str: db "ROL byte by CL", 10, 0

testingDaaStr: db "DAA/ADJ4A", 10, 0

test8InputStr: db "Testing Input: 0x00", 0
test16InputStr: db "Testing Input: 0x0000", 0
test8x8InputStr: db "Testing Input: 0x00, 0x00", 0
test16x8InputStr: db "Testing Input: 0x0000, 0x00", 0
test16x16InputStr: db "Testing Inp: 0x0000, 0x0000", 0
inputStr: db "Input:0x", 0
expectedStr: db "Expected Result:", 10, 0
testedStr: db "Tested Result:", 10, 0
valueStr: db "Value:0x",0
flagsStr: db " Flags:0x",0
okStr: db "Ok!", 10, 0
failedStr: db "Failed!", 10, 0
preFlagStr: db "PreF: ", 0
postFlagStr: db "PostF: ", 0
hexPrefixStr: db " 0x",0
fHexPrefixStr: db " F:0x",0

author: db "Written by Fredrik Ahlström, 2023"

	ROM_HEADER initialize, MYSEGMENT, RH_WS_COLOR, RH_ROM_4MBITS, RH_NO_SRAM, RH_HORIZONTAL

SECTION .bss start=0x0100 ; Keep space for Int Vectors

globalFrameCounter: resw 1
bgPos:
bgXPos: resb 1
bgYPos: resb 1
fgPos:
fgXPos: resb 1
fgYPos: resb 1
cursorPos:
cursorXPos: resb 1
cursorYPos: resb 1
menuXPos: resb 1
menuYPos: resb 1
keysHeld: resb 1
keysDown: resb 1

lfsr1: resw 1
lfsr2: resw 1

inputVal1: resw 1
inputVal2: resw 1
inputVal3: resw 1
inputFlags: resw 1
inputCarry: resw 1

testedResult1: resw 1
testedResult2: resw 1
testedFlags: resw 1
testedException: resw 1		; If a (division) exception occurred.

expectedResult1: resw 1
expectedResult2: resw 1
expectedFlags: resw 1
expectedException: resw 1

boundLow: resw 1
boundHigh: resw 1

isTesting: resb 1			; If currently running test.
dummy: resb 1

selfModifyingCode: resb 8
