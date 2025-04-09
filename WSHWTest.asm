;-----------------------------------------------------------------------------
;
;  WonderSwan Hardware Test
;         by Fredrik Ahlström, 2023-2025
;         https://github.com/FluBBaOfWard/WSHWTest
;
;  UP/DOWN    - Choose option
;  A          - Start
;
;  Assemble with: 
;                   nasm -f bin -o WSHWTest.wsc WSHWTest.asm
;
;-----------------------------------------------------------------------------

	ORG 0x40000
	CPU 186
	BITS 16

	%include "WonderSwan.inc"

	CODESEGMENT equ 0x4000
	DATASEGMENT equ 0xf000
	foregroundMap equ WS_TILE_BANK - MAP_SIZE
	backgroundMap equ foregroundMap - MAP_SIZE
	spriteTable equ backgroundMap - SPR_TABLE_SIZE
	soundTable equ spriteTable - 0x40

	PSR_S equ 0x80
	PSR_Z equ 0x40
	PSR_P equ 0x04

	MENU_ENTRIES equ 10

SECTION .text start=0x40000
;-----------------------------------------------------------------------------
debugInit:				; this should be called when keypad row 0 is always 1
;-----------------------------------------------------------------------------
	mov al, 1
	mov [ss:bootMode], al
	jmp initialize
	times	(16)-$+$$ db 0x00
;-----------------------------------------------------------------------------
pcv2Init:				; this should be called when keypad row 1 is always 1
;-----------------------------------------------------------------------------
	mov al, 2
	mov [ss:bootMode], al
;	jmp initialize
;-----------------------------------------------------------------------------
initialize:
	xor ax, ax
	mov ss, ax			; Set SS segment to 0x0000 (RAM).
	mov [ss:startRegSP], sp
	mov sp, 0x2000
	pushf
	cli
	cld
	pop ax
	mov [ss:startRegF], ax
;	mov [ss:startRegAW], ax
	mov [ss:startRegCW], cx
	mov [ss:startRegDW], dx
	mov [ss:startRegBW], bx
;	mov [ss:startRegSP], sp
	mov [ss:startRegBP], bp
	mov [ss:startRegIX], si
	mov [ss:startRegIY], di
	mov ax, es
	mov [ss:startRegDS1], ax
	mov ax, cs
	mov [ss:startRegPS], ax
;	mov ax, ss
;	mov [ss:startRegSS], ax
	mov ax, ds
	mov [ss:startRegDS0], ax

; Dump IO Regs
	mov cx, 0xC0
	mov dx, 0
	mov bp, 0
.b0:
	in al, dx
	mov [ss:startIORegs,bp], al
	add dx, 1
	add bp, 1
	loop .b0

	in al, SYSTEM_CTRL1
	test al, 0x02			; Color model?
	jnz notPCV2
	in al, IO_SND_OUT_CTRL
	test al, 0x80			; Headphones connected?
	jz notPCV2
	mov al, [ss:bootMode]
	mov [ss:pcv2Mode], al
notPCV2:
;-----------------------------------------------------------------------------
; Initialize registers and RAM
;-----------------------------------------------------------------------------
	mov ax, DATASEGMENT
	mov ds, ax
	xor ax, ax
	mov es, ax			; Set ES segment to 0x0000 (RAM).

	; Setup stack
	mov bp, ax
	mov ss, ax
	mov sp, WS_STACK

	; Clear Ram
	mov di, globalFrameCounter
	mov cx, 0x1800
	rep stosw

	out IO_SRAM_BANK,al

;-----------------------------------------------------------------------------
; Initialize variables
;-----------------------------------------------------------------------------
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

	; Init Sound
	mov al,WAVE_RAM(soundTable)
	out IO_WAVE_RAM, al

;-----------------------------------------------------------------------------
; Register our interrupt handlers
;-----------------------------------------------------------------------------
	mov di, 0*4		; Division error vector
	mov word [es:di], divisionErrorHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 1*4		; Int1
	mov word [es:di], int1InstructionHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 2*4		; NMI
	mov word [es:di], nmiHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 3*4		; Int3
	mov word [es:di], int3InstructionHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 4*4		; BRKV
	mov word [es:di], overflowExceptionHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 5*4		; CHKIND
	mov word [es:di], boundsExceptionHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 6*4		; Undefined instruction vector
	mov word [es:di], undefinedInstructionHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 7*4		; POLL
	mov word [es:di], pollExceptionHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, 0x10*4	; output char vector
	mov word [es:di], outputCharHandler
	mov word [es:di + 2], CODESEGMENT

	mov ax, INT_BASE	; 0x20
	out IO_INT_VECTOR, al

	mov di, INTVEC_HBLANK_TIMER
	add di, ax
	shl di, 2
	mov word [es:di], hblankTimerHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, INTVEC_VBLANK_START
	add di, ax
	shl di, 2
;	mov word [es:di], vblankHandler
	mov word [es:di], vblankInterruptHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, INTVEC_VBLANK_TIMER
	add di, ax
	shl di, 2
	mov word [es:di], vblankTimerHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, INTVEC_DRAWING_LINE
	add di, ax
	shl di, 2
	mov word [es:di], lineCompareHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, INTVEC_SERIAL_RECEIVE
	add di, ax
	shl di, 2
	mov word [es:di], serialReceiveHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, INTVEC_RTC_ALARM
	add di, ax
	shl di, 2
	mov word [es:di], cartridgeIrqHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, INTVEC_KEY_PRESS
	add di, ax
	shl di, 2
	mov word [es:di], keyPressHandler
	mov word [es:di + 2], CODESEGMENT

	mov di, INTVEC_SERIAL_SEND
	add di, ax
	shl di, 2
	mov word [es:di], serialTransmitHandler
	mov word [es:di + 2], CODESEGMENT

	; Clear HBL & Timers
	xor ax, ax
	out IOw_H_BLANK_TIMER, ax
	out IO_TIMER_CTRL, al

	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al

	; Acknowledge all interrupts
	mov al, 0xFF
	out INT_CAUSE_CLEAR, al

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
	loop monoFontLoop

;-----------------------------------------------------------------------------
; Copy font palette into WSC's palette area
;-----------------------------------------------------------------------------

	; Copy 2-colour (2 bytes per colour) font palette to 
	; beginning of palettes area (becoming palette 0)
	mov si, FontTilePalette
	mov di, WSC_PALETTES
	mov cx, 2
	rep movsw

	mov ax, 0x7f0
	out IO_LCD_GRAY_01, ax
	mov ax, 0x0010
	out IOw_SCR_LUT_0, ax
	mov ax, 0x0020
	out IOw_SCR_LUT_1, ax
	mov ax, 0x0000
	out IOw_SCR_LUT_2, ax
	mov ax, 0x0000
	out IOw_SCR_LUT_4, ax

	; Copy sound to RAM
	mov si, SoundSamples
	mov di, soundTable
	mov cx, 0x20
	rep movsw
;-----------------------------------------------------------------------------
; Make background map point to our tiles, essentially "painting" the
; background layer with our tiles, coloured as per our palettes
;-----------------------------------------------------------------------------
main:
	call clearScreen
	call clearForegroundMap

	mov si, headLineStr
	call writeString

	mov si, menuShowRegistersStr
	call writeString
	mov si, menuTestAllStr
	call writeString
	mov si, menuTestInterruptStr
	call writeString
	mov si, menuTestTimersStr
	call writeString
	mov si, menuTestIORegsStr
	call writeString
	mov si, menuTestWindowsStr
	call writeString
	mov si, menuTestSoundStr
	call writeString
	mov si, menuTestSweepStr
	call writeString
	mov si, menuTestNoiseStr
	call writeString
	mov si, menuLcdOffStr
	call writeString
	mov si, menuPowerOffStr
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

	mov bx, [es:keysDown]

	; Check player input
;	test bl, (PAD_RIGHT<<4)
;	jnz speed_up

;	test bl, (PAD_LEFT<<4)
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
	cmp cl, MENU_ENTRIES
	js dontMoveDown
	mov cl, MENU_ENTRIES
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
	jz showStartRegs
	cmp cl, 1
	jz testAll
	cmp cl, 2
	jz testInterrupt
	cmp cl, 3
	jz testTimers
	cmp cl, 4
	jz testIORegs
	cmp cl, 5
	jz testWindows
	cmp cl, 6
	jz testSound
	cmp cl, 7
	jz testSoundSwp
	cmp cl, 8
	jz testNoise
	cmp cl, 9
	jz turnOffLCD
	cmp cl, 10
	jz powerOffWS
	; No input, restart main loop
	jmp mainLoop
;-----------------------------------------------------------------------------
;
; END main area
;
;-----------------------------------------------------------------------------
showStartRegs:
	call writeStartRegs

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testAll:
	call testIrq
	call testHBlankTimer
	call testVBlankTimer

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testInterrupt:
	call testIrq

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testTimers:
	call testHBlankTimer
	call testVBlankTimer

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testIORegs:
	call testIORegisters

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
testWindows:
	call testHorizontalWindows

	jmp main

;-----------------------------------------------------------------------------
testSound:
	call testSoundMixer

	jmp main

;-----------------------------------------------------------------------------
testSoundSwp:
	call testSoundSweep

	jmp main

;-----------------------------------------------------------------------------
testNoise:
	call testNoiseValues

	call checkKeyInput
	jmp main

;-----------------------------------------------------------------------------
powerOffWS:
	mov si, testingPowerOffStr
	call writeString
	mov al, 1
	out SYSTEM_CTRL3, al
off:
	hlt
	nop
	call checkKeyInput
	jmp main
;-----------------------------------------------------------------------------
turnOffLCD:
	mov si, testingLcdOffStr
	call writeString
	in al, IO_LCD_SEG_DATA
	or al, LCD_ICON_SLEEP
	out IO_LCD_SEG_DATA, al
	in al,IO_LCD_IF_CTRL
	and al, 0xFE		; LCD off
	out IO_LCD_IF_CTRL, al

	cli
	mov al, [ss:pcv2Mode]
	cmp al, 2
	jnz .f0
	mov al, KEYPAD_READ_ARROWS_V	; This is buttons on PCV2.
	jmp .f1
.f0:
	mov al, KEYPAD_READ_BUTTONS
.f1:
	out IO_KEYPAD, al
	; Enable only Joy interrupt
	mov al, INT_KEY_PRESS
	out IO_INT_ENABLE, al
	mov cx, 4						; Wait 4 frames to settle key bounces.
.kWait0:
	in al, IO_LCD_LINE
	cmp al, 0x91
	jz .kWait0
.kWait1:
	in al, IO_LCD_LINE
	cmp al, 0x91
	jnz .kWait1
	loop .kWait0

	; Acknowledge all interrupts
	mov al, 0xFF
	out INT_CAUSE_CLEAR, al

	sti
	hlt					; Wait for key press INT

	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al
	; Acknowledge all interrupts
	mov al, 0xFF
	out INT_CAUSE_CLEAR, al

	in al,IO_LCD_IF_CTRL
	or al, LCD_ON
	out IO_LCD_IF_CTRL, al
	in al,IO_LCD_SEG_DATA
	and al, 0xFE		; Sleep icon off
	out IO_LCD_SEG_DATA, al

	xor ax, ax
	mov si, testingKeyIrqLine
	mov al, [es:keyIrqLine]
	call writeStartReg

	hlt
	call checkKeyInput
	jmp main
;-----------------------------------------------------------------------------
writeStartReg:		; SI string, AX value
;-----------------------------------------------------------------------------
	push ax
	call writeString
	pop ax
	call printHexW
	mov al, 10
	int 0x10
	ret
;-----------------------------------------------------------------------------
writeStartRegs:
;-----------------------------------------------------------------------------
	mov si, startFStr
	mov ax, [es:startRegF]
	call writeStartReg

	mov si, startAWStr
	mov ax, [es:startRegAW]
	call writeStartReg

	mov si, startCWStr
	mov ax, [es:startRegCW]
	call writeStartReg

	mov si, startDWStr
	mov ax, [es:startRegDW]
	call writeStartReg

	mov si, startBWStr
	mov ax, [es:startRegBW]
	call writeStartReg

	mov si, startBPStr
	mov ax, [es:startRegBP]
	call writeStartReg

	mov si, startSPStr
	mov ax, [es:startRegSP]
	call writeStartReg

	mov si, startIXStr
	mov ax, [es:startRegIX]
	call writeStartReg

	mov si, startIYStr
	mov ax, [es:startRegIY]
	call writeStartReg

	mov si, startDS1Str
	mov ax, [es:startRegDS1]
	call writeStartReg

	mov si, startPSStr
	mov ax, [es:startRegPS]
	call writeStartReg

	mov si, startSSStr
	mov ax, [es:startRegSS]
	call writeStartReg

	mov si, startDS0Str
	mov ax, [es:startRegDS0]
	call writeStartReg

	call checkKeyInput

	mov al, 10
	int 0x10
	mov al, 10
	int 0x10

	mov dx, 0x80
	mov bp, 0
.b1:
	mov ax, bp
	call printHexB
	mov al, ':'
	int 0x10
	mov cx, 8
.b0:
	mov al, ' '
	int 0x10
	mov al, [ss:startIORegs,bp]
	call printHexB
	add bp, 1
	loop .b0
	mov al, 10
	int 0x10
	sub dx, 8
	jnz .b1

	cmp bp, 0xC0
	jz .end
	call checkKeyInput
	mov dx, 0x40
	jmp .b1
.end:
	ret

;-----------------------------------------------------------------------------
; Test Interrupt Manager.
;-----------------------------------------------------------------------------
testIrq:
	mov si, testingIrqStr
	call writeString

	mov si, testingIrq0Str
	call writeString

	mov byte [es:isTesting], 1

	cli

	; Disable all interrupts
	xor al, al
	out IO_INT_ENABLE, al

	; Acknowledge all interrupts
	dec al
	out INT_CAUSE_CLEAR, al

	; Setup HBL/VBL Timers
	mov ax, 1
	out IOw_H_BLANK_TIMER, ax
	out IOw_V_BLANK_TIMER, ax
	mov al, 0x0F
	out IO_TIMER_CTRL, al

	mov al, 0x40
	out IO_LCD_INTERRUPT, al

	; Enable serial
	mov al, COMM_ENABLE | COMM_SPEED_38400
	out IO_SERIAL_STATUS, al

	mov bl,40
	call waitLine
	mov bl,20
	call waitLine

	in al, IO_INT_CAUSE
	xor al, 0
	mov si, failedStr
	jnz int0Fail
	mov si, okStr
int0Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq1Str
	call writeString

	; Enable all interrupts
	mov al, 0xFF
	out IO_INT_ENABLE, al

	mov bl,40
	call waitLine
	mov bl,20
	call waitLine

	in al, IO_INT_CAUSE
	xor al, 0
	mov si, failedStr
	jz int1Fail
	mov si, okStr
int1Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq2Str
	call writeString

	; Disable all interrupts
	xor al, al
	out IO_INT_ENABLE, al

	mov bl,40
	call waitLine
	mov bl,20
	call waitLine

	in al, IO_INT_CAUSE
	xor al, 0
	mov si, failedStr
	jz int2Fail
	mov si, okStr
int2Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq3Str
	call writeString

	; Acknowledge all interrupts
	mov al, 0xFF
	out INT_CAUSE_CLEAR, al

	mov bl,40
	call waitLine
	mov bl,20
	call waitLine

	in al, IO_INT_CAUSE
	xor al, 0
	mov si, failedStr
	jnz int3Fail
	mov si, okStr
int3Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq4Str
	call writeString

	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al

	mov bl,80
	call waitLine
	mov bl,60
	call waitLine

	mov word [es:vblankIrqCount], 0

	sti
	nop
	cli

	mov al, [es:vblankIrqCount]
	xor al, 0
	mov si, failedStr
	jz int4Fail
	mov si, okStr
int4Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq5Str
	call writeString

	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al

	mov bl,40
	call waitLine
	mov bl,20
	call waitLine

	; Disable all interrupts
	xor al, al
	out IO_INT_ENABLE, al

	mov word [es:vblankIrqCount], 0

	sti
	nop
	cli

	mov al, [es:vblankIrqCount]
	xor al, 0
	mov si, failedStr
	jz int5Fail
	mov si, okStr
int5Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq6Str
	call writeString

	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al

	mov bl,40
	call waitLine
	mov bl,20
	call waitLine

	; Acknowledge VBL interrupt
	mov al, INT_VBLANK_START
	out INT_CAUSE_CLEAR, al

	mov word [es:vblankIrqCount], 0

	sti
	nop
	cli

	mov al, [es:vblankIrqCount]
	xor al, 0
	mov si, failedStr
	jnz int6Fail
	mov si, okStr
int6Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq7Str
	call writeString

	; Enable HBL interrupt
	mov al, INT_HBLANK_TIMER 
	out IO_INT_ENABLE, al

	; Acknowledge all interrupts
	mov al, 0xFF
	out INT_CAUSE_CLEAR, al

	mov bl,40
	call waitLine
	mov bl,20
	call waitLine

	mov word [es:hblankTimerIrqCount], 0

	sti
	nop
	nop
	cli

	mov al, [es:hblankTimerIrqCount]
	cmp al, 2
	mov si, failedStr
	jnz int7Fail
	mov si, okStr
int7Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingIrq8Str
	call writeString

	mov bl,20
	call waitLine

	; Enable serial
	mov al, COMM_ENABLE | COMM_SPEED_38400 | COMM_OVERRUN_RESET
	out IO_SERIAL_STATUS, al

	; Only enable serial receive interrupt
	mov al, INT_SERIAL_RECEIVE
	out IO_INT_ENABLE, al

	; Acknowledge all interrupts
	mov al, 0xFF
	out INT_CAUSE_CLEAR, al

	mov bl,22
	call waitLine

	in al, IO_INT_CAUSE
	xor al, 0
	mov si, failedStr
	jnz int8Fail
	mov si, okStr
int8Fail:
	call writeString
	sti
;-----------------------------------------------------------------------------
	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al
	mov byte [es:isTesting], 0
	xor ax, ax
	ret

;-----------------------------------------------------------------------------
; Test Timers
;-----------------------------------------------------------------------------
testHBlankTimer:
	mov si, testingTimerStr
	call writeString

	mov si, testingTimer0Str
	call writeString

	mov byte [es:isTesting], 1

	cli

	; Disable all interrupts
	xor al, al
	out IO_INT_ENABLE, al

	; Acknowledge all interrupts
	dec al
	out INT_CAUSE_CLEAR, al

	mov bl, 20
	call waitLine

	; Setup HBL Timer
	mov ax, 0
	out IOw_H_BLANK_TIMER, ax
	mov al, 0x03
	out IO_TIMER_CTRL, al

	mov bl, 80
	call waitLine

	in ax, IOw_H_BLANK_COUNTER
	xor ax, 0
	mov si, failedStr
	jnz timer0Fail
	mov si, okStr
timer0Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingTimer1Str
	call writeString

	mov bl, 20
	call waitLine

	; Setup HBL Timer
	mov al, 0x00
	out IO_TIMER_CTRL, al
	mov ax, 300
	out IOw_H_BLANK_TIMER, ax

	mov bl, 80
	call waitLine

	in ax, IOw_H_BLANK_COUNTER
	cmp ax, 300
	mov si, failedStr
	jnz timer1Fail
	mov si, okStr
timer1Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingTimer2Str
	call writeString

	mov bl, 20
	call waitLine

	; Setup HBL Timer
	mov ax, 300
	out IOw_H_BLANK_TIMER, ax
	mov al, 0x03
	out IO_TIMER_CTRL, al

	mov bl, 80
	call waitLine

	in ax, IOw_H_BLANK_COUNTER
	cmp ax, 241
	mov si, failedStr
	jge timer2Fail
	mov si, okStr
timer2Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingTimer3Str
	call writeString

	mov bl, 20
	call waitLine

	; Setup HBL Timer
	mov ax, 300
	out IOw_H_BLANK_TIMER, ax
	mov al, 0x01
	out IO_TIMER_CTRL, al

	mov bl, 80
	call waitLine

	in ax, IOw_H_BLANK_COUNTER
	cmp ax, 241
	mov si, failedStr
	jge timer3Fail
	mov si, okStr
timer3Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingTimer4Str
	call writeString

	mov bl, 20
	call waitLine

	; Setup HBL Timer
	mov ax, 300
	out IOw_H_BLANK_TIMER, ax
	mov al, 0x02
	out IO_TIMER_CTRL, al

	mov bl, 80
	call waitLine

	in ax, IOw_H_BLANK_COUNTER
	cmp ax, 300
	mov si, failedStr
	jnz timer4Fail
	mov si, okStr
timer4Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingTimer5Str
	call writeString

	mov bl, 20
	call waitLine

	; Setup HBL Timer
	mov ax, 300
	out IOw_H_BLANK_TIMER, ax
	mov al, 0x01
	out IO_TIMER_CTRL, al

	mov bl, 40
	call waitLine

	; Turn off timer
	xor al, al
	out IO_TIMER_CTRL, al

	mov bl, 60
	call waitLine

	; Turn it on again
	mov al, 0x01
	out IO_TIMER_CTRL, al

	mov bl, 100
	call waitLine

	in ax, IOw_H_BLANK_COUNTER
	cmp ax, 239
	mov si, failedStr
	jl timer5Fail
	mov si, okStr
timer5Fail:
	call writeString

;-----------------------------------------------------------------------------
	mov si, testingTimer6Str
	call writeString

	; Turn off timer
	xor al, al
	out IO_TIMER_CTRL, al

	; Acknowledge HBl interrupt
	mov al, INT_HBLANK_TIMER
	out INT_CAUSE_CLEAR, al

	; Enable HBl interrupt
	mov al, INT_HBLANK_TIMER
	out IO_INT_ENABLE, al

	mov bl, 20
	call waitLine

	; Setup HBL Timer
	mov ax, 1
	out IOw_H_BLANK_TIMER, ax

	mov bl, 22
	call waitLine


	in al, IO_INT_CAUSE
	xor al, INT_HBLANK_TIMER
	mov si, failedStr
	jnz timer6Fail
	mov si, okStr
timer6Fail:
	call writeString
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al
	sti
	ret
;-----------------------------------------------------------------------------
testVBlankTimer:

	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al

	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	xor ax, ax
	ret

;-----------------------------------------------------------------------------
; Test IO Regs.
;-----------------------------------------------------------------------------
testIORegisters:
	mov si, testingIORegsStr
	call writeString

	mov byte [es:isTesting], 1
	in al, SYSTEM_CTRL1
	test al, WS_COLOR
	jz ioNoColor
	in al, SYSTEM_CTRL3
	test al, 0x80
	jz ioColor
	jmp ioCrystal

;-----------------------------------------------------------------------------
ioNoColor:
	mov si, ioRegMasksASWAN
	call runIORegTest
	jmp endIOTest

;-----------------------------------------------------------------------------
ioColor:
	mov si, ioRegMasksSPHINXMono
	call runIORegTest

	mov si, testingIORegsColStr
	call writeString
	call setColorMode

	mov si, ioRegMasksSPHINX
	call runIORegTest
	jmp endIOColorTest

;-----------------------------------------------------------------------------
ioCrystal:
	mov si, ioRegMasksSPHINX2Mono
	call runIORegTest

	mov si, testingIORegsColStr
	call writeString
	call setColorMode

	mov si, ioRegMasksSPHINX2
	call runIORegTest
endIOColorTest:
	in al, SYSTEM_CTRL2
	and al, ~VMODE_COLOR
	out SYSTEM_CTRL2, al
endIOTest:
	mov si, doneStr
	call writeString
	mov byte [es:isTesting], 0
	xor ax, ax
	ret

;-----------------------------------------------------------------------------
runIORegTest:
	mov cx, 0xB0
	mov dx, 0
IOTestLoop:
	mov bp, dx
	add bp, dx
	mov bx, [ds:si,bp]
	cmp bx, 0xffff
	jz skipIOTest
	call testIORegWithMask
	xor al, 0
	jz skipIOTest
	call breakIOTest
skipIOTest:
	inc dl
	loop IOTestLoop

;-----------------------------------------------------------------------------
; Special regs

	mov dx, IO_SND_OUT_CTRL	; 0x91
	mov bx, 0x7f0f			; Ignore top bit result.
	call testIORegWithMask2

	mov al, NOISE_RESET
	out IO_SND_NOISE_CTRL, al	; Clear LFSR
	mov dx, IOw_SND_RANDOM	; 0x92
	xor bx, bx				; Result should be zero.
	call testIORegWithMask
	mov dx, IOw_SND_RANDOM+1	; 0x93
	xor bx, bx				; Result should be zero.
	call testIORegWithMask

	mov dx, SYSTEM_CTRL1	; 0xA0
	in al, dx
	mov bp, ax				; Save old IO value
	and al, 0x4				; Keep 8/16-bit bus
	out dx, al
	nop
	in al, dx
	mov bx, bp
	and bl, 0x87			; These bits can not/should not be cleared once set.
	xor al, bl
	jz ioTest1Ok
	call ioRegFail
ioTest1Ok:
	mov ax, bp
	or al, 0xfe				; Don't lock boot rom if not set.
	out dx, al
	nop
	in al, dx
	mov bx, bp
	or bl, 0x0c
	xor al, bl
	jz ioTest2Ok
	call ioRegFail
ioTest2Ok:
	mov ax, bp		; Restore old IO value
	out dx, al

	; Disable all interrupt
	xor al, al
	out IO_INT_ENABLE, al
	dec al
	out INT_CAUSE_CLEAR, al
	mov dx, IO_INT_VECTOR	; 0xB0
	mov bx, 0x00f8
	call testIORegWithMask
	; Enable VBL interrupt
	mov al, INT_VBLANK_START
	out IO_INT_ENABLE, al

	mov dx, IO_SERIAL_DATA	; 0xB1
	in al, dx
	mov bp, ax				; Save old IO value
	xor al, 0xff			; Invert in value
	out dx, al
	nop
	in al, dx
	mov bx, bp
	xor al, bl
	jz ioTest3Ok
	call ioRegFail
ioTest3Ok:

	mov dx, IO_INT_ENABLE	; 0xB2
	mov bx, 0x00ff
	call testIORegWithMask

	mov dx, IO_SERIAL_STATUS	; 0xB3
	mov bx, 0x00c4
	call testIORegWithMask

	mov dx, IO_INT_CAUSE	; 0xB4
	in al, dx
	mov bp, ax				; Save old IO value
	xor al, 0xff			; Invert in value
	out dx, al
	nop
	in al, dx
	mov bx, bp
	xor al, bl
	jz ioTest4Ok
	call ioRegFail
ioTest4Ok:

	ret
;-----------------------------------------------------------------------------
breakIOTest:
	push si
	xor al, 0
	mov si, failedStr
	jnz io0Fail
	mov si, okStr
io0Fail:
;	call writeString
	call checkKeyInput
	pop si
	ret
;-----------------------------------------------------------------------------
setColorMode:
	in al, SYSTEM_CTRL2
	or al, VMODE_COLOR
	out SYSTEM_CTRL2, al
	hlt					; Wait for VBlank to latch color mode.
	ret
;-----------------------------------------------------------------------------
testIORegWithMask:	; bl = test value mask, bh = always set bits, dx = io port.
;-----------------------------------------------------------------------------
	in al, dx
	mov bp, ax		; Save old IO value
	xor ah, ah
	mov al, ah
	out dx, al
	nop
	in al, dx
	or ah, bh
	xor al, ah
	jnz ioRegFail
	mov ah, 0xff
	mov al, ah
	out dx, al
	nop
	in al, dx
	and ah, bl
	xor al, ah
	jnz ioRegFail
	mov ax, bp		; Restore old IO value
	out dx, al
	xor al, al
	ret
;-----------------------------------------------------------------------------
testIORegWithMask2:	; bl = test value mask, bh = io value mask, dx = io port.
;-----------------------------------------------------------------------------
	in al, dx
	mov bp, ax		; Save old IO value
	xor ah, ah
	mov al, ah
	out dx, al
	nop
	in al, dx
	and al, bh
	xor al, ah
	jnz ioRegFail
	mov ah, 0xff
	mov al, ah
	out dx, al
	nop
	in al, dx
	and al, bh
	and ah, bl
	xor al, ah
	jnz ioRegFail
	mov ax, bp		; Restore old IO value
	out dx, al
	xor al, al
	ret
;-----------------------------------------------------------------------------
ioRegFail:
	push si
	push ax
	mov ax, bp		; Restore old IO value
	out dx, al
	mov ax, dx
	call printHexB
	mov al, ':'
	int 0x10
	pop ax
	call printHexB
	mov al, ':'
	int 0x10
	mov si, failedStr
	call writeString
	pop si
	mov al, 1
	ret
;-----------------------------------------------------------------------------
testHorizontalWindows:

	; Turn on display
	mov al, BG_ON | FG_ON | FG_WIN_ON
	out IO_DISPLAY_CTRL, al

	mov di, backgroundMap + ((4 * MAP_TWIDTH) + 13) * 2
	mov ax, BG_CHR( 0x7F, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	stosw

	mov di, backgroundMap + ((5 * MAP_TWIDTH) + 12) * 2
	mov ax, BG_CHR( 0x7F, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	stosw
	stosw
	stosw

	mov di, foregroundMap + ((3 * MAP_TWIDTH) + 12) * 2
	mov ax, BG_CHR( 0x7F, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	stosw
	stosw
	stosw

	mov di, foregroundMap + ((4 * MAP_TWIDTH) + 12) * 2
	mov ax, BG_CHR( 0x7F, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	mov ax, BG_CHR( 0x7F, 1, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	stosw
	mov ax, BG_CHR( 0x7F, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw

	mov di, foregroundMap + ((5 * MAP_TWIDTH) + 12) * 2
	mov ax, BG_CHR( 0x7F, 2, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	mov ax, BG_CHR( 0x7F, 1, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	stosw
	mov ax, BG_CHR( 0x7F, 2, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw

	mov di, foregroundMap + ((6 * MAP_TWIDTH) + 12) * 2
	mov ax, BG_CHR( 0x7F, 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	stosw
	stosw
	stosw
	stosw


winTestLoop:
	mov ax, 0x4428
	out IO_SCR2_WIN_X1, ax
	mov ax, 0x47A7
	out IO_SCR2_WIN_X2, ax

.waitLine31:
	in al, IO_LCD_LINE
	cmp al, 31
	jne .waitLine31

	mov ax, 0x1068
	out IO_SCR2_WIN_X1, ax
	mov ax, 0x4777
	out IO_SCR2_WIN_X2, ax

.waitLine39:
	in al, IO_LCD_LINE
	cmp al, 39
	jne .waitLine39

	mov al, 0x60
	out IO_SCR2_WIN_X1, al
	mov al, 0x7F
	out IO_SCR2_WIN_X2, al

.waitLine47:
	in al, IO_LCD_LINE
	cmp al, 47
	jne .waitLine47

	mov ax, 0x3868
	out IO_SCR2_WIN_X1, ax
	mov ax, 0x2F77
	out IO_SCR2_WIN_X2, ax

	hlt
	mov al, [es:keysDown]
	test al, PAD_A | PAD_B
	jz winTestLoop

	mov byte [es:isTesting], 0
	xor ax, ax
	ret

;-----------------------------------------------------------------------------
; Test sound output/mixer.
;-----------------------------------------------------------------------------
testSoundMixer:
	mov si, testingSoundStr
	call writeString

	mov si, testingSound0Str
	call writeString

	mov si, testingSound1Str
	call writeString

	in al, IO_SND_OUT_CTRL
	test al, 0x80				;Headphones?
	jz noHeadPhones
	mov si, testingSound2Str
	call writeString
noHeadPhones:

	mov ax, 0x800-0x60			;2kHz?
	out IOw_SND_FREQ_1, ax
	mov ax, 0x800-0xC0			;1kHz?
	out IOw_SND_FREQ_2, ax
	mov ax, 0x800-0x140			;500Hz?
	out IOw_SND_FREQ_3, ax
	mov ax, 0x800-0x280			;250Hz?
	out IOw_SND_FREQ_4, ax

	mov al, 0xFF
	out IO_SND_VOL_1, al
	out IO_SND_VOL_2, al
	out IO_SND_VOL_3, al
	out IO_SND_VOL_4, al

	xor ah,ah
	mov bl, 7
	mov cl, SND_1_ON | SND_2_ON | SND_3_ON | SND_4_ON
soundLoop:
	test al, (PAD_LEFT<<4)
	jz noSndAdd
	sub bl, 2
	jns noSndAdd
	mov bl, 0
noSndAdd:
	test al, (PAD_RIGHT<<4)
	jz noSndSub
	add bl, 2
	cmp bl, 6
	jc noSndSub
	mov bl, 6
noSndSub:
	xor cl, ah				; Y1-Y4 to turn on/off Ch
	mov al, cl
	out IO_SND_CH_CTRL, al

	mov al, 0x1
	or al, bl
	out IO_SND_OUT_CTRL, al

	hlt
	mov ax, [es:keysDown]
	test al, PAD_A | PAD_B
	jz soundLoop

	mov al, 0
	out IO_SND_CH_CTRL, al

	ret
;-----------------------------------------------------------------------------
; Test sound sweep.
;-----------------------------------------------------------------------------
testSoundSweep:
	mov si, testingSweepStr
	call writeString

	mov si, testingSweep0Str
	call writeString

	mov si, testingSweep1Str
	call writeString

	mov ax, 0x800-0x140			;500Hz?
	out IOw_SND_FREQ_3, ax

	mov al, 0xFF
	out IO_SND_VOL_3, al

	xor ah,ah
	mov al, SND_3_ON | SND_3_SWEEP
	out IO_SND_CH_CTRL, al
	mov al, 0x9
	out IO_SND_OUT_CTRL, al
	mov bl, 6
	mov cl, 1
sweepLoop:
	test al, (PAD_LEFT<<4)
	jz noSweepAdd
	sub bl, 2
	jns noSweepAdd
	mov bl, 0
noSweepAdd:
	test al, (PAD_RIGHT<<4)
	jz noSweepSub
	add bl, 2
	cmp bl, 6
	jc noSweepSub
	mov bl, 6
noSweepSub:
	mov al, bl
	out IO_SND_SWEEP, al

	mov al, cl
	out IO_SND_SWEEP_TIME, al

	hlt
	mov ax, [es:keysDown]
	test al, PAD_A | PAD_B
	jz sweepLoop

	mov al, 0
	out IO_SND_CH_CTRL, al

	ret
;-----------------------------------------------------------------------------
; Test noise values.
;-----------------------------------------------------------------------------
testNoiseValues:
	mov si, testingNoiseStr
	call writeString

	mov ax, 0x800-0x200			;Change every raster row.
	out IOw_SND_FREQ_4, ax

	mov al, 0x22
	out IO_SND_VOL_4, al

	mov al, SND_4_ON | SND_4_NOISE
	out IO_SND_CH_CTRL, al
	mov al, 0x9
	out IO_SND_OUT_CTRL, al

	mov bp, 0x4080<<1			; Tap bit 14 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 0
	mov cx, 32767
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov bp, 0x0480<<1			; Tap bit 10 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 1
	mov cx, 1953
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov bp, 0x2080<<1			; Tap bit 13 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 2
	mov cx, 254
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov bp, 0x0090<<1			; Tap bit 4 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 3
	mov cx, 217
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov bp, 0x0180<<1			; Tap bit 8 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 4
	mov cx, 73
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov bp, 0x00C0<<1			; Tap bit 6 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 5
	mov cx, 63
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov bp, 0x0280<<1			; Tap bit 9 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 6
	mov cx, 42
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov bp, 0x0880<<1			; Tap bit 11 & 7
	mov al, NOISE_ENABLE | NOISE_RESET | 7
	mov cx, 28
	call runNoiseTest
	cmp al, 0
	jnz noiseNotOk

	mov si, okStr
	jmp noiseIsOk
noiseNotOk:
	mov si, failedStr
noiseIsOk:
	call writeString
	mov al, 0
	out IO_SND_CH_CTRL, al

	ret

runNoiseTest:
	mov bl, al
	and ax, 7
	mov si, testingNoiseMode
	call writeStartReg
	mov al, bl
	xor bx, bx
	cli
	out IO_SND_NOISE_CTRL, al
noiseLoop:
	in ax, IOw_SND_RANDOM
	cmp ax, bx
	jz noiseLoop

	add bx, bx
	mov dx, bx
	and dx, bp
	jz eqNoise
	xor dx, bp
	jnz neNoise
eqNoise:
	or bl, 1
neNoise:
	and bh, 0x7F
	cmp ax, bx
	jnz noiseFail
	loop noiseLoop
noiseFinished:
	sti
	ret
noiseFail:
	push bx
	mov si, testingNoiseReadVal
	call writeStartReg
	pop ax
	mov si, testingNoiseExpVal
	call writeStartReg
	mov al, 1
	sti
	ret

;-----------------------------------------------------------------------------
; Wait for input, A continue, B cancel.
;-----------------------------------------------------------------------------
checkKeyInput:
	hlt
	mov al, [es:keysDown]
	test al, PAD_A | PAD_B | PAD_START
	jz checkKeyInput
	and al, PAD_A
	jz keyCancel
	mov al, 1
keyCancel:
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

waitLine:
   in al, IO_LCD_LINE
   cmp al,bl
   jnz waitLine
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
	push cx
	mov di, foregroundMap
	; Clear a tilemap by writing space (0x20) to all locations.
	mov ax, BG_CHR( ' ', 4, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov cx, MAP_TWIDTH * SCREEN_THEIGHT
	rep stosw
	pop cx
	ret
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
	push cx
	mov cx, SCREEN_TWIDTH * SCREEN_THEIGHT
textLoop:
	lodsb
	int 0x10
	xor al, 0
	jz endString
	loop textLoop
endString:
	pop cx
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
; Special Keypad handler for PCV2
;-----------------------------------------------------------------------------
checkPCV2Keys:
	push si
	xor ah,ah
	xor bh,bh
	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	daa
	in al, IO_KEYPAD
	and al, 0x0F
	mov si, ax
	mov bl,[ds:pcv2udl, si]
	shl bl, 4
	mov al, KEYPAD_READ_ARROWS_H
	out IO_KEYPAD, al
	daa
	in al, IO_KEYPAD
	and al, 0x0F
	mov si, ax
	mov al,[ds:pcv2rev, si]
	or bl, al
	mov al, KEYPAD_READ_ARROWS_V
	out IO_KEYPAD, al
	daa
	in al, IO_KEYPAD
	and al, 0x0F
	mov si, ax
	mov al,[ds:pcv2pcc, si]
	or bl, al

	pop si
	ret
;-----------------------------------------------------------------------------
; Our vblank interrupt handler
;-----------------------------------------------------------------------------
vblankInterruptHandler:
	push ax
	push bx

	mov al, [ss:pcv2Mode]
	cmp al, 2
	jnz normalKeys
	call checkPCV2Keys
	jmp setKeypad
normalKeys:
	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	daa
	in al, IO_KEYPAD
	mov bl, al
	and bl, 0x0F
	mov al, KEYPAD_READ_ARROWS_H
	out IO_KEYPAD, al
	daa
	in al, IO_KEYPAD
	shl al, 4
	or bl, al
	mov al, KEYPAD_READ_ARROWS_V
	out IO_KEYPAD, al
	daa
	in al, IO_KEYPAD
	mov bh, al
setKeypad:
	mov ax, [es:keysHeld]
	mov [es:keysHeld], bx
	xor ax, bx
	and ax, bx
	mov [es:keysDown], ax

	; globalFrameCounter++
	inc word [es:globalFrameCounter]
	inc word [es:vblankIrqCount]

	mov ax, [es:bgPos]
	out IO_SCR1_SCRL_X, ax
	mov ax, [es:fgPos]
	out IO_SCR2_SCRL_X, ax

;	mov al, [es:isTesting]
;	xor al, 0
;	jz skipValuePrint
;skipValuePrint:
acknowledgeVBlankInterrupt:
	mov al, INT_VBLANK_START
	out INT_CAUSE_CLEAR, al

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
	inc word[es:nmiIrqCount]
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
; The HBlank Timer handler
;-----------------------------------------------------------------------------
hblankTimerHandler:
	push ax
	mov ax, [es:hblankTimerIrqCount]
	inc ax
	mov [es:hblankTimerIrqCount], ax
	cmp ax, 2
	js hblankCont
	; Acknowledge HBlank interrupt
	mov al, INT_HBLANK_TIMER 
	out INT_CAUSE_CLEAR, al
hblankCont:
	pop ax
	iret
;-----------------------------------------------------------------------------
; The VBlank handler
;-----------------------------------------------------------------------------
vblankHandler:
	inc word[es:vblankIrqCount]
	iret
;-----------------------------------------------------------------------------
; The VBlank Timer handler
;-----------------------------------------------------------------------------
vblankTimerHandler:
	inc word[es:vblankTimerIrqCount]
	iret
;-----------------------------------------------------------------------------
; The Line Compare handler
;-----------------------------------------------------------------------------
lineCompareHandler:
	inc word[es:lineCompareIrqCount]
	iret
;-----------------------------------------------------------------------------
; The Serial Receive handler
;-----------------------------------------------------------------------------
serialReceiveHandler:
	inc word[es:serialReceiveIrqCount]
	iret
;-----------------------------------------------------------------------------
; The Cartridge Irq handler
;-----------------------------------------------------------------------------
cartridgeIrqHandler:
	inc word[es:cartridgeIrqCount]
	iret
;-----------------------------------------------------------------------------
; The Key Press handler
;-----------------------------------------------------------------------------
keyPressHandler:
	push ax
	in al, IO_LCD_LINE
	mov [es:keyIrqLine], al
	inc word[es:keyPressIrqCount]
	mov al, INT_KEY_PRESS
	out INT_CAUSE_CLEAR, al
	pop ax
	iret
;-----------------------------------------------------------------------------
; The Serial Transmit handler
;-----------------------------------------------------------------------------
serialTransmitHandler:
	inc word[es:serialTransmitIrqCount]
	iret

;-----------------------------------------------------------------------------
; Write a char to background. al = char
;-----------------------------------------------------------------------------
outputCharHandler:
	push bx
	push cx
	push di

	cmp al, 10
	jz newLine
	mov cl, [es:cursorXPos]
	xor al, 0
	jz endOutput
	xor bh, bh
	mov bl, [es:cursorYPos]
	and bl, 0x1F
	shl bx, 5		; bx * MAP_TWIDTH
	add bl, cl
	shl bx, 1
	mov di, backgroundMap
	add di, bx
	stosb
	inc cl
	cmp cl, 28
	jnz endOutput
newLine:
	mov bl, [es:cursorYPos]
	inc bl
	mov al, bl
	sub al, SCREEN_THEIGHT
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
SECTION .data start=0xF0000

	align 2

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
	db 0x00,0x00,0x60,0x92,0x0C,0x00,0x00,0x00,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF

SoundSamples:
	db 0x98,0xCB,0xED,0xFF,0xFF,0xEF,0xBD,0x8A,0x57,0x24,0x01,0x00,0x00,0x21,0x43,0x76
	db 0x98,0xCB,0xED,0xFF,0xFF,0xEF,0xBD,0x8A,0x57,0x24,0x01,0x00,0x00,0x21,0x43,0x76
	db 0x98,0xCB,0xED,0xFF,0xFF,0xEF,0xBD,0x8A,0x57,0x24,0x01,0x00,0x00,0x21,0x43,0x76
	db 0x98,0xCB,0xED,0xFF,0xFF,0xEF,0xBD,0x8A,0x57,0x24,0x01,0x00,0x00,0x21,0x43,0x76

ioRegMasksASWAN:
	dw 0x003f, 0x0007, 0xffff, 0x00ff, 0x001f, 0x007f, 0x00ff, 0x0077 ; 0x00
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x08
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00f1, 0x003f, 0x00ff, 0x00ff ; 0x10
	dw 0xffff, 0x9090, 0xffff, 0x0000, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x18
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x20
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x28
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x30
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x38
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x40
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x48
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x50
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x58
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x60
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x68
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x70
	dw 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090, 0x9090 ; 0x78
	dw 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007 ; 0x80
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x001f, 0x0017, 0x00ff ; 0x88
	dw 0x00ef, 0xffff, 0xffff, 0xffff, 0x000f, 0x00ff, 0xffff, 0xffff ; 0x90
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x9090, 0x9090 ; 0x98
	dw 0xffff, 0x9090, 0x000f, 0x000f, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0xA0
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0x9090, 0x9090, 0x9090, 0x9090 ; 0xA8
ioRegMasksSPHINXMono:
	dw 0x003f, 0x0007, 0xffff, 0x00ff, 0x001f, 0x007f, 0x00ff, 0x0077 ; 0x00
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x08
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0xffff, 0x003f, 0x00ff, 0x00ff ; 0x10
	dw 0xffff, 0x0000, 0xffff, 0x0000, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x18
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x20
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x28
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x30
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x38
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x40
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x48
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x50
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x58
	dw 0xffff, 0x0000, 0xffff, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x60
	dw 0x0000, 0x0000, 0x00ff, 0x006f, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x68
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x70
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x78
	dw 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007 ; 0x80
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x001f, 0x0017, 0x00ff ; 0x88
	dw 0x00ef, 0xffff, 0xffff, 0xffff, 0x000f, 0x00ff, 0xffff, 0xffff ; 0x90
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0003, 0x0000 ; 0x98
	dw 0xffff, 0x0000, 0x000f, 0x000f, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0xA0
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0000, 0x0000, 0x0000 ; 0xA8
ioRegMasksSPHINX:
	dw 0x003f, 0x00ff, 0xffff, 0x00ff, 0x003f, 0x007f, 0x00ff, 0x00ff ; 0x00
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x08
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0xffff, 0x003f, 0x00ff, 0x00ff ; 0x10
	dw 0xffff, 0x0000, 0xffff, 0x0000, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x18
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x20
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x28
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x30
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x38
	dw 0x00fe, 0x00ff, 0x000f, 0x0000, 0x00fe, 0x00ff, 0x00fe, 0x00ff ; 0x40
	dw 0x0040, 0x0000, 0x00ff, 0x00ff, 0x000f, 0x0000, 0x00ff, 0x00ff ; 0x48
	dw 0x000f, 0x0000, 0x005f, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x50
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x58
	dw 0xffff, 0x0000, 0xffff, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x60
	dw 0x0000, 0x0000, 0x00ff, 0x006f, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x68
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x70
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x78
	dw 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007 ; 0x80
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x001f, 0x0017, 0x00ff ; 0x88
	dw 0x00ef, 0xffff, 0xffff, 0xffff, 0x000f, 0x00ff, 0xffff, 0xffff ; 0x90
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0003, 0x0000 ; 0x98
	dw 0xffff, 0x0000, 0x000f, 0x000f, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0xA0
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0000, 0x0000, 0x0000 ; 0xA8
ioRegMasksSPHINX2Mono:
	dw 0x003f, 0x0007, 0xffff, 0x00ff, 0x001f, 0x007f, 0x00ff, 0x0077 ; 0x00
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x08
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x0001, 0x003f, 0x00ff, 0x0000 ; 0x10
	dw 0xffff, 0x0000, 0xffff, 0x0000, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x18
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x20
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x28
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x30
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x38
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x40
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x48
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x50
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x58
	dw 0xffff, 0x0000, 0x8081, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x60
	dw 0x0000, 0x0000, 0x00ff, 0x006f, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x68
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff ; 0x70
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x78
	dw 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007 ; 0x80
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x001f, 0x0017, 0x00ff ; 0x88
	dw 0x00ef, 0xffff, 0xffff, 0xffff, 0x000f, 0x00ff, 0xffff, 0xffff ; 0x90
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0003, 0x0000 ; 0x98
	dw 0xffff, 0x0000, 0x000f, 0x000f, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0xA0
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0000, 0x0000, 0x0000 ; 0xA8
ioRegMasksSPHINX2:
	dw 0x003f, 0x00ff, 0xffff, 0x00ff, 0x003f, 0x007f, 0x00ff, 0x00ff ; 0x00
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x08
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x0001, 0x003f, 0x00ff, 0x0000 ; 0x10
	dw 0xffff, 0x0000, 0xffff, 0x0000, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0x18
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x20
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x28
	dw 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077, 0x0077 ; 0x30
	dw 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077, 0x0070, 0x0077 ; 0x38
	dw 0x00fe, 0x00ff, 0x000f, 0x0000, 0x00fe, 0x00ff, 0x00fe, 0x00ff ; 0x40
	dw 0x0040, 0x0000, 0x00ff, 0x00ff, 0x000f, 0x0000, 0x00ff, 0x00ff ; 0x48
	dw 0x000f, 0x0000, 0x005f, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x50
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x58
	dw 0xffff, 0x0000, 0x8081, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x60
	dw 0x0000, 0x0000, 0x00ff, 0x006f, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x68
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff ; 0x70
	dw 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000 ; 0x78
	dw 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007, 0x00ff, 0x0007 ; 0x80
	dw 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x00ff, 0x001f, 0x0017, 0x00ff ; 0x88
	dw 0x00ef, 0xffff, 0xffff, 0xffff, 0x000f, 0x00ff, 0xffff, 0xffff ; 0x90
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0003, 0x0000 ; 0x98
	dw 0xffff, 0x0000, 0x000f, 0x000f, 0x00ff, 0x00ff, 0x00ff, 0x00ff ; 0xA0
	dw 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0x0000, 0x0000, 0x0000 ; 0xA8

pcv2udl:
	db 0x0, 0x8, 0x0, 0x8, 0x4, 0xC, 0x4, 0xC, 0x1, 0x9, 0x1, 0x9, 0x5, 0xD, 0x5, 0xD
pcv2rev:
	db 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1
pcv2pcc:
	db 0x0, 0x8, 0x0, 0x4, 0x8, 0xC, 0x8, 0xC, 0x0, 0x4, 0x0, 0x4, 0x8, 0xC, 0x8, 0xC

prepareData:
	dw 0xF0AB, 0x570D

alphabet: db "ABCDEFGHIJKLMNOPQRSTUVWXYZ!", 10, 0
alphabet2: db "abcdefghijklmnopqrstuvwxyz.,", 10, 0

headLineStr: db " WonderSwan HW Test 20250409", 10, 0

menuShowRegistersStr: db "  ShowStartup Registers.", 10, 0
menuTestAllStr: db "  Test All.",10 , 0
menuTestInterruptStr: db "  Test Interrupt Manager.", 10, 0
menuTestTimersStr: db "  Test Timers.", 10, 0
menuTestIORegsStr: db "  Test IO Regs.", 10, 0
menuTestWindowsStr: db "  Test Windows.", 10, 0
menuTestSoundStr: db "  Test Sound Mixer.", 10, 0
menuTestSweepStr: db "  Test Sound Sweep.", 10, 0
menuTestNoiseStr: db "  Test Noise Values.", 10, 0
menuLcdOffStr: db "  LCD Off.", 10, 0
menuPowerOffStr: db "  Power Off.", 10, 0

startFStr:   db "F:      ", 0
startAWStr:  db "AW/AX:  ", 0
startCWStr:  db "CW/CX:  ", 0
startDWStr:  db "DW/DX:  ", 0
startBWStr:  db "BW/BX:  ", 0
startSPStr:  db "SP:     ", 0
startBPStr:  db "BP:     ", 0
startIXStr:  db "IX/SI:  ", 0
startIYStr:  db "IY/DI:  ", 0
startDS1Str: db "DS1/ES: ", 0
startPSStr:  db "PS/CS:  ", 0
startSSStr:  db "SS:     ", 0
startDS0Str: db "DS0/DS: ", 0

testingIrqStr: db "Interrupt Manager", 10, 0
testingIrq0Str: db "IRQs not set when disabled:", 10, 0
testingIrq1Str: db "IRQs set when enabled:", 10, 0
testingIrq2Str: db "IRQs latched when disabled:", 10, 0
testingIrq3Str: db "IRQs cleared when acked:", 10, 0
testingIrq4Str: db "IRQs happen after waiting:", 10, 0
testingIrq5Str: db "IRQs happen when disabled:", 10, 0
testingIrq6Str: db "No IRQs after acknowledged:", 10, 0
testingIrq7Str: db "Multiple IRQs without ack:", 10, 0
testingIrq8Str: db "Serial IRQ test:", 10, 0

testingTimerStr: db "Timers", 10, 0
testingTimer0Str: db "Timers dont run when zero:", 10, 0
testingTimer1Str: db "Timers dont run when off:", 10, 0
testingTimer2Str: db "Timers run when on+repeat:", 10, 0
testingTimer3Str: db "Timers run when on+one shot:", 0
testingTimer4Str: db "Timers dont run, off+repeat:", 0
testingTimer5Str: db "Timers continue by on/off:", 10, 0
testingTimer6Str: db "Timers always fire when c 1:", 0

testingIORegsStr: db "IO Registers", 10, 0
testingIORegsColStr: db "IO Registers Color Mode", 10, 0

testingSoundStr: db "Sound wrapping/clipping", 10, 0
testingSound0Str: db "Y1-Y4 to turn ch on/off", 10, 0
testingSound1Str: db "X2,X4 to change shift", 10, 0
testingSound2Str: db "Remove headphones to test", 10, 0

testingSweepStr: db "Sound sweep", 10, 0
testingSweep0Str: db "Y1-Y4 to change amount", 10, 0
testingSweep1Str: db "X2,X4 to change tick", 10, 0

testingNoiseStr: db "Sound Noise Values", 10, 0
testingNoiseMode:  db "Noise Mode: ", 0
testingNoiseExpVal:  db "Noise Expected Value: ", 0
testingNoiseReadVal:  db "Noise Read Value: ", 0

testingPowerOffStr: db "Power Off only on color", 10, 0
testingLcdOffStr: db "LCD Off", 10, 0
testingKeyIrqLine:  db "Key Irq Line: ", 0

test8InputStr: db "Testing Input: 0x00", 0
test16InputStr: db "Testing Input: 0x0000", 0
test8x8InputStr: db "Testing Input: 0x00, 0x00", 0
test16x8InputStr: db "Testing Input: 0x0000, 0x00", 0
test16x16InputStr: db "Testing Inp: 0x0000, 0x0000", 0
inputStr: db "Input:0x", 0
expectedStr: db "Expected Result:", 10, 0
testedStr: db "Tested Result:", 10, 0
valueStr: db "Value:0x", 0
flagsStr: db " Flags:0x", 0
okStr: db "Ok!", 10, 0
failedStr: db "Failed!", 10, 0
doneStr: db "Done.", 10, 0
preFlagStr: db "PreF: ", 0
postFlagStr: db "PostF: ", 0
hexPrefixStr: db " 0x", 0
fHexPrefixStr: db " F:0x", 0

author: db "Written by Fredrik Ahlström, 2023-2025"

	ROM_HEADER initialize, CODESEGMENT, RH_WS_COLOR, RH_ROM_4MBITS, RH_NO_SRAM, RH_HORIZONTAL

SECTION .bss start=0x0100 ; Keep space for Int Vectors

startIORegs: resb 0xC0

startRegF: resw 1
startRegAW: resw 1
startRegCW: resw 1
startRegDW: resw 1
startRegBW: resw 1
startRegSP: resw 1
startRegBP: resw 1
startRegIX: resw 1
startRegIY: resw 1
startRegDS1: resw 1
startRegPS: resw 1
startRegSS: resw 1
startRegDS0: resw 1
bootMode: resb 1
pcv2Mode: resb 1

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

	align 2
; Keys held down
keysHeld: resw 1
; Keys pressed down since last time
keysDown: resw 1
lfsr1: resw 1
lfsr2: resw 1

nmiIrqCount: resw 1
hblankTimerIrqCount: resw 1
vblankIrqCount: resw 1
vblankTimerIrqCount: resw 1
lineCompareIrqCount: resw 1
serialReceiveIrqCount: resw 1
cartridgeIrqCount: resw 1
keyPressIrqCount: resw 1
serialTransmitIrqCount: resw 1

inputVal1: resw 1
inputVal2: resw 1
inputVal3: resw 1
inputFlags: resw 1
inputCarry: resw 1

testedResult1: resw 1
testedResult2: resw 1
testedFlags: resw 1
testedException: resw 1		; If an exception occurred.

expectedResult1: resw 1
expectedResult2: resw 1
expectedFlags: resw 1
expectedException: resw 1

boundLow: resw 1
boundHigh: resw 1

isTesting: resb 1			; If currently running test.
keyIrqLine: resb 1

selfModifyingCode: resb 8
