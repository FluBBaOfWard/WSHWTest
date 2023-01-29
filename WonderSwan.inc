;--------------------------------------------
;     Wonderswan Registers & Equates v0.2
;              by Orion_ [2008]
;
; http://onorisoft.free.fr/
;
; with help of WStech24.txt by Judge and Dox
;--------------------------------------------

; IO_	mean byte access
; IOw_	mean word access


;-------------
; Rom Header

RH_ROM_4MBITS		equ	0x02
RH_ROM_8MBITS		equ	0x03
RH_ROM_16MBITS		equ	0x04
RH_ROM_32MBITS		equ	0x06
RH_ROM_64MBITS		equ	0x08
RH_ROM_128MBITS		equ	0x09

RH_NO_SRAM			equ	0x00
RH_SRAM_64KBITS		equ	0x01
RH_SRAM_256KBITS	equ	0x02
RH_SRAM_1MBITS		equ	0x03
RH_SRAM_2MBITS		equ	0x04
RH_SRAM_1KBITS		equ	0x10
RH_SRAM_16KBITS		equ	0x20
RH_SRAM_8KBITS		equ	0x50

RH_WS_MONO		equ	0x00
RH_WS_COLOR		equ	0x01

RH_NO_RTC		equ	0x00
RH_RTC			equ	0x01

RH_HORIZONTAL	equ	(0x04 + 0x00)
RH_VERTICAL		equ	(0x04 + 0x01)



;-----------------
; Memory Address

WS_RAM_BASE	equ	0x0000
WS_TILE_BANK	equ	0x2000
WS_TILE_BANK2	equ	0x4000
WS_STACK	equ	WS_TILE_BANK-2

WSC_TILE_BANK1	equ	0x4000
WSC_TILE_BANK2	equ	0x8000
WSC_RAM_BASE2	equ	0xC000
WSC_PALETTES	equ	0xFE00
WSC_STACK	equ	WSC_PALETTES-2

MAP_SIZE	equ	0x800
SPR_TABLE_SIZE	equ	0x200



;-------------
; Interrupts

INTVEC_HBLANK_TIMER	equ	7
INTVEC_VBLANK_START	equ	6
INTVEC_VBLANK_TIMER	equ	5
INTVEC_DRAWING_LINE	equ	4
INTVEC_SERIAL_RECEIVE	equ	3
INTVEC_RTC_ALARM	equ	2
INTVEC_KEY_PRESS	equ	1
INTVEC_SERIAL_SEND	equ	0



;-----------------
; Useful Defines

%define BG_CHR(tile,pal,bank,hflip,vflip)	(((vflip) << 15) | ((hflip) << 14) | ((bank) << 13) | ((pal) << 9) | (tile))

%macro	ROM_HEADER	6	; Label, Segment, WSType, RomSize, SRamSize, WSSpec
	times	((64*1024)-16)-$+%1 db 0xFF

	db	0xEA	; jmpf
	dw	%1	; Label
	dw	%2	; Segment
	db	0x00

	db	0x42	; Developer ID
	db	%3
	db	0x01	; Cart number
	db	0x00
	db	%4
	db	%5
	db	%6
	db	0x00
	dw	0x0000	; Checksum
%endmacro

%macro	PADDING	1	; Number of Segment
	times	(%1*64*1024) db 0xFF
%endmacro

SCREEN_WIDTH	equ	224
SCREEN_HEIGHT	equ	144
SCREEN_TWIDTH	equ	(SCREEN_WIDTH / 8)
SCREEN_THEIGHT	equ	(SCREEN_HEIGHT / 8)
MAP_WIDTH		equ	256
MAP_HEIGHT		equ	256
MAP_TWIDTH		equ	(MAP_WIDTH / 8)
MAP_THEIGHT		equ	(MAP_HEIGHT / 8)



;-----------------------------------
; I/O Ports and associated equates

IO_DISPLAY_CTRL	equ	0x00
BG_ON			equ	0x01
BG_OFF			equ	0x00
FG_ON			equ	0x02
FG_OFF			equ	0x00
SPR_ON			equ	0x04
SPR_OFF			equ	0x00
SPR_WIN_ON		equ	0x08
SPR_WIN_OFF		equ	0x00
FG_IN_OUT_WIN	equ	0x00
FG_IN_WIN		equ	0x10
FG_OUT_WIN		equ	0x30

IO_BG_PAL		equ	0x01
%define	BG_COLOR(a)	(a)
%define	BG_PAL(a)	(a << 4)

IO_LCD_LINE		equ	0x02
IO_LCD_INTERRUPT	equ	0x03

IO_SPR_AREA		equ	0x04
%define	SPR_AREA(a)	(a >> 9)	; Sprite Table Address must be 512 bytes aligned !

IO_SPR_START_NO	equ	0x05
IO_SPR_CNT		equ	0x06

IO_SCR_AREA		equ	0x07
%define	FG_MAP(a)	((a >> 11) << 4)	; FG Map Address must be 2048 bytes aligned !
%define	BG_MAP(a)	(a >> 11)		; BG Map Address must be 2048 bytes aligned !

IO_SCR2_WIN_X1	equ	0x08
IO_SCR2_WIN_Y1	equ	0x09
IO_SCR2_WIN_X2	equ	0x0A
IO_SCR2_WIN_Y2	equ	0x0B

IO_SPR_WIN_X1	equ	0x0C
IO_SPR_WIN_Y1	equ	0x0D
IO_SPR_WIN_X2	equ	0x0E
IO_SPR_WIN_Y2	equ	0x0F

IO_SCR1_SCRL_X	equ	0x10
IO_SCR1_SCRL_Y	equ	0x11

IO_SCR2_SCRL_X	equ	0x12
IO_SCR2_SCRL_XY	equ	0x13

IO_LCD_IF_CTRL	equ	0x14
LCD_ON			equ	0x01
LCD_OFF			equ	0x00

IO_LCD_SEG_DATA	equ	0x15
LCD_ICON_SLEEP	equ	0x01
LCD_ICON_VERTI	equ	0x02
LCD_ICON_HORIZ	equ	0x04
LCD_ICON_DOT1	equ	0x08
LCD_ICON_DOT2	equ	0x10
LCD_ICON_DOT3	equ	0x20

IO_LCD_GRAY_01	equ	0x1C
IO_LCD_GRAY_23	equ	0x1D
IO_LCD_GRAY_45	equ	0x1E
IO_LCD_GRAY_67	equ	0x1F

IOw_SCR_LUT_0	equ	0x20
IOw_SCR_LUT_1	equ	0x22
IOw_SCR_LUT_2	equ	0x24
IOw_SCR_LUT_3	equ	0x26
IOw_SCR_LUT_4	equ	0x28
IOw_SCR_LUT_5	equ	0x2A
IOw_SCR_LUT_6	equ	0x2C
IOw_SCR_LUT_7	equ	0x2E
IOw_SPR_LUT_0	equ	0x30
IOw_SPR_LUT_1	equ	0x32
IOw_SPR_LUT_2	equ	0x34
IOw_SPR_LUT_3	equ	0x36
IOw_SPR_LUT_4	equ	0x38
IOw_SPR_LUT_5	equ	0x3A
IOw_SPR_LUT_6	equ	0x3C
IOw_SPR_LUT_7	equ	0x3E

IOw_DMA_SRC		equ	0x40
IO_DMA_SRC_BANK	equ	0x42
IOw_DMA_DST		equ	0x44
IOw_DMA_SIZE	equ	0x46
IO_DMA_CTRL		equ	0x48
DMA_START		equ	0x80
DMA_CHECK		equ	0x80

IOw_SNDDMA_SRC	equ	0x4A
IO_SNDDMA_BANK	equ	0x4C
IOw_SNDDMA_SIZE	equ	0x4E
IO_SNDDMA_CTRL	equ	0x52

SYSTEM_CTRL2	equ	0x60
VMODE_16C_CHK	equ	0xE0	; 16 colors per tile chunky mode
VMODE_16C_PLN	equ	0xC0	; 16 colors per tile planar mode
VMODE_4C		equ	0x80	; 4 colors per tile color
VMODE_4C_MONO	equ	0x00	; 4 colors per tile mono
VMODE_CLEANINIT	equ	0x0C	; (?) from FF2

SYSTEM_CTRL3	equ 0x62

IOw_SND_FREQ_1	equ	0x80	; Frequency
IOw_SND_FREQ_2	equ	0x82
IOw_SND_FREQ_3	equ	0x84
IOw_SND_FREQ_4	equ	0x86

IO_SND_VOL_1	equ	0x88	; Volume
IO_SND_VOL_2	equ	0x89
IO_SND_VOL_3	equ	0x8A
IO_SND_VOL_4	equ	0x8B

IO_SND_SWEEP	equ	0x8C
IO_SND_SWEEP_TIME	equ	0x8D

IO_SND_NOISE_CTRL	equ	0x8E
%define	NOISE_TYPE(a)	(a)
NOISE_RESET		equ	0x08
NOISE_ENABLE		equ	0x10

IO_WAVE_RAM		equ	0x8F	; Sample  location
%define	WAVE_RAM(a)	(a >> 6)

IO_SND_CH_CTRL	equ	0x90
SND_1_ON		equ	0x01
SND_1_OFF		equ	0x00
SND_2_ON		equ	0x02
SND_2_OFF		equ	0x00
SND_3_ON		equ	0x04
SND_3_OFF		equ	0x00
SND_4_ON		equ	0x08
SND_4_OFF		equ	0x00
SND_2_VOICE		equ	0x20
SND_3_SWEEP		equ	0x40
SND_4_NOISE		equ	0x80

IO_SND_OUT_CTRL	equ	0x91
SND_OUT_MONO	equ	0x01
SND_OUT_STEREO	equ	0x08
%define	SND_OUT_VOLUME(a)	((a & 0x03) << 1)

IOw_SND_RANDOM	equ	0x92
IO_SND_VOL_CH2	equ	0x94	; Ch2 Sample Volume (4 bits, left/right)

SYSTEM_CTRL1	equ	0xA0
WS_COLOR		equ	0x02
WS_MONO			equ	0x00

IO_TIMER_CTRL			equ	0xA2
HBLANK_TIMER_ON			equ	0x01
HBLANK_TIMER_OFF		equ	0x00
HBLANK_TIMER_MODE_ONESHOT	equ	0x00
HBLANK_TIMER_MODE_AUTOPRESET	equ	0x02
VBLANK_TIMER_ON			equ	0x04
VBLANK_TIMER_OFF		equ	0x00
VBLANK_TIMER_MODE_ONESHOT	equ	0x00
VBLANK_TIMER_MODE_AUTOPRESET	equ	0x08

IOw_H_BLANK_TIMER	equ	0xA4
IOw_V_BLANK_TIMER	equ	0xA6

IOw_H_BLANK_COUNTER	equ	0xA8	; Hblank Counter - 1/12000s
IOw_V_BLANK_COUNTER	equ	0xAA	; Vblank Counter - 1/75s

IO_INT_VECTOR	equ	0xB0
INT_BASE		equ	0x20

IO_SERIAL_DATA	equ	0xB1	; Communication byte

IO_INT_ENABLE		equ	0xB2
INT_HBLANK_TIMER	equ	0x80
INT_VBLANK_START	equ	0x40
INT_VBLANK_TIMER	equ	0x20
INT_DRAWING_LINE	equ	0x10
INT_SERIAL_RECEIVE	equ	0x08
INT_RTC_ALARM		equ	0x04
INT_KEY_PRESS		equ	0x02
INT_SERIAL_SEND		equ	0x01

IO_SERIAL_STATUS	equ	0xB3	; Communication direction
COMM_RECEIVE_INT_GEN	equ	0x80
COMM_SPEED_9600		equ	0x00
COMM_SPEED_38400	equ	0x40
COMM_SEND_INT_GEN	equ	0x20
COMM_SEND_COMPLETE	equ	0x04
COMM_ERROR		equ	0x02
COMM_RECEIVE_COMPLETE	equ	0x01

IO_INT_CAUSE	equ	0xB4

IO_KEYPAD		equ	0xB5
KEYPAD_READ_ARROWS_V	equ	0x10
KEYPAD_READ_ARROWS_H	equ	0x20
KEYPAD_READ_BUTTONS	equ	0x40
PAD_UP			equ	0x01
PAD_RIGHT		equ	0x02
PAD_DOWN		equ	0x04
PAD_LEFT		equ	0x08
PAD_START		equ	0x02
PAD_A			equ	0x04
PAD_B			equ	0x08

INT_CAUSE_CLEAR	equ	0xB6	; See IO_INT_ENABLE equates for values

IOw_INTERNAL_EEPROM_DATA	equ	0xBA
IOw_INTERNAL_EEPROM_ADDRESS	equ	0xBC

IOw_INTERNAL_EEPROM_CTRL	equ	0xBE
IEEPROM_INIT			equ	0x80
IEEPROM_PROTECT			equ	0x40
IEEPROM_WRITE			equ	0x20
IEEPROM_READ			equ	0x10
IEEPROM_WRITE_COMPLETE		equ	0x02
IEEPROM_READ_COMPLETE		equ	0x01

IO_ROM_BASE_BANK	equ	0xC0
IO_SRAM_BANK		equ	0xC1
IO_ROM_BANK_SEGMENT2	equ	0xC2
IO_ROM_BANK_SEGMENT3	equ	0xC3

IO_RTC_CTRL		equ	0xCA
RTC_COMMAND_RESET	equ	0x10
RTC_COMMAND_ALARM	equ	0x12
RTC_COMMAND_SET_TIME	equ	0x14
RTC_COMMAND_GET_TIME	equ	0x15
RTC_COMMAND_ACK		equ	0x80

IO_RTC_DATA	equ	0xCB
