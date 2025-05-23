# WSHWTest V0.2.0 (20250410)

Hardware test suite for WonderSwan
These descriptions are my interpretation of what is happening, that doesn't mean this is actualy what is happening, I'm open to pull requests for both code and interpretations of how things work.

There are also 2 other test programs that I have made.

* [WSCPUTest](https://github.com/FluBBaOfWard/WSCpuTest) - Tests functions of the NEC V30MZ CPU instructions.
* [WSTimingTest](https://github.com/FluBBaOfWard/WSTimingTest) - Tests timing of the NEC V30MZ CPU instruction.

## Start Registers

On all boot roms in all modes:
SP is set to 0x2000.
PS/CS is set to start segment, if booting in PCV2 mode it's set to 0x4000.
DS0/DS is set to boot rom base (0xFF00 for ASWAN and 0xFE00 for SPHINX/SPHINX2).

## Interrupts

It's important to make distinction between the actual CPU part and the interrupt manager/handler. Even though they are part of the same physical chip in the WonderSwan they are logicaly different parts. The interrupt manager can set the interrupt pin high or low on the CPU and then it can supply an interrupt vector on the low 8 bits of the databus when the cpu request it (when it wants to take an interrupt).

The interrupt manager latches all enabled incoming interrupt requests until they are acknowledged with a write to port 0xB6, the interrupt requests are not cleared by disabling them through 0xB2. The 2 Timer interrupts, VBlank, Line Compare & Key interrupts are edge sensitive  and latched (remembered) until they are acknowledged. The serial & cartridge interrupts are latched as well but they can't be acknowledged as long as they are enabled and asserted by the corresponding device.

All interrupts that are latched and visible in 0xB4 also cause interrupts if/when the cpu is able to accept them.

The interrupt pin on the cpu is asserted as long as there are bits set in 0xB4, the interrupt manager allways sends the vector for the highest enabled interrupt.

## Timers

The timer logic is a bit backwards, the on/off bit is used to write back the downcounted value, the downcount always happens, this means that if the timer is off and the counter value is 1 an interrupt will happen but zero is not written back to the counter so it's basicly the same as timer on with repeat.
Writing to the timer values instantly updates the counter value.
Toggling the Timer on/off doesn't reload the counter value, it just pauses / resumes the counter.

So what happens is:

1. Check if counter is zero, if so stop doing anything more.
2. Count down counter, if it's now zero tell the interrupt manager.
3. If counted down value was zero, check repeat bit and fetch timer value.
4. Is Timer on? Write back downcounted/timer value to counter.

## IO Registers

This tests the writability of the IO registers, this does not test the functionality of the registers. Not all registers are tested.

## LCD off

This turns on the Star icon and sleep mode for the LCD, turns off all interrupts except KEY.
It waits for a KEY interrupt and then disables LCD sleep & star icon.
Key interrupt seem to always happen on row 0x90.

### IO Startup Values

ASWAN IO Registers:

```text
0x00 = 0
0x01 = 0
0x02 = 0x01
0x03 = 0x40/0x20
0x04 = 0
0x05 = 0
0x06 = 0
0x07 = rnd
0x08 = 0x10
0x09 = 0x60
0x0A = 0
0x0B = 0
0x0C = 0x8A
0x0D = 0/0x40?
0x0E = 0x10
0x0F = 0x04
0x10-0x13 = rnd
0x14 = 0
0x15 = 0
0x16 = 0x9C
0x17 = 0x92
0x18 = 0x90
0x19 = 0x90
0x1A = 0x00/0x20/0x22, depending on if cart ok / headphones are connected or not.
0x1B = 0
0x1C-0x3F = rnd
0x40-7F = 0x90
0x80-0x8D = rnd
0x8E = 0
0x8F = rnd
0x90 = 0
0x91 = 0x00/0x80, depending on if headphones are connected or not.
0x92,0x93 = 0
0x94 = rnd, often 0
0x95-0x9D = 0
0x9E-0x9F = 0x90
0xA0 = 0x04
0xA1 = 0x90
0xA2-0xA4 = 0
0xA5-0xA7 = rnd
0xA8-0xB0 = 0
0xAC-0xAF = 0x90
0xB0 = 0
0xB1 = rnd (0x25/0x27/0xA5)
0xB2-0xB7 = 0
0xB8,0xB9 = 0x90
0xB8-0xBF = 0
```

Sphinx IO Registers:

```text
0x00 = 0
0x01 = 0
0x02 = 0x01
0x03 = rnd (0xB9/0xBB/0xBD)
0x04 = 0
0x05 = 0
0x06 = 0
0x07-0x12 = rnd
0x13 = 0x07/0x87?
0x14 = 0
0x15 = 0
0x16 = 0x9E
0x17 = 0x9B
0x18 = 0
0x19 = 0
0x1A = 0x00/0x20/0x22, depending on if cart ok / headphones are connected or not.
0x1B = 0
0x1C-0x3F = rnd
0x40-5F = 0
0x60 = 0x0A
0x61-0x6A = 0
0x6B = 0x0F
0x6C-0x7F = 0
0x80-0x8D = rnd
0x8E = 0
0x8F = rnd
0x90 = 0
0x91 = 0x00/0x80, depending on if headphones are connected or not.
0x92,0x93 = 0
0x94 = rnd
0x95-0x9F = 0
0xA0 = 0x06
0xA1-0xA3 = 0
0xA4-0xA7 = rnd
0xA8-0xB0 = 0
0xB1 = rnd (0x25/0x27/0xA5)
0xB2-0xBF = 0
```

Sphinx2 IO Registers:

```text
0x00 = 0
0x01 = 0
0x02 = 0x01
0x03 = rnd (0x1A)
0x04 = 0
0x05 = 0
0x06 = 0
0x07-0x12 = rnd
0x13 = 0x30?
0x14 = 0
0x15 = 0
0x16 = 0x9E
0x17 = 0
0x18 = 0
0x19 = 0
0x1A = 0x00/0x20/0x22, depending on if cart ok / headphones are connected or not.
0x1B = 0
0x1C-0x3F = rnd
0x40-5F = 0
0x60 = 0x0A
0x61 = 0
0x62 = 0x80
0x63-0x6A = 0
0x6B = 0x0F
0x6C-0x6F = 0
0x70-0x77 = SC LCD values (0xd0, 0x77, 0xf7, 0x06, 0xe2, 0x0a, 0xea, 0xee)
0x78-0x7F = 0
0x80-0x8D = rnd
0x8E = 0
0x8F = rnd
0x90 = 0
0x91 = 0x00/0x80, depending on if headphones are connected or not.
0x92,0x93 = 0
0x94 = 0x04?
0x95-0x9F = 0
0xA0 = 0x06
0xA1-0xA3 = 0
0xA4-0xA7 = rnd
0xA8-0xB0 = 0
0xB1 = rnd (0xe0?)
0xB2-0xBF = 0
```
