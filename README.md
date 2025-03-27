# WSHWTest V0.2.0 (20250327)

Hardware test suite for WonderSwan
These descriptions are my interpretation of what is happening, that doesn't mean this is actualy what is happening, I'm open to pull requests for both code and interpretations of how things work.

## Start Registers

On all boot roms in all modes:
SP is set to 0x2000.
PS/CS is set to 0xF000.
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

## LCD off

This turns on the Star icon and sleep mode for the LCD, turns off all interrupts except KEY.
It waits for a KEY interrupt and then disables LCD sleep & star icon.
Key interrupt seem to always happen on row 0x90.
