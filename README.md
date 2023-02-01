# WSHWTest

Hardware test suite for WonderSwan

## Interrupts

It's important to make distinction between the actual CPU part and the interrupt manager/handler. Even though they are part of the same physical chip in the WonderSwan they are logicaly different parts. The interrupt manager can set the interrupt pin high or low on the CPU and then it can supply an interrupt vector on the low 8 bits of the databus when the cpu request it (when it wants to take an interrupt).

The interrupt manager latches all enabled incoming interrupt requests until they are acknowledged with a write to port 0xB6, the interrupt requests are not cleared by disabling them through 0xB2. The 2 Timer interrupts, VBlank and Line Compare interrupts are just pulsed (don't know for how many cycles) but they are latched (remembered) until they are acknowledged. The serial, key & cartridge interrupts are latched as well but they can't be acknowledged as long as they are enabled and held high by the corresponding device.

All interrupts that are latched and visible in 0xB4 also cause interrupts if/when the cpu is able to accept them.

The interrupt pin on the cpu is enabled as long as there are bits set in 0xB4, the interrupt manager allways sends the vector for the top enabled bit.
