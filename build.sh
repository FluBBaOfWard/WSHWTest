#!/bin/sh
romname=WSHWTest

nasm -f bin -o $romname.wsc $romname.asm -l $romname.lst
