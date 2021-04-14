#!/bin/sh
name=$(basename $1)
stack run -- $1 > $name.asm  &&
yasm -f elf64 -g dwarf2 -o $name.o $name.asm && ld $name.o -o $name.exe
