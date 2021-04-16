# Maxfucktor-hs
## What
A dead simple BF compiler written this time in Haskell.
## How
In the project's root you will find a `build.sh` script, that being invoked first builds the compiler itself and then immediatelly shoves provided BF source into it (requires `stack` and `yasm`).
```bash
>> ./build.sh bf_src/helloworld.b
>> ./helloworld.b.exe 
Hello World!
>>
```
Resulting ASM code should look like this:
```nasm
global _start
section .data
  memory: times 32768 db 0
section .text
_start:
  mov rsi, memory
  mov rdx, 1 ;; rdx wont change during the runtime
  mov rdi, 1 ;; rdi represents an io descriptor, typically 1 or 0
  jmp run
exit:
  mov rax, 60
  mov rdi, 0
  syscall
run:
add byte [rsi], byte 10
l1:
cmp byte [rsi], byte 0
je l2
add rsi, 1
add byte [rsi], byte 7
add rsi, 1
add byte [rsi], byte 10
add rsi, 1
add byte [rsi], byte 3
add rsi, 1
add byte [rsi], byte 1
sub rsi, 4
sub byte [rsi], byte 1
jmp l1
l2:
add rsi, 1
add byte [rsi], byte 2
mov rax, 1
syscall
add rsi, 1
add byte [rsi], byte 1
mov rax, 1
syscall
add byte [rsi], byte 7
mov rax, 1
syscall
mov rax, 1
syscall
add byte [rsi], byte 3
mov rax, 1
syscall
add rsi, 1
add byte [rsi], byte 2
mov rax, 1
syscall
sub rsi, 2
add byte [rsi], byte 15
mov rax, 1
syscall
add rsi, 1
mov rax, 1
syscall
add byte [rsi], byte 3
mov rax, 1
syscall
sub byte [rsi], byte 6
mov rax, 1
syscall
sub byte [rsi], byte 8
mov rax, 1
syscall
add rsi, 1
add byte [rsi], byte 1
mov rax, 1
syscall
jmp exit
```
Not great, not terrible, I know I know.
