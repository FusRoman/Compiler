ADDRESS $r15 stack_pointer
READ $r14 $r15
DIRECTREAD $r0 a
INCR $r0 96
PRINT $r0
CONST $r0 10
PRINT $r0
NOP
DIRECTREAD $r0 a
DIRECTREAD $r1 a
DIRECTREAD $r2 a
DIRECTREAD $r3 a
DIRECTREAD $r4 a
DIRECTREAD $r5 a
DIRECTREAD $r6 a
DIRECTREAD $r7 a
DIRECTREAD $r8 a
DIRECTREAD $r9 a
DIRECTREAD $r10 a
DIRECTREAD $r11 a
DIRECTREAD $r12 a
DIRECTREAD $r13 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r0
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r0 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r1
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r1 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r2
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r2 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r3
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r3 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r4
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r4 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r5
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r5 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r6
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r6 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r7
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r7 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r8
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r8 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r9
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r9 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r10
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r10 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r11
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r11 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r12
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r12 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r13
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r13 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r0
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r0 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r1
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r1 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r2
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r2 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r3
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r3 a
# Empilement de la plus ancienne valeur
WRITE $r14 $r4
DECR $r14 1
WRITE $r15 $r14
DIRECTREAD $r4 a
NOP
NOP
ADD $r3 $r3 $r4
ADD $r2 $r2 $r3
ADD $r1 $r1 $r2
ADD $r0 $r0 $r1
ADD $r13 $r13 $r0
ADD $r12 $r12 $r13
ADD $r11 $r11 $r12
ADD $r10 $r10 $r11
ADD $r9 $r9 $r10
ADD $r8 $r8 $r9
ADD $r7 $r7 $r8
ADD $r6 $r6 $r7
ADD $r5 $r5 $r6
INCR $r14 1
READ $r4 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r4 $r4 $r5
INCR $r14 1
READ $r3 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r3 $r3 $r4
INCR $r14 1
READ $r2 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r2 $r2 $r3
INCR $r2 16
PRINT $r2
NOP
NOP
INCR $r14 1
READ $r1 $r14
INCR $r14 1
READ $r0 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r0 $r0 $r1
INCR $r14 1
READ $r13 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r13 $r13 $r0
INCR $r14 1
READ $r12 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r12 $r12 $r13
INCR $r14 1
READ $r11 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r11 $r11 $r12
INCR $r14 1
READ $r10 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r10 $r10 $r11
INCR $r14 1
READ $r9 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r9 $r9 $r10
INCR $r14 1
READ $r8 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r8 $r8 $r9
INCR $r14 1
READ $r7 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r7 $r7 $r8
INCR $r14 1
READ $r6 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r6 $r6 $r7
INCR $r14 1
READ $r5 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r5 $r5 $r6
INCR $r14 1
READ $r4 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r4 $r4 $r5
INCR $r14 1
READ $r3 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r3 $r3 $r4
INCR $r14 1
READ $r2 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r2 $r2 $r3
INCR $r14 1
READ $r1 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r1 $r1 $r2
INCR $r14 1
READ $r0 $r14
DECR $r14 1
WRITE $r15 $r14
ADD $r0 $r0 $r1
INCR $r0 17
PRINT $r0
NOP
NOP
CONST $r0 10
PRINT $r0
EXIT
a:
1
stack_pointer:
65535