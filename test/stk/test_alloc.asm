ADDRESS $r15 stack_pointer
READ $r14 $r15
DIRECTREAD $r0 a
# stack_depth=1, unused_regs=13, current_reg=1, used=false
INCR $r0 96
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false
DIRECTREAD $r0 a
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 a
# stack_depth=2, unused_regs=12, current_reg=2, used=false
DIRECTREAD $r2 a
# stack_depth=3, unused_regs=11, current_reg=3, used=false
DIRECTREAD $r3 a
# stack_depth=4, unused_regs=10, current_reg=4, used=false
DIRECTREAD $r4 a
# stack_depth=5, unused_regs=9, current_reg=5, used=false
DIRECTREAD $r5 a
# stack_depth=6, unused_regs=8, current_reg=6, used=false
DIRECTREAD $r6 a
# stack_depth=7, unused_regs=7, current_reg=7, used=false
DIRECTREAD $r7 a
# stack_depth=8, unused_regs=6, current_reg=8, used=false
DIRECTREAD $r8 a
# stack_depth=9, unused_regs=5, current_reg=9, used=false
DIRECTREAD $r9 a
# stack_depth=10, unused_regs=4, current_reg=10, used=false
DIRECTREAD $r10 a
# stack_depth=11, unused_regs=3, current_reg=11, used=false
DIRECTREAD $r11 a
# stack_depth=12, unused_regs=2, current_reg=12, used=false
DIRECTREAD $r12 a
# stack_depth=13, unused_regs=1, current_reg=13, used=false
DIRECTREAD $r13 a
# stack_depth=14, unused_regs=0, current_reg=0, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r0
DECR $r14 1
DIRECTREAD $r0 a
# stack_depth=15, unused_regs=0, current_reg=1, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r1
DECR $r14 1
DIRECTREAD $r1 a
# stack_depth=16, unused_regs=0, current_reg=2, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r2
DECR $r14 1
DIRECTREAD $r2 a
# stack_depth=17, unused_regs=0, current_reg=3, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r3
DECR $r14 1
DIRECTREAD $r3 a
# stack_depth=18, unused_regs=0, current_reg=4, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r4
DECR $r14 1
DIRECTREAD $r4 a
# stack_depth=19, unused_regs=0, current_reg=5, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r5
DECR $r14 1
DIRECTREAD $r5 a
# stack_depth=20, unused_regs=0, current_reg=6, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r6
DECR $r14 1
DIRECTREAD $r6 a
# stack_depth=21, unused_regs=0, current_reg=7, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r7
DECR $r14 1
DIRECTREAD $r7 a
# stack_depth=22, unused_regs=0, current_reg=8, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r8
DECR $r14 1
DIRECTREAD $r8 a
# stack_depth=23, unused_regs=0, current_reg=9, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r9
DECR $r14 1
DIRECTREAD $r9 a
# stack_depth=24, unused_regs=0, current_reg=10, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r10
DECR $r14 1
DIRECTREAD $r10 a
# stack_depth=25, unused_regs=0, current_reg=11, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r11
DECR $r14 1
DIRECTREAD $r11 a
# stack_depth=26, unused_regs=0, current_reg=12, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r12
DECR $r14 1
DIRECTREAD $r12 a
# stack_depth=27, unused_regs=0, current_reg=13, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r13
DECR $r14 1
DIRECTREAD $r13 a
# stack_depth=28, unused_regs=0, current_reg=0, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r0
DECR $r14 1
DIRECTREAD $r0 a
# stack_depth=29, unused_regs=0, current_reg=1, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r1
DECR $r14 1
DIRECTREAD $r1 a
# stack_depth=30, unused_regs=0, current_reg=2, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r2
DECR $r14 1
DIRECTREAD $r2 a
# stack_depth=31, unused_regs=0, current_reg=3, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r3
DECR $r14 1
DIRECTREAD $r3 a
# stack_depth=32, unused_regs=0, current_reg=4, used=true
# Empilement de la plus ancienne valeur
WRITE $r14 $r4
DECR $r14 1
DIRECTREAD $r4 a
# stack_depth=33, unused_regs=0, current_reg=5, used=true
NOP
# stack_depth=33, unused_regs=0, current_reg=5, used=true
NOP
# stack_depth=33, unused_regs=0, current_reg=5, used=true
DECR $r14 1
ADD $r3 $r3 $r4
# stack_depth=32, unused_regs=1, current_reg=4, used=false
DECR $r14 1
ADD $r2 $r2 $r3
# stack_depth=31, unused_regs=2, current_reg=3, used=false
DECR $r14 1
ADD $r1 $r1 $r2
# stack_depth=30, unused_regs=3, current_reg=2, used=false
DECR $r14 1
ADD $r0 $r0 $r1
# stack_depth=29, unused_regs=4, current_reg=1, used=false
DECR $r14 1
ADD $r13 $r13 $r0
# stack_depth=28, unused_regs=5, current_reg=0, used=false
DECR $r14 1
ADD $r12 $r12 $r13
# stack_depth=27, unused_regs=6, current_reg=13, used=false
DECR $r14 1
ADD $r11 $r11 $r12
# stack_depth=26, unused_regs=7, current_reg=12, used=false
DECR $r14 1
ADD $r10 $r10 $r11
# stack_depth=25, unused_regs=8, current_reg=11, used=false
DECR $r14 1
ADD $r9 $r9 $r10
# stack_depth=24, unused_regs=9, current_reg=10, used=false
DECR $r14 1
ADD $r8 $r8 $r9
# stack_depth=23, unused_regs=10, current_reg=9, used=false
DECR $r14 1
ADD $r7 $r7 $r8
# stack_depth=22, unused_regs=11, current_reg=8, used=false
DECR $r14 1
ADD $r6 $r6 $r7
# stack_depth=21, unused_regs=12, current_reg=7, used=false
DECR $r14 1
ADD $r5 $r5 $r6
# stack_depth=20, unused_regs=13, current_reg=6, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r4 $r14
DECR $r14 1
ADD $r4 $r4 $r5
# stack_depth=19, unused_regs=13, current_reg=5, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r3 $r14
DECR $r14 1
ADD $r3 $r3 $r4
# stack_depth=18, unused_regs=13, current_reg=4, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r2 $r14
DECR $r14 1
ADD $r2 $r2 $r3
# stack_depth=17, unused_regs=13, current_reg=3, used=false
INCR $r2 16
# stack_depth=17, unused_regs=13, current_reg=3, used=false
PRINT $r2
# stack_depth=16, unused_regs=14, current_reg=2, used=false
NOP
# stack_depth=16, unused_regs=14, current_reg=2, used=false
NOP
# stack_depth=16, unused_regs=14, current_reg=2, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r1 $r14
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r0 $r14
DECR $r14 1
ADD $r0 $r0 $r1
# stack_depth=15, unused_regs=13, current_reg=1, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r13 $r14
DECR $r14 1
ADD $r13 $r13 $r0
# stack_depth=14, unused_regs=13, current_reg=0, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r12 $r14
DECR $r14 1
ADD $r12 $r12 $r13
# stack_depth=13, unused_regs=13, current_reg=13, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r11 $r14
DECR $r14 1
ADD $r11 $r11 $r12
# stack_depth=12, unused_regs=13, current_reg=12, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r10 $r14
DECR $r14 1
ADD $r10 $r10 $r11
# stack_depth=11, unused_regs=13, current_reg=11, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r9 $r14
DECR $r14 1
ADD $r9 $r9 $r10
# stack_depth=10, unused_regs=13, current_reg=10, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r8 $r14
DECR $r14 1
ADD $r8 $r8 $r9
# stack_depth=9, unused_regs=13, current_reg=9, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r7 $r14
DECR $r14 1
ADD $r7 $r7 $r8
# stack_depth=8, unused_regs=13, current_reg=8, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r6 $r14
DECR $r14 1
ADD $r6 $r6 $r7
# stack_depth=7, unused_regs=13, current_reg=7, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r5 $r14
DECR $r14 1
ADD $r5 $r5 $r6
# stack_depth=6, unused_regs=13, current_reg=6, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r4 $r14
DECR $r14 1
ADD $r4 $r4 $r5
# stack_depth=5, unused_regs=13, current_reg=5, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r3 $r14
DECR $r14 1
ADD $r3 $r3 $r4
# stack_depth=4, unused_regs=13, current_reg=4, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r2 $r14
DECR $r14 1
ADD $r2 $r2 $r3
# stack_depth=3, unused_regs=13, current_reg=3, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r1 $r14
DECR $r14 1
ADD $r1 $r1 $r2
# stack_depth=2, unused_regs=13, current_reg=2, used=false
# Désempilement de la plus récente valeur du stack
INCR $r14 1
READ $r0 $r14
DECR $r14 1
ADD $r0 $r0 $r1
# stack_depth=1, unused_regs=13, current_reg=1, used=false
INCR $r0 17
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
EXIT
# stack_depth=0, unused_regs=14, current_reg=0, used=false
a:
1
stack_pointer:
65535