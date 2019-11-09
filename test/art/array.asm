ADDRESS $r15 stack_pointer
READ $r14 $r15
fill:
# stack_depth=0, unused_regs=14, current_reg=0, used=false
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false
ADDRESS $r0 a
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false
DECR $r14 1
ADD $r0 $r0 $r1
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false
INCR $r1 97
# stack_depth=2, unused_regs=12, current_reg=2, used=false
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false
ADDRESS $r0 l
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false
DECR $r1 1
# stack_depth=2, unused_regs=12, current_reg=2, used=false
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false
CONST $r0 97
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
ADDRESS $r0 fill
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false
CONST $r2 0
# stack_depth=3, unused_regs=11, current_reg=3, used=false
DECR $r14 1
GE $r1 $r1 $r2
# stack_depth=2, unused_regs=12, current_reg=2, used=false
JUMP $r0 WHEN $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
ADDRESS $r0 l
# stack_depth=1, unused_regs=13, current_reg=1, used=false
CONST $r1 0
# stack_depth=2, unused_regs=12, current_reg=2, used=false
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false
display:
# stack_depth=0, unused_regs=14, current_reg=0, used=false
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false
ADDRESS $r0 a
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false
DECR $r14 1
ADD $r0 $r0 $r1
# stack_depth=1, unused_regs=13, current_reg=1, used=false
READ $r0 $r0
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false
ADDRESS $r0 l
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false
INCR $r1 1
# stack_depth=2, unused_regs=12, current_reg=2, used=false
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false
ADDRESS $r0 display
# stack_depth=1, unused_regs=13, current_reg=1, used=false
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false
CONST $r2 25
# stack_depth=3, unused_regs=11, current_reg=3, used=false
DECR $r14 1
LE $r1 $r1 $r2
# stack_depth=2, unused_regs=12, current_reg=2, used=false
JUMP $r0 WHEN $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
CONST $r0 97
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false
EXIT
# stack_depth=0, unused_regs=14, current_reg=0, used=false
l:
25
a:
0
stack_pointer:
65535