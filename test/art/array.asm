ADDRESS $r15 stack_pointer
READ $r14 $r15

# fill:
fill:
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# NOP
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 'a'
ADDRESS $r0 a
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# DIRECTREAD 'l'
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# ADD
ADD $r0 $r0 $r1
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# DIRECTREAD 'l'
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# INCR 97
INCR $r1 97
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# WRITE
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# NOP
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 'l'
ADDRESS $r0 l
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# DIRECTREAD 'l'
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# DECR 1
DECR $r1 1
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# WRITE
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 97
CONST $r0 97
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# PRINT
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 'fill'
ADDRESS $r0 fill
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# DIRECTREAD 'l'
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# push 0
CONST $r2 0
# stack_depth=3, unused_regs=11, current_reg=3, used=false

# GE
GE $r1 $r1 $r2
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# JUMPWHEN
JUMP $r0 WHEN $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 10
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# PRINT
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 'l'
ADDRESS $r0 l
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# push 0
CONST $r1 0
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# WRITE
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# display:
display:
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# NOP
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 'a'
ADDRESS $r0 a
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# DIRECTREAD 'l'
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# ADD
ADD $r0 $r0 $r1
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# READ
READ $r0 $r0
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# PRINT
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# NOP
NOP
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 'l'
ADDRESS $r0 l
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# DIRECTREAD 'l'
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# INCR 1
INCR $r1 1
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# WRITE
WRITE $r0 $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 'display'
ADDRESS $r0 display
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# DIRECTREAD 'l'
DIRECTREAD $r1 l
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# push 25
CONST $r2 25
# stack_depth=3, unused_regs=11, current_reg=3, used=false

# LE
LE $r1 $r1 $r2
# stack_depth=2, unused_regs=12, current_reg=2, used=false

# JUMPWHEN
JUMP $r0 WHEN $r1
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 10
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# PRINT
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 97
CONST $r0 97
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# PRINT
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# push 10
CONST $r0 10
# stack_depth=1, unused_regs=13, current_reg=1, used=false

# PRINT
PRINT $r0
# stack_depth=0, unused_regs=14, current_reg=0, used=false

# EXIT
EXIT
# stack_depth=0, unused_regs=14, current_reg=0, used=false
l:
25
a:
0
stack_pointer:
65535