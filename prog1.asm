ADDRESS $r15 stack_pointer
READ $r14 $r15
loop:
DECR $r14 1
ADDRESS $r0 c
WRITE $r14 $r0
WRITE $r15 $r14
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
READ $r1 $r0
DECR $r14 1
WRITE $r14 $r1
WRITE $r15 $r14
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
PRINT $r0
DECR $r14 1
ADDRESS $r0 c
WRITE $r14 $r0
WRITE $r15 $r14
DECR $r14 1
ADDRESS $r0 c
WRITE $r14 $r0
WRITE $r15 $r14
DECR $r14 1
ADDRESS $r0 c
WRITE $r14 $r0
WRITE $r15 $r14
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
READ $r1 $r0
DECR $r14 1
WRITE $r14 $r1
WRITE $r15 $r14
DECR $r14 1
CONST $r0 1
WRITE $r14 $r0
WRITE $r15 $r14
READ $r1 $r14
INCR $r14 1
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
ADD $r0 $r0 $r1
DECR $r14 1
WRITE $r14 $r0
WRITE $r15 $r14
# - THE write - begin ------------------------------------
READ $r1 $r14
INCR $r14 1
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
WRITE $r0 $r1
# - THE write - end --------------------------------------
DECR $r14 1
ADDRESS $r0 loop
WRITE $r14 $r0
WRITE $r15 $r14
DECR $r14 1
ADDRESS $r0 c
WRITE $r14 $r0
WRITE $r15 $r14
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
READ $r1 $r0
DECR $r14 1
WRITE $r14 $r1
WRITE $r15 $r14
DECR $r14 1
CONST $r0 122
WRITE $r14 $r0
WRITE $r15 $r14
READ $r1 $r14
INCR $r14 1
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
LE $r0 $r0 $r1
DECR $r14 1
WRITE $r14 $r0
WRITE $r15 $r14
READ $r1 $r14
INCR $r14 1
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
JUMP $r0 WHEN $r1
DECR $r14 1
CONST $r0 10
WRITE $r14 $r0
WRITE $r15 $r14
READ $r0 $r14
INCR $r14 1
WRITE $r15 $r14
PRINT $r0
EXIT
c:
97
stack_pointer:
65536