CONST $r15 65535
salut:
CONST $r14 83
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 84
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 79
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 80
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 32
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 84
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 72
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 69
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 32
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 80
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 82
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 79
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 71
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 82
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 65
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 77
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
WRITE $r15 $r14
CONST $r14 0
DECR $r15 1
INCR $r15 1
READ $r0 $r15
INCR $r15 1
WRITE $r0 $r14
READ $r14 $r15
loop:
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
READ $r14 $r14
WRITE $r15 $r14
CONST $r14 48
DECR $r15 1
INCR $r15 1
READ $r0 $r15
ADD $r14 $r0 $r14
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 33
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
NOP
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
READ $r14 $r14
WRITE $r15 $r14
CONST $r14 1
DECR $r15 1
INCR $r15 1
READ $r0 $r15
ADD $r14 $r0 $r14
INCR $r15 1
READ $r0 $r15
INCR $r15 1
WRITE $r0 $r14
READ $r14 $r15
NOP
NOP
WRITE $r15 $r14
ADDRESS $r14 loop
DECR $r15 1
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
READ $r14 $r14
WRITE $r15 $r14
CONST $r14 10
DECR $r15 1
INCR $r15 1
READ $r0 $r15
LT $r14 $r0 $r14
MOVE $r1 $r14
INCR $r15 1
READ $r0 $r15
INCR $r15 1
READ $r14 $r15
JUMP $r0 WHEN $r1
NOP
WRITE $r15 $r14
CONST $r14 10
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 97
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
CONST $r14 10
DECR $r15 1
PRINT $r14
INCR $r15 1
READ $r14 $r15
WRITE $r15 $r14
ADDRESS $r14 salut
DECR $r15 1
MOVE $r0 $r14
INCR $r15 1
READ $r14 $r15
JUMP $r0
EXIT
n:
0
