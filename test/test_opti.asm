CONST $r15 65535
# push 97
CONST $r14 97
DECR $r15 1
# PRINT
PRINT $r14
# push 10
CONST $r14 10
# PRINT
PRINT $r14
INCR $r15 1
READ $r14 $r15
# loop:
loop:
# push 97
WRITE $r15 $r14
CONST $r14 97
DECR $r15 1
# push 'n'
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
# READ
READ $r14 $r14
# push 48
WRITE $r15 $r14
CONST $r14 48
DECR $r15 1
# ADD
INCR $r15 1
READ $r0 $r15
ADD $r14 $r0 $r14
# PRINT
PRINT $r14
# push 'n'
ADDRESS $r14 n
# push 'n'
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
# READ
READ $r14 $r14
# push 1
WRITE $r15 $r14
CONST $r14 1
DECR $r15 1
# ADD
INCR $r15 1
READ $r0 $r15
ADD $r14 $r0 $r14
# WRITE
INCR $r15 1
READ $r0 $r15
WRITE $r0 $r14
# push 'loop'
ADDRESS $r14 loop
# push 'n'
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
# READ
READ $r14 $r14
# push 10
WRITE $r15 $r14
CONST $r14 10
DECR $r15 1
# LT
INCR $r15 1
READ $r0 $r15
LT $r14 $r0 $r14
# JUMPWHEN
MOVE $r1 $r14
INCR $r15 1
READ $r0 $r15
INCR $r15 1
READ $r14 $r15
JUMP $r0 WHEN $r1
# push 10
WRITE $r15 $r14
CONST $r14 10
DECR $r15 1
# PRINT
PRINT $r14
INCR $r15 1
READ $r14 $r15
# clear:
clear:
# PRINT
PRINT $r14
# push 'n'
ADDRESS $r14 n
# push 'n'
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
# READ
READ $r14 $r14
# push 1
WRITE $r15 $r14
CONST $r14 1
DECR $r15 1
# SUB
INCR $r15 1
READ $r0 $r15
SUB $r14 $r0 $r14
# WRITE
INCR $r15 1
READ $r0 $r15
WRITE $r0 $r14
# push 'clear'
ADDRESS $r14 clear
# push 'n'
WRITE $r15 $r14
ADDRESS $r14 n
DECR $r15 1
# READ
READ $r14 $r14
# push 0
WRITE $r15 $r14
CONST $r14 0
DECR $r15 1
# GT
INCR $r15 1
READ $r0 $r15
GT $r14 $r0 $r14
# JUMPWHEN
MOVE $r1 $r14
INCR $r15 1
READ $r0 $r15
INCR $r15 1
READ $r14 $r15
JUMP $r0 WHEN $r1
# push 10
WRITE $r15 $r14
CONST $r14 10
DECR $r15 1
# PRINT
PRINT $r14
INCR $r15 1
READ $r14 $r15
# EXIT
EXIT
n:
0
