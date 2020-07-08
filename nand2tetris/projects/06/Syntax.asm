// OK
@0
@32767
// @LABEL1
// @symbol2
// @$:._
D=0
0;JMP
AMD=D|M

// NG
// @-1
// @32768
// @1LABEL
// @2symbol
// @!"#%&'()-^\@[;],/\=~|`{+*}>?
// D=0;
// =0;JMP
// DAM=M|D
// 0
// 0;JMPoo
