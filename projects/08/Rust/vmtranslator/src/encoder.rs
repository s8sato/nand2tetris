use crate::Arithmetic;
use crate::Command;
use crate::Segment;

#[derive(Default)]
pub struct Encoder {
    pub file_stem: String,
    pub function: String,
    counter: Counter,
}

#[derive(Default)]
struct Counter {
    comparison: i32,
    call: i32,
}

impl Encoder {
    pub fn bootstrap(&mut self) -> String {
        // *SP = 256
        [ "@256"
        , "D=A"
        , "@SP"
        , "M=D"
        // call Sys.init
        , &*self.exec(Command::Call { f: "Sys.init".into(), n_args: 0 })
        ].join("\n")
    }
    pub fn exec(&mut self, cmd: Command) -> String {
        let at_const = |i: i32| {
            ["@", &*i.to_string()].join("")
        };
        let at = |s: &str| {
            ["@", s].join("")
        };
        let label = |s: &str| {
            ["(", s, ")"].join("")
        };
        let push_d =
            [ "@SP"
            , "M=M+1"
            , "A=M-1"
            , "M=D"
            ].join("\n");
        let pop_d =
            [ "@SP"
            , "AM=M-1"
            , "D=M"
            ].join("\n");
        match cmd {
            Command::Arithmetic(x) => {
                let unary = |suffix: &str| {
                    [ "@SP"
                    , "A=M-1"
                    , suffix
                    ].join("\n")
                };
                let binary = |suffix: &str| {
                    [ "@SP"
                    , "AM=M-1"
                    , "D=M"
                    , "A=A-1"
                    , suffix
                    ].join("\n")
                };
                let mut comparison = |op: &str| {
                    let _true = ["CMP", &*self.counter.comparison.to_string(), "_TRUE"].join("");
                    let _end = ["CMP", &*self.counter.comparison.to_string(), "_END"].join("");
                    self.counter.comparison += 1;
                    let suffix = 
                        [ "D=M-D"
                        , &*at(&*_true)
                        , &*["D;J", op].join("")
                        , "@SP"
                        , "A=M-1"
                        , "M=0"
                        , &*at(&*_end)
                        , "0;JMP"
                        , &*label(&*_true)
                        , "@SP"
                        , "A=M-1"
                        , "M=-1"
                        , &*label(&*_end)
                        ].join("\n");
                    binary(&*suffix)
                };
                match x {
                    Arithmetic::Add => binary("M=D+M"),
                    Arithmetic::Sub => binary("M=M-D"),
                    Arithmetic::Neg => unary("M=-M"),
                    Arithmetic::Eq  => comparison("EQ"),
                    Arithmetic::Gt  => comparison("GT"),
                    Arithmetic::Lt  => comparison("LT"),
                    Arithmetic::And => binary("M=D&M"),
                    Arithmetic::Or  => binary("M=D|M"),
                    Arithmetic::Not => unary("M=!M"),
                }
            },
            Command::Push { segment, idx } => {
                let push_seg = |seg: &str, idx: i32| {
                    [ &*at(seg)
                    , "D=M"
                    , &*at_const(idx)
                    , "A=D+A"
                    , "D=M"
                    , &*push_d
                    ].join("\n")
                };
                let push_seg_at = |base: i32, idx: i32| {
                    [ &*at_const(base)
                    , "D=A"
                    , &*at_const(idx)
                    , "A=D+A"
                    , "D=M"
                    , &*push_d
                    ].join("\n")
                };
                match segment {
                    Segment::Constant =>
                        [ &*at_const(idx)
                        , "D=A"
                        , &*push_d
                        ].join("\n"),
                    Segment::Local => push_seg("LCL", idx),
                    Segment::Argument => push_seg("ARG", idx),
                    Segment::This => push_seg("THIS", idx),
                    Segment::That => push_seg("THAT", idx),
                    Segment::Pointer => push_seg_at(3, idx),
                    Segment::Temp => push_seg_at(5, idx),
                    Segment::Static =>
                        [ &*["@", &*self.file_stem, ".", &*idx.to_string()].join("")
                        , "D=M"
                        , &*push_d
                        ].join("\n"),
                }
            },
            Command::Pop { segment, idx } => {
                let d_to_seg =
                    [ "@R13"
                    , "A=M"
                    , "M=D"
                    ].join("\n");
                let pop_seg = |seg: &str, idx: i32| {
                    [ &*at(seg)
                    , "D=M"
                    , &*at_const(idx)
                    , "D=D+A"
                    , "@R13"
                    , "M=D"
                    , &*pop_d
                    , &*d_to_seg
                    ].join("\n")
                };
                let pop_seg_at = |base: i32, idx: i32| {
                    [ &*at_const(base)
                    , "D=A"
                    , &*at_const(idx)
                    , "D=D+A"
                    , "@R13"
                    , "M=D"
                    , &*pop_d
                    , &*d_to_seg
                    ].join("\n")
                };
                match segment {
                    Segment::Constant => pop_d,
                    Segment::Local => pop_seg("LCL", idx),
                    Segment::Argument => pop_seg("ARG", idx),
                    Segment::This => pop_seg("THIS", idx),
                    Segment::That => pop_seg("THAT", idx),
                    Segment::Pointer => pop_seg_at(3, idx),
                    Segment::Temp => pop_seg_at(5, idx),
                    Segment::Static =>
                        [ &*pop_d
                        , &*["@", &*self.file_stem, ".", &*idx.to_string()].join("")
                        , "M=D"
                        ].join("\n"),
                }
            },
            Command::Label(symbol) => {
                label(&*[&*self.function, "$", &*symbol].join(""))
            },
            Command::Goto(symbol) => {
                [ &*["@", &*self.function, "$", &*symbol].join("")
                , "0;JMP"
                ].join("\n")
            },
            Command::IfGoto(symbol) => {
                [ &*pop_d
                , &*["@", &*self.function, "$", &*symbol].join("")
                , "D;JNE"
                ].join("\n")
            },
            Command::Function { f, n_locals } => {
                self.function = f.clone();
                [ &*label(&*f)
                , &*at_const(n_locals)
                , "D=A"
                , &*label(&*[&*f, "$", "0_LOOP"].join(""))
                , "D=D-1"
                , &*["@", &*f, "$", "0_END"].join("")
                , "D;JLT"
                // push 0
                , "@SP"
                , "M=M+1"
                , "A=M-1"
                , "M=0"
                //
                , &*["@", &*f, "$", "0_LOOP"].join("")
                , "0;JMP"
                , &*label(&*[&*f, "$", "0_END"].join(""))
                ].join("\n")
            },
            Command::Call { f, n_args } => {
                let ret = ["RET", &*self.counter.call.to_string()].join("");
                self.counter.call += 1;
                let push = |s: &str| {
                    [ &*at(s)
                    , "D=M"
                    , &*push_d
                    ].join("\n")
                };
                // push return-address
                [ &*at(&*ret)
                , "D=A"
                , &*push_d
                //
                , &*push("LCL")
                , &*push("ARG")
                , &*push("THIS")
                , &*push("THAT")
                // *ARG = *SP-n-5
                , "@SP"
                , "D=M"
                , &*at_const(n_args)
                , "D=D-A"
                , "@5"
                , "D=D-A"
                , "@ARG"
                , "M=D"
                // *LCL = *SP
                , "@SP"
                , "D=M"
                , "@LCL"
                , "M=D"
                // goto f
                , &*at(&*f)
                , "0;JMP"
                // (return-address)
                , &*label(&*ret)
                ].join("\n")
            },
            Command::Return => {
                let restore = |s: &str, idx: i32| {
                    [ "@R13"
                    , "D=M"
                    , &*at_const(idx)
                    , "A=D-A"
                    , "D=M"
                    , &*at(s)
                    , "M=D"
                    ].join("\n")
                };
                // FRAME = *LCL
                [ "@LCL"
                , "D=M"
                , "@R13"
                , "M=D"
                // RET = *(FRAME-5)
                , "@5"
                , "A=D-A"
                , "D=M"
                , "@R14"
                , "M=D"
                // **ARG = pop()
                , &*pop_d
                , "@ARG"
                , "A=M"
                , "M=D"
                // *SP = *ARG+1
                , "@ARG"
                , "D=M"
                , "@SP"
                , "M=D+1"
                //
                , &*restore("THAT", 1)
                , &*restore("THIS", 2)
                , &*restore("ARG", 3)
                , &*restore("LCL", 4)
                // goto *RET
                , "@R14"
                , "A=M"
                , "0;JMP"
                ].join("\n")
            },
        }
    }
}
