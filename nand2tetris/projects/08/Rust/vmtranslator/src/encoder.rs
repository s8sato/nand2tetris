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
    // call: i32,
}

impl Encoder {
    pub fn exec(&mut self, cmd: Command) -> String {
        match cmd {
            Command::Arithmetic(x) => {
                let pre_binary_op =
                    [ "@SP"
                    , "AM=M-1"
                    , "D=M"
                    , "A=A-1"
                    ].join("\n");
                let pre_unary_op =
                    [ "@SP"
                    , "A=M-1"
                    ].join("\n");
                let mut comparison = |op: &str| {
                    let _true = ["CMP", &*self.counter.comparison.to_string(), "_TRUE"].join("");
                    let _end = ["CMP", &*self.counter.comparison.to_string(), "_END"].join("");
                    let res =
                        [ "D=M-D"
                        , &*["@", &*_true].join("")
                        , &*["D;J", op].join("")
                        , "@SP"
                        , "A=M-1"
                        , "M=0"
                        , &*["@", &*_end].join("")
                        , "0;JMP"
                        , &*label(&*_true)
                        , "@SP"
                        , "A=M-1"
                        , "M=-1"
                        , &*label(&*_end)
                        ].join("\n");
                    self.counter.comparison += 1;
                    res
                };
                match x {
                    Arithmetic::Add => [&*pre_binary_op, "M=D+M"].join("\n"),
                    Arithmetic::Sub => [&*pre_binary_op, "M=M-D"].join("\n"),
                    Arithmetic::Neg => [&*pre_unary_op,  "M=-M"] .join("\n"),
                    Arithmetic::Eq  => [&*pre_binary_op, &*comparison("EQ")].join("\n"),
                    Arithmetic::Gt  => [&*pre_binary_op, &*comparison("GT")].join("\n"),
                    Arithmetic::Lt  => [&*pre_binary_op, &*comparison("LT")].join("\n"),
                    Arithmetic::And => [&*pre_binary_op, "M=D&M"].join("\n"),
                    Arithmetic::Or  => [&*pre_binary_op, "M=D|M"].join("\n"),
                    Arithmetic::Not => [&*pre_unary_op,  "M=!M"].join("\n"),
                }
            },
            Command::Push { segment, idx } => {
                let push_d =
                    [ "@SP"
                    , "AM=M+1"
                    , "A=A-1"
                    , "M=D"
                    ].join("\n");
                let push_seg = | seg: &str, idx: i32| {
                    [ &*["@", seg].join("")
                    , "D=M"
                    , &*at(idx)
                    , "A=D+A"
                    , "D=M"
                    , &*push_d
                    ].join("\n")
                };
                let push_seg_at = | base: i32, idx: i32| {
                    [ &*at(base)
                    , "D=A"
                    , &*at(idx)
                    , "A=D+A"
                    , "D=M"
                    , &*push_d
                    ].join("\n")
                };
                match segment {
                    Segment::Constant =>
                        [ &*at(idx)
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
                let pop_d =
                    [ "@SP"
                    , "AM=M-1"
                    , "D=M"
                    ].join("\n");
                let d_to_seg =
                    [ "@R13"
                    , "A=M"
                    , "M=D"
                    ].join("\n");
                let pop_seg = | seg: &str, idx: i32| {
                    [ &*["@", seg].join("")
                    , "D=M"
                    , &*at(idx)
                    , "D=D+A"
                    , "@R13"
                    , "M=D"
                    , &*pop_d
                    , &*d_to_seg
                    ].join("\n")
                };
                let pop_seg_at = | base: i32, idx: i32| {
                    [ &*at(base)
                    , "D=A"
                    , &*at(idx)
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
                [ "@SP"
                , "AM=M-1"
                , "D=M"
                , &*["@", &*self.function, "$", &*symbol].join("")
                , "D;JNE"
                ].join("\n")
            },
            _ => unreachable!(),
            // Command::Function { f, n_locals } => ,
            // Command::Call { f, n_args } => ,
            // Command::Return => ,
        }
    }
}

fn at(i: i32) -> String {
    ["@", &*i.to_string()].join("")
}

fn label(s: &str) -> String {
    ["(", s, ")"].join("")
}
