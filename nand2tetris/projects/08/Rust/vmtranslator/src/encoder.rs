use crate::Arithmetic;
use crate::Command;
use crate::Segment;

pub struct Encoder {
    pub basename: String,
    pub counter: i32,
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
                    let res =
                        [ "D=M-D"
                        , &*self.at_label("TRUE")
                        , &*["D;J", op].join("")
                        , "@SP"
                        , "A=M-1"
                        , "M=0"
                        , &*self.at_label("END")
                        , "0;JMP"
                        , &*self.label("TRUE")
                        , "@SP"
                        , "A=M-1"
                        , "M=-1"
                        , &*self.label("END")
                        ].join("\n");
                    self.counter += 1;
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
                    , "A=M"
                    , "M=D"
                    , "@SP"
                    , "M=M+1"].join("\n");
                let push_seg = | seg: &str, idx: i32| {
                    [ &*at_const(idx)
                    , "D=A"
                    , &*["@", seg].join("")
                    , "A=D+M"
                    , "D=M"
                    , &*push_d
                    ].join("\n")
                };
                let push_seg_at = | at: i32, idx: i32| {
                    [ &*at_const(idx)
                    , "D=A"
                    , &*at_const(at)
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
                        [ &*["@", &*self.basename, ".", &*idx.to_string()].join("")
                        , "D=M"
                        , &*push_d
                        ].join("\n"),
                }
            },
            Command::Pop { segment, idx } => {
                let pop_d =
                    [ "@SP"
                    , "AM=M-1"
                    , "D=M"].join("\n");
                let d_to_seg =
                    [ "@R13"
                    , "A=M"
                    , "M=D"].join("\n");
                let pop_seg = | seg: &str, idx: i32| {
                    [ &*at_const(idx)
                    , "D=A"
                    , &*["@", seg].join("")
                    , "D=D+M"
                    , "@R13"
                    , "M=D"
                    , &*pop_d
                    , &*d_to_seg
                    ].join("\n")
                };
                let pop_seg_at = | at: i32, idx: i32| {
                    [ &*at_const(idx)
                    , "D=A"
                    , &*at_const(at)
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
                        , &*["@", &*self.basename, ".", &*idx.to_string()].join("")
                        , "M=D"
                        ].join("\n"),
                }
            },
            _ => unreachable!(),
            // Command::Label(symbol) => ,
            // Command::Goto(symbol) => ,
            // Command::If(symbol) => ,
            // Command::Function { symbol, num } => ,
            // Command::Call { symbol, num } => ,
            // Command::Return => ,
        }
    }
    fn symbol(&self, s: &str) -> String {
        [s, &*self.basename, &*self.counter.to_string()].join(".")
    }
    fn at_label(&self, s: &str) -> String {
        ["@", &*self.symbol(s)].join("")
    }
    fn label(&self, s: &str) -> String {
        ["(", &*self.symbol(s), ")"].join("")
    }
}

fn at_const(addr: i32) -> String {
    ["@", &*addr.to_string()].join("")
}
