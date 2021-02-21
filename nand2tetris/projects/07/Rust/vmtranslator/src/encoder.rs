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
                match segment {
                    Segment::Constant =>
                        [ &*at_const(idx)
                        , "D=A"
                        , "@SP"
                        , "A=M"
                        , "M=D"
                        , "@SP"
                        , "M=M+1"
                        ].join("\n"),
                    _ => unreachable!(),
                    // Segment::Local => ,
                    // Segment::Argument => ,
                    // Segment::This => ,
                    // Segment::That => ,
                    // Segment::Pointer => ,
                    // Segment::Temp => ,
                    // Segment::Static => ,
                }
            },
            _ => unreachable!(),
            // Command::Pop { segment, idx } => ,
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
