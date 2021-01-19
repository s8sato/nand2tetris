use crate::Command;

impl Command {
    pub fn encode(self) -> String {
        self.raw.repeat(2)
    }
}
