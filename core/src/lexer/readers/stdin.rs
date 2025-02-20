use std::io::Read;

pub struct StdinReader {
    stdin: std::io::Stdin,
    buffer: String,
}

impl Default for StdinReader {
    fn default() -> Self {
        Self {
            stdin: std::io::stdin(),
            buffer: String::new(),
        }
    }
}

impl Read for StdinReader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        while self.buffer.len() < buf.len() {
            self.stdin.read_line(&mut self.buffer)?;
        }
        buf.copy_from_slice(self.buffer.as_bytes()[..buf.len()].as_ref());
        self.buffer = self.buffer[buf.len()..].to_string(); // remove the read bytes
        Ok(buf.len())
    }
}
