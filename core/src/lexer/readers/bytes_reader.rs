use std::io::BufRead;
use std::io::Read;
use std::io::Result;
use std::io::Seek;

/// Provides a wrapper for strings so that they can be consumed via the `std::io::Read` and `std::io::Seek` traits.
pub struct BytesReader<'a> {
    data: &'a [u8],
    pos: usize,
    seek_pos: usize,
}

impl<'a> BytesReader<'a> {
    /// Wrap a string in a `StringReader`, which implements `std::io::Read`.
    pub fn new(data: &'a [u8]) -> Self {
        Self {
            data,
            pos: 0,
            seek_pos: 0,
        }
    }
}

impl<'a> Read for BytesReader<'a> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let len = std::cmp::min(buf.len(), self.data.len() - self.pos);
        buf[..len].copy_from_slice(&self.data[self.pos..self.pos + len]);
        self.pos += len;
        Ok(len)
    }
}

impl<'a> BufRead for BytesReader<'a> {
    fn fill_buf(&mut self) -> Result<&[u8]> {
        Ok(&self.data[self.pos..])
    }

    fn consume(&mut self, amt: usize) {
        self.pos = std::cmp::min(self.pos + amt, self.data.len());
    }
}

impl<'a> Seek for BytesReader<'a> {
    fn seek(&mut self, pos: std::io::SeekFrom) -> Result<u64> {
        match pos {
            std::io::SeekFrom::Start(pos) => {
                self.seek_pos = pos as usize;
            }
            std::io::SeekFrom::End(pos) => {
                self.seek_pos = (self.data.len() as i64 - pos) as usize;
            }
            std::io::SeekFrom::Current(pos) => {
                self.seek_pos = (self.seek_pos as i64 + pos) as usize;
            }
        }
        // Check if the seek position is valid.
        if self.seek_pos > self.data.len() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "invalid seek to a negative or overflowing position",
            ));
        }
        Ok(self.seek_pos as u64)
    }
}

impl<'a> From<&'a String> for BytesReader<'a> {
    fn from(data: &'a String) -> Self {
        Self::new(data.as_bytes())
    }
}

impl<'a> From<&'a str> for BytesReader<'a> {
    fn from(data: &'a str) -> Self {
        Self::new(data.as_bytes())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::BufReader;

    #[test]
    fn test() {
        let data = "abc\ndef";
        let mut reader = BufReader::new(BytesReader::from(data));
        let mut buffer = String::new();

        buffer.clear();
        reader.read_line(&mut buffer).unwrap();
        assert_eq!("abc\n", buffer);

        buffer.clear();
        reader.read_line(&mut buffer).unwrap();
        assert_eq!("def", buffer);
    }
}
