use std::str::FromStr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Xlen {
    Rv32,
    Rv64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Target {
    pub xlen: Xlen,
    pub m: bool,
    pub f: bool,
    pub zifencei: bool,
}

impl Default for Target {
    fn default() -> Target {
        Target {
            xlen: Xlen::Rv32,
            m: false,
            f: false,
            zifencei: false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseTargetError {
    Prefix,
    Xlen,
    UnknownExt,
}

impl FromStr for Target {
    type Err = ParseTargetError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.to_ascii_lowercase();
        let rest = s
            .strip_prefix("rv")
            .or_else(|| s.strip_prefix("riscv"))
            .ok_or(ParseTargetError::Prefix)?;

        let xlen = match &rest.get(..2) {
            Some("32") => Xlen::Rv32,
            Some("64") => Xlen::Rv64,
            _ => return Err(ParseTargetError::Xlen),
        };
        let rest = &rest[2..];

        let mut target = Target {
            xlen,
            ..Default::default()
        };

        let mut extensions = rest;
        while !extensions.is_empty() {
            let (ext, rest) = extension(extensions);
            extensions = rest;

            match ext {
                "i" => (),
                "m" => target.m = true,
                "f" => target.f = true,
                "zifencei" => target.zifencei = true,
                _ => return Err(ParseTargetError::UnknownExt),
            }
        }

        Ok(target)
    }
}

fn extension(s: &str) -> (&str, &str) {
    let len = if s.starts_with('z')
        || s.starts_with('x')
        || s.starts_with("sv")
        || s.starts_with("ss")
        || s.starts_with("sh")
        || s.starts_with("sm")
    {
        s.find('_').unwrap_or(s.len())
    } else {
        1
    };

    let mut next_index = len;
    if s[len..].starts_with('_') {
        next_index += 1;
    }
    let rest = &s[next_index..];

    (&s[..len], rest)
}
