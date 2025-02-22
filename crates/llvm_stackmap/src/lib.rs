use std::io::Cursor;

use byteorder::{LittleEndian, ReadBytesExt};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Invalid stackmap version: {0}")]
    InvalidVersion(u8),
    #[error("Invalid reserved value")]
    InvalidReserved,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Header {
    pub version: u8,
}

#[derive(Debug)]
pub struct StkSizeRecord {
    pub function_address: u64,
    pub stack_size: u64,
    pub record_count: u64,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub kind: LocationKind,
    pub size: u16,
    pub dwarf_reg_num: u16,
    pub offset_or_small_constant: i32,
}

#[derive(Debug, Clone)]
pub enum LocationKind {
    Register,
    Direct,
    Indirect,
    Constant,
    ConstantIndex,
}

impl LocationKind {
    fn from_u8(value: u8) -> Option<Self> {
        match value {
            1 => Some(LocationKind::Register),
            2 => Some(LocationKind::Direct),
            3 => Some(LocationKind::Indirect),
            4 => Some(LocationKind::Constant),
            5 => Some(LocationKind::ConstantIndex),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct LiveOut {
    pub dwarf_reg_num: u16,
    pub size_in_bytes: u8,
}

#[derive(Debug)]
pub struct Record {
    pub patchpoint_id: u64,
    pub instruction_offset: u32,
    pub locations: Vec<Location>,
    pub live_outs: Vec<LiveOut>,
}

#[derive(Debug)]
pub struct StackMap {
    pub header: Header,
    pub function_records: Vec<StkSizeRecord>,
    pub constants: Vec<u64>,
    pub records: Vec<Record>,
}

impl StackMap {
    pub fn parse(data: Vec<u8>) -> Result<Self> {
        let mut cursor = Cursor::new(data);

        // Parse header
        let version = cursor.read_u8()?;
        if version != 3 {
            return Err(Error::InvalidVersion(version));
        }

        let reserved1 = cursor.read_u8()?;
        let reserved2 = cursor.read_u16::<LittleEndian>()?;
        if reserved1 != 0 || reserved2 != 0 {
            return Err(Error::InvalidReserved);
        }

        let header = Header { version };

        // Parse counts
        let num_functions = cursor.read_u32::<LittleEndian>()?;
        let num_constants = cursor.read_u32::<LittleEndian>()?;
        let num_records = cursor.read_u32::<LittleEndian>()?;

        // Parse function records
        let mut function_records = Vec::with_capacity(num_functions as usize);
        for _ in 0..num_functions {
            function_records.push(StkSizeRecord {
                function_address: cursor.read_u64::<LittleEndian>()?,
                stack_size: cursor.read_u64::<LittleEndian>()?,
                record_count: cursor.read_u64::<LittleEndian>()?,
            });
        }

        // Parse constants
        let mut constants = Vec::with_capacity(num_constants as usize);
        for _ in 0..num_constants {
            constants.push(cursor.read_u64::<LittleEndian>()?);
        }

        // Parse records
        let mut records = Vec::with_capacity(num_records as usize);
        for _ in 0..num_records {
            let patchpoint_id = cursor.read_u64::<LittleEndian>()?;
            let instruction_offset = cursor.read_u32::<LittleEndian>()?;
            let _reserved = cursor.read_u16::<LittleEndian>()?; // record flags
            let num_locations = cursor.read_u16::<LittleEndian>()?;

            // Parse locations
            let mut locations = Vec::with_capacity(num_locations as usize);
            for _ in 0..num_locations {
                let kind_byte = cursor.read_u8()?;
                let kind = LocationKind::from_u8(kind_byte).ok_or_else(|| {
                    std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!("Invalid location kind: {}", kind_byte),
                    )
                })?;

                let _reserved = cursor.read_u8()?;
                let size = cursor.read_u16::<LittleEndian>()?;
                let dwarf_reg_num = cursor.read_u16::<LittleEndian>()?;
                let _reserved = cursor.read_u16::<LittleEndian>()?;
                let offset_or_small_constant = cursor.read_i32::<LittleEndian>()?;

                locations.push(Location {
                    kind,
                    size,
                    dwarf_reg_num,
                    offset_or_small_constant,
                });
            }

            // Handle 8-byte alignment padding after locations
            if (num_locations * 12) % 8 != 0 {
                cursor.read_u32::<LittleEndian>()?;
            }

            let _padding = cursor.read_u16::<LittleEndian>()?;
            let num_live_outs = cursor.read_u16::<LittleEndian>()?;

            // Parse live-outs
            let mut live_outs = Vec::with_capacity(num_live_outs as usize);
            for _ in 0..num_live_outs {
                let dwarf_reg_num = cursor.read_u16::<LittleEndian>()?;
                let _reserved = cursor.read_u8()?;
                let size_in_bytes = cursor.read_u8()?;

                live_outs.push(LiveOut {
                    dwarf_reg_num,
                    size_in_bytes,
                });
            }

            // Handle 8-byte alignment padding after live-outs
            if (num_live_outs * 4) % 8 != 0 {
                cursor.read_u32::<LittleEndian>()?;
            }

            records.push(Record {
                patchpoint_id,
                instruction_offset,
                locations,
                live_outs,
            });
        }

        Ok(StackMap {
            header,
            function_records,
            constants,
            records,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_stackmap() {
        let data = vec![
            // Header
            3, 0, 0, 0, // Counts
            0, 0, 0, 0, // num_functions
            0, 0, 0, 0, // num_constants
            0, 0, 0, 0, // num_records
        ];

        let stackmap = StackMap::parse(data).unwrap();
        assert_eq!(stackmap.header.version, 3);
        assert!(stackmap.function_records.is_empty());
        assert!(stackmap.constants.is_empty());
        assert!(stackmap.records.is_empty());
    }

    #[test]
    fn test_invalid_version() {
        let data = vec![
            4, 0, 0, 0, // Invalid version
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ];

        match StackMap::parse(data) {
            Err(Error::InvalidVersion(4)) => (),
            _ => panic!("Expected InvalidVersion error"),
        }
    }
}
