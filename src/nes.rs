use std::fs::File;
use std::io::prelude::*;
use std::io::SeekFrom;

pub struct Rom {
    prg_rom: [u8; 32768],
    //prg_rom_0: [u8; 16384],
    //chr_rom: [u8; 8192],
}

impl Rom {
    pub fn new(file: &str) -> Self {
        println!("Load nes file: {}", file);
        let mut file = File::open(file).unwrap();
        let mut header = [0u8; 16];
        file.seek(SeekFrom::Start(0)).unwrap();
        file.read(&mut header).unwrap();
        let prg_rom_size_16k = header[4];

        let mut prg_rom = [0u8; 32768];

        match prg_rom_size_16k {
            1 => {
                file.read(&mut prg_rom[(32768 / 2)..]).unwrap();
            },
            2 => {
                file.read(&mut prg_rom).unwrap();
            },
            _ => todo!(),
        }
        //println!("{:x}", prg_rom[0xfffc-0x8000]);
        Self {
            prg_rom
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        let addr = addr - 0x8000;
        //println!("ROM read: 0x{:x}", self.prg_rom[addr as usize]);
        self.prg_rom[addr as usize]
    }
}