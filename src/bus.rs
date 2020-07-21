use super::nes::{Rom};
use super::ppu::{Ppu};

pub struct Bus {
    ram: [u8; 2048],
    rom: Rom,
    pub clk: u32,
    ppu: Ppu,
}

impl Bus {
    pub fn read(&mut self, addr: u16) -> u8 {
        self.clk += 1;

        if addr <= 0x1FFF {
            // RAM
            let addr = addr & 0x7FF;
            self.ram[addr as usize]
        }
        else if addr <= 0x3FFF {
            // Registers Video
            self.ppu.read(addr)
        }
        else if addr <= 0x4017 {
            // Registers Audio & DMA & I/O
            todo!();
        }
        else if addr <= 0x4FFF {
            // Not used
            todo!();
        }
        else if addr <= 0x5FFF {
            // Expansion ROM\RAM (etc. in MMC5)
            todo!();
        }
        else if addr <= 0x7FFF {
            // SRAM (aka WRAM) (etc. in MMC3)
            todo!();
        }
        else {
            // PRG-ROM)
            self.rom.read(addr)
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        self.clk += 1;

        if addr <= 0x1FFF {
            // RAM
            let addr = addr & 0x7FF;
            self.ram[addr as usize] = val;
        } else if addr <= 0x3FFF{
            //println!("write video ram");
        } else if addr <= 0x4017 {
            // Registers Audio & DMA & I/O
            //println!("write audio ram");
        }
        else if addr <= 0x4FFF {
            // Not used
            todo!();
        }
        else if addr <= 0x5FFF {
            // Expansion ROM\RAM (etc. in MMC5)
            todo!();
        }
        else if addr <= 0x7FFF {
            // SRAM (aka WRAM) (etc. in MMC3)
            todo!();
        }
        else {
            // PRG-ROM)
            todo!();
        }
    }

    pub fn clock(&mut self) {
        self.clk += 1;

        for _ in 0..3 {
            self.ppu.clock();
        }
    }

    pub fn new() -> Self {
        Self {
            rom: Rom::new("C:/github/nes/Super_Mario_Bros_(E).nes"),
            ram: [0u8; 2048],
            clk: 7,
            ppu: Ppu::new(),
        }
    }
}
