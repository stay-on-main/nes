const PPUCTRL: u16 = 0x2000;
const PPUMASK: u16 = 0x2001;
const PPUSTATUS: u16 = 0x2002;
const OAMADDR: u16 = 0x2003;
const OAMDATA: u16 = 0x2004;
const PPUSCROLL: u16 = 0x2005;
const PPUADDR: u16 = 0x2006;
const PPUDATA: u16 = 0x2007;

pub struct Ppu {
    pub scanline: i32,
    pub status_reg: u8,
    pub cycle: usize,
    ctrl_reg: u8,
    mask_reg: u8,
    vram_addr: u16,
    vram: [u8; 2048],
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            scanline: 0,
            status_reg: 0,
            cycle: 0,
            ctrl_reg: 0,
            mask_reg: 0,
            vram_addr: 0,
            vram: [0u8; 2048],
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        assert!(addr >= 0x2000);
        assert!(addr <= 0x3FFF);

        let addr = 0x2000 | (addr & 0b111);

        match addr {
            PPUCTRL => {
                self.ctrl_reg = val;
            },
            PPUMASK => {
                self.mask_reg = val;
            },
            OAMADDR => {
                todo!();
            },
            OAMDATA => {
                todo!();
            },
            PPUSCROLL => {
                //todo!();
            },
            PPUADDR => {
                if self.vram_addr == 0 {
                    self.vram_addr |= (val as u16) << 8;
                } else {
                    self.vram_addr |= val as u16;
                }
            },
            PPUDATA => {
                println!("Write: 0x{:02x} to VRAM: 0x{:02x}", val, self.vram_addr);
                self.vram[self.vram_addr as usize - 0x2000] = val;

                if self.ctrl_reg & (1 << 2) == 0 {
                    self.vram_addr += 1;
                } else {
                    self.vram_addr += 32;
                }
            },
            _ => {
                println!("Write: 0x{:02x} to Video 0x{:04x}", val, addr);
                todo!();
            }
        }
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        assert!(addr >= 0x2000);
        assert!(addr <= 0x3FFF);
        let addr = 0x2000 | (addr & 0b111);

        match addr {
            PPUCTRL => {
                todo!();
            },
            PPUMASK => {
                todo!();
            },
            PPUSTATUS => {
                let val = self.status_reg;
                // clear vblank bit
                self.status_reg &= !(1 << 7);
                // reset vram_addr
                self.vram_addr = 0;
                println!("Clear vram_addr");
                val
            },
            OAMADDR => {
                todo!();
            },
            OAMDATA => {
                todo!();
            },
            PPUSCROLL => {
                todo!();
            },
            PPUADDR => {
                todo!();
            },
            PPUDATA => {
                todo!();
            },
            _ => {
                todo!();
            }
        }
    }

    pub fn clock(&mut self) {
        self.cycle += 1;

        if self.cycle == 341 {
            self.cycle = 0;

            self.scanline += 1;

            if self.scanline == 241 {
                self.status_reg |= 1 << 7;
            }

            if self.scanline == 260 {
                self.scanline = -1;

                self.status_reg &= !(1 << 7);
            }
        }
    }
}