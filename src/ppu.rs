const PPUCTRL_REG: u16 = 0x2000;
const PPUMASK_REG: u16 = 0x2001;
const PPUSTATUS: u16 = 0x2002;
const OAMADDR: u16 = 0x2003;
const OAMDATA: u16 = 0x2004;
const PPUSCROLL: u16 = 0x2005;
const PPUADDR: u16 = 0x2006;
const PPUDATA: u16 = 0x2007;

pub struct Ppu {

}

impl Ppu {
    pub fn new() -> Self {
        todo!();
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        assert!(addr >= 0x2000);
        assert!(addr <= 0x3FFF);

        let addr = 0x2000 + addr & 0b111;
        todo!();
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        assert!(addr >= 0x2000);
        assert!(addr <= 0x3FFF);

        match 0x2000 + (addr & 0b111) {
            PPUCTRL => {},
            PPUMASK => {},
            PPUSTATUS => {},
            OAMADDR => {},
            OAMDATA => {},
            PPUSCROLL => {},
            PPUADDR => {},
            PPUDATA => {},
        }

        42
    }

    pub fn clock(&mut self) {
        todo!();
    }
}