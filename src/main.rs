pub mod bus;
use bus::{Bus};

mod nes;
mod cpu;
use cpu::Cpu;

pub mod ppu;

fn main() {
    let bus = Bus::new();
    let mut cpu = Cpu::new(bus);

    for _ in 0..8991 {
        cpu.clock();
    }
}
