pub mod bus;
use bus::{Bus};

mod nes;
mod cpu;
use cpu::Cpu;

pub mod ppu;

fn main() {
    let bus = Bus::new();
    let mut cpu = Cpu::new(bus);

    let start_clock = 43754;
    let clock_count = 100;

    for _ in 0..start_clock + clock_count {
        cpu.clock(start_clock);
    }
}
