pub mod bus;
use bus::{Bus};

mod nes;
mod cpu;
use cpu::Cpu;

pub mod ppu;

fn main() {
    let bus = Bus::new();
    let mut cpu = Opcode::new(bus);

    let start_clock = 43754;
    let clock_count = 100;

    for _ in 0..start_clock + clock_count {
        cpu.clock(start_clock);
    }
}


enum Opcode {
    Adc,
    And,
}

enum Addr {
    Imm,
    Zp,
    ZpX,
    Abs,
    AbsX,
    AbsY,
    IndX,
    IndY,
}

fn print(opcode: u8) -> (Addr, Opcode) {
    //let arr:[(u8, Opcode, Addr); 10] = [
    match opcode {
        0x69 => (Opcode::Adc, Addr::Imm),
        0x65 => (Adc, Zp),
        0x75 => (Adc, ZpXRW),
        0x6D => (Adc, Abs),
        0x7D => (Adc, AbsX),
        0x79 => (Adc, AbsY),
        0x61 => (Adc, IndX),
        0x71 => (Adc, IndY),
        0x29 => (And, Imm),
        0x25 => (Addr::Zp, Opcode::And),
        0x35 => (Addr::ZpX, Opcode::And),
        0x2D => (Addr::Abs, Opcode::And),
        0x3D => (Addr::AbsX, Opcode::And),
        0x39 => (Addr::AbsY, Opcode::And),
        0x21 => (Addr::IndX, Opcode::And),
        0x31 => (Addr::IndY, Opcode::And),
        // ASL - Arithmetic Shift Left
        0x0A => (Addr::Acc, Opcode::asl),
        0x06 => (Addr::Zp, Opcode::asl),
        0x16 => (Addr::ZpX, Opcode::asl),
        0x0E => (Addr::Abs, Opcode::asl),
        0x1E => (Addr::AbsX, Opcode::asl),
        // BCC - Branch if Carry Clear
0x90 => (Addr::Rel, (self.get_bit(StatusBit::Carry) == false),
// BCS - Branch if Carry Set
0xB0 => (Addr::Rel, (self.get_bit(StatusBit::Carry) == true),
// BEQ - Branch if Equal
0xF0 => (Addr::Rel, (self.get_bit(StatusBit::Zero) == true),
// BIT - Bit Test
0x24 => (Addr::Zp, Opcode::bit),
0x2C => (Addr::AbsOpcode::bit),
// BMI - Branch if Minus
0x30 => (Addr::Rel, (self.get_bit(StatusBit::Sign) == true),
// BNE - Branch if Not Equal
0xD0 => (Addr::Rel, (self.get_bit(StatusBit::Zero) == false),
// BPL - Branch if Positive
0x10 => (Addr::Rel, (self.get_bit(StatusBit::Sign) == false),
// BRK - Force Interrupt
0x00 => self.brk(),
// BVC - Branch if Overflow Clear
0x50 => (Addr::Rel, (self.get_bit(StatusBit::Overflow) == false),
// BVS - Branch if Overflow Set
0x70 => (Addr::Rel, (self.get_bit(StatusBit::Overflow) == true),
// CLC - Clear Carry Flag
0x18 => s(Addr::Impl, Opcode::clc),
// CLD - Clear Decimal Mode
0xD8 => s(Addr::Impl, Opcode::cld),
// CLI - Clear Interrupt Disable
0x58 => s(Addr::Impl, Opcode::cli),
// CLV - Clear Overflow Flag
0xB8 => s(Addr::Impl, Opcode::clv),
// CMP - Compare
0xC9 => (Addr::Imm, Opcode::cmp),
0xC5 => (Addr::Zp, Opcode::cmp),
0xD5 => (Addr::ZpX,  Opcode::cmp),
0xCD => (Addr::AbsOpcode::cmp),
0xDD => (Addr::AbsX, Opcode::cmp),
0xD9 => (Addr::AbsY, Opcode::cmp),
0xC1 => (Addr::IndX, Opcode::cmp),
0xD1 => (Addr::IndY, Opcode::cmp),
// CPX - Compare X Register
0xE0 => (Addr::Imm, Opcode::cpx),
0xE4 => (Addr::Zp, Opcode::cpx),
0xEC => (Addr::AbsOpcode::cpx),
// CPY - Compare Y Register
0xC0 => (Addr::Imm, Opcode::cpy),
0xC4 => (Addr::Zp, Opcode::cpy),
0xCC => (Addr::AbsOpcode::cpy),
// DCP
0xC3 => (Addr::IndX, Opcode::dcp),
0xC7 => (Addr::Zp, Opcode::dcp),
0xCF => (Addr::Abs, Opcode::dcp),
0xD3 => (Addr::IndY, Opcode::dcp),
0xD7 => (Addr::ZpX, Opcode::dcp),
0xDB => (Addr::AbsY, Opcode::dcp),
0xDF => (Addr::AbsX, Opcode::dcp),
// DEC - Decrement Memory
0xC6 => (Addr::Zp, Opcode::dec),
0xD6 => (Addr::ZpX, Opcode::dec),
0xCE => (Addr::Abs, Opcode::dec),
0xDE => (Addr::AbsX, Opcode::dec),
// DEX - Decrement X Register
0xCA => s(Addr::Impl, Opcode::dex),
// DEY - Decrement Y Register
0x88 => s(Addr::Impl, Opcode::dey),
// EOR - Exclusive OR
0x49 => (Addr::Imm, Opcode::eor),
0x45 => (Addr::Zp, Opcode::eor),
0x55 => (Addr::ZpX,  Opcode::eor),
0x4D => (Addr::AbsOpcode::eor),
0x5D => (Addr::AbsX, Opcode::eor),
0x59 => (Addr::AbsY, Opcode::eor),
0x41 => (Addr::IndX, Opcode::eor),
0x51 => (Addr::IndY, Opcode::eor),
// IGN
0x04 | 0x44 | 0x64 => (Addr::Zp, Opcode::ign),
0x0C => (Addr::AbsOpcode::ign),
0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => (Addr::ZpX,  Opcode::ign),
0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => (Addr::AbsX, Opcode::ign),
// INC - Increment Memory
0xE6 => (Addr::Zp, Opcode::inc),
0xF6 => (Addr::ZpX, Opcode::inc),
0xEE => (Addr::Abs, Opcode::inc),
0xFE => (Addr::AbsX, Opcode::inc),
// INX - Increment X Register
0xE8 => s(Addr::Impl, Opcode::inx),
// INY - Increment Y Register
0xC8 => s(Addr::Impl, Opcode::iny),
// ISC
0xE3 => (Addr::IndX, Opcode::isb),
0xE7 => (Addr::Zp, Opcode::isb),
0xEF => (Addr::Abs, Opcode::isb),
0xF3 => (Addr::IndY, Opcode::isb),
0xF7 => (Addr::ZpX, Opcode::isb),
0xFB => (Addr::AbsY, Opcode::isb),
0xFF => (Addr::AbsX, Opcode::isb),
// JMP - Jump
0x4C => self.absolute_jmp(), //jmp(Opcode::absolute),
0x6C => self.indirect_jmp(), //jmp(Opcode::indirect),
// JSR - Jump to Subroutine
0x20 => self.jsr(),//jsr(Opcode::absolute),
// LAX
0xA3 => (Addr::IndX, Opcode::lax),
0xA7 => (Addr::Zp, Opcode::lax),
0xAF => (Addr::AbsOpcode::lax),
0xB3 => (Addr::IndY, Opcode::lax),
0xB7 => (Addr::ZpY, Opcode::lax),
0xBF => (Addr::AbsY, Opcode::lax),
// LDA - Load Accumulator
0xA9 => (Addr::Imm, Opcode::lda),
0xA5 => (Addr::Zp, Opcode::lda),
0xB5 => (Addr::ZpX,  Opcode::lda),
0xAD => (Addr::AbsOpcode::lda), //lda(Opcode::absolute),
0xBD => (Addr::AbsX, Opcode::lda),
0xB9 => (Addr::AbsY, Opcode::lda),
0xA1 => (Addr::IndX, Opcode::lda),
0xB1 => (Addr::IndY, Opcode::lda),
// LDX - Load X Register
0xA2 => (Addr::Imm, Opcode::ldx),
0xA6 => (Addr::Zp, Opcode::ldx),
0xB6 => (Addr::ZpY, Opcode::ldx),
0xAE => (Addr::AbsOpcode::ldx),
0xBE => (Addr::AbsY, Opcode::ldx),
// LDY - Load Y Register
0xA0 => (Addr::Imm, Opcode::ldy),
0xA4 => (Addr::Zp, Opcode::ldy),
0xB4 => (Addr::ZpX,  Opcode::ldy),
0xAC => (Addr::AbsOpcode::ldy),
0xBC => (Addr::AbsX, Opcode::ldy),
// LSR - Logical Shift Right
0x4A => (Addr::Acc, Opcode::lsr),
0x46 => (Addr::Zp, Opcode::lsr),
0x56 => (Addr::ZpX, Opcode::lsr),
0x4E => (Addr::Abs, Opcode::lsr),
0x5E => (Addr::AbsX, Opcode::lsr),
// NOP - No Operation
0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => (Addr::Impl, Opcode::nop),
// ORA - Logical Inclusive OR
0x09 => (Addr::Imm, Opcode::ora),
0x05 => (Addr::Zp, Opcode::ora),
0x15 => (Addr::ZpX,  Opcode::ora),
0x0D => (Addr::AbsOpcode::ora),
0x1D => (Addr::AbsX, Opcode::ora),
0x19 => (Addr::AbsY, Opcode::ora),
0x01 => (Addr::IndX, Opcode::ora),
0x11 => (Addr::IndY, Opcode::ora),
// PHA - Push Accumulator
0x48 => self.pha(),
// PHP - Push Processor Status
0x08 => self.php(),
// PLA - Pull Accumulator
0x68 => self.pla(),
// PLP - Pull Processor Status
0x28 => self.plp(),
// RLA
0x23 => (Addr::IndX, Opcode::rla),
0x27 => (Addr::Zp, Opcode::rla),
0x2F => (Addr::Abs, Opcode::rla),
0x33 => (Addr::IndY, Opcode::rla),
0x37 => (Addr::ZpX, Opcode::rla),
0x3B => (Addr::AbsY, Opcode::rla),
0x3F => (Addr::AbsX, Opcode::rla),
// ROL - Rotate Left
0x2A => (Addr::Acc, Opcode::rol),
0x26 => (Addr::Zp, Opcode::rol),
0x36 => (Addr::ZpX, Opcode::rol),
0x2E => (Addr::Abs, Opcode::rol),
0x3E => (Addr::AbsX, Opcode::rol),
// ROR - Rotate Right
0x6A => (Addr::Acc, Opcode::ror),
0x66 => (Addr::Zp, Opcode::ror),
0x76 => (Addr::ZpX, Opcode::ror),
0x6E => (Addr::Abs, Opcode::ror),
0x7E => (Addr::AbsX, Opcode::ror),
// RRA
0x63 => (Addr::IndX, Opcode::rra),
0x67 => (Addr::Zp, Opcode::rra),
0x6F => (Addr::Abs, Opcode::rra),
0x73 => (Addr::IndY, Opcode::rra),
0x77 => (Addr::ZpX, Opcode::rra),
0x7B => (Addr::AbsY, Opcode::rra),
0x7F => (Addr::AbsX, Opcode::rra),
// RTI - Return from Interrupt
0x40 => self.rti(),
// RTS - Return from Subroutine
0x60 => self.rts(),
// SAX
0x83 => self.indexed_x_write(self.a & self.x),
0x87 => self.zero_page_write(self.a & self.x),
0x8F => self.absolute_write(self.a & self.x),
0x97 => self.zero_page_indexed_write(self.y, self.a & self.x),
// SBC - Subtract with Carry
0xE9 | 0xEB => (Addr::Imm, Opcode::sbc),
0xE5 => (Addr::Zp, Opcode::sbc),
0xF5 => (Addr::ZpX,  Opcode::sbc),
0xED => (Addr::AbsOpcode::sbc),
0xFD => (Addr::AbsX, Opcode::sbc),
0xF9 => (Addr::AbsY, Opcode::sbc),
0xE1 => (Addr::IndX, Opcode::sbc),
0xF1 => (Addr::IndY, Opcode::sbc),
// SEC - Set Carry Flag
0x38 => s(Addr::Impl, Opcode::sec),
// SED - Set Decimal Flag
0xF8 => s(Addr::Impl, Opcode::sed),
// SEI - Set Interrupt Disable
0x78 => s(Addr::Impl, Opcode::sei),
// SLO
0x03 => (Addr::IndX, Opcode::slo),
0x07 => (Addr::Zp, Opcode::slo),
0x0F => (Addr::Abs, Opcode::slo),
0x13 => (Addr::IndY, Opcode::slo),
0x17 => (Addr::ZpX, Opcode::slo),
0x1B => (Addr::AbsY, Opcode::slo),
0x1F => (Addr::AbsX, Opcode::slo),
// SRE
0x43 => (Addr::IndX, Opcode::sre),
0x47 => (Addr::Zp, Opcode::sre),
0x4f => (Addr::Abs, Opcode::sre),
0x53 => (Addr::IndY, Opcode::sre),
0x57 => (Addr::ZpX, Opcode::sre),
0x5B => (Addr::AbsY, Opcode::sre),
0x5F => (Addr::AbsX, Opcode::sre),
// STA - Store Accumulator
0x85 => self.zero_page_write(self.a),
0x95 => self.zero_page_indexed_write(self.x, self.a),
0x8D => self.absolute_write(self.a),
0x9D => self.absolute_indexed_write(self.x, self.a),
0x99 => self.absolute_indexed_write(self.y, self.a),
0x81 => self.indexed_x_write(self.a),
0x91 => self.indexed_y_write(self.a),
// STX - Store X Register
0x86 => self.zero_page_write(self.x),
0x96 => self.zero_page_indexed_write(self.y, self.x),
0x8E => self.absolute_write(self.x),
// STY - Store Y Register
0x84 => self.zero_page_write(self.y),
0x94 => self.zero_page_indexed_write(self.x, self.y),
0x8C => self.absolute_write(self.y),
// SKB
0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => (Addr::Imm, Opcode::skb),
// TAX - Transfer Accumulator to X
0xAA => (Addr::Impl, Opcode::tax),
// TAY - Transfer Accumulator to Y
0xA8 => (Addr::Impl, Opcode::tay),
// TSX - Transfer Stack Pointer to X
0xBA => (Addr::Impl, Opcode::tsx),
// TXA - Transfer X to Accumulator
0x8A => (Addr::Impl, Opcode::txa),
// TXS - Transfer X to Stack Pointer
0x9A => (Addr::Impl, Opcode::txs),
// TYA - Transfer Y to Accumulator
0x98 => (Addr::Impl, Opcode::tya),
_ => {},
        }