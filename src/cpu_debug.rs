enum Addr {
    Imm,
}

fn print(opcode: u8) -> (Addr, Opcode) {
    match opcode {
// ADC - Add with Carry
0x69 => (Addr::Imm, Cpu::adc),
0x65 => (Addr::Zpr, Cpu::adc),
0x75 => (Addr::ZpXr, Cpu::adc),
0x6D => (Addr::Absr, Cpu::adc),
0x7D => (Addr::AbsXr, Cpu::adc),
0x79 => (Addr::AbsYr, Cpu::adc),
0x61 => (Addr::IndXr, Cpu::adc),
0x71 => (Addr::IndYr, Cpu::adc),
// AND - Logical AND
0x29 => (Addr::Imm, Cpu::and),
0x25 => (Addr::Zpr, Cpu::and),
0x35 => (Addr::ZpXr, Cpu::and),
0x2D => (Addr::Absr, Cpu::and),
0x3D => (Addr::AbsXr, Cpu::and),
0x39 => (Addr::AbsYr, Cpu::and),
0x21 => (Addr::IndXr,Cpu::and),
0x31 => (Addr::IndYr, Cpu::and),
// ASL - Arithmetic Shift Left
0x0A => (Addr::Acc, Cpu::asl),
0x06 => (Addr::Zprw, Cpu::asl),
0x16 => (Addr::ZpXrw, Cpu::asl),
0x0E => (Addr::Absrw, Cpu::asl),
0x1E => (Addr::AbsXrw, Cpu::asl),
// BCC - Branch if Carry Clear
0x90 => (Addr::Rel, self.get_bit(StatusBit::Carry) == false),
// BCS - Branch if Carry Set
0xB0 => (Addr::Rel, self.get_bit(StatusBit::Carry) == true),
// BEQ - Branch if Equal
0xF0 => (Addr::Rel, self.get_bit(StatusBit::Zero) == true),
// BIT - Bit Test
0x24 => (Addr::Zpr, Cpu::bit),
0x2C => (Addr::Absr, Cpu::bit),
// BMI - Branch if Minus
0x30 => (Addr::Rel, self.get_bit(StatusBit::Sign) == true),
// BNE - Branch if Not Equal
0xD0 => (Addr::Rel, self.get_bit(StatusBit::Zero) == false),
// BPL - Branch if Positive
0x10 => (Addr::Rel, self.get_bit(StatusBit::Sign) == false),
// BRK - Force Interrupt
0x00 => self.brk(),
// BVC - Branch if Overflow Clear
0x50 => (Addr::Rel, self.get_bit(StatusBit::Overflow) == false),
// BVS - Branch if Overflow Set
0x70 => (Addr::Rel, self.get_bit(StatusBit::Overflow) == true),
// CLC - Clear Carry Flag
0x18 => (Addr::Impl, Cpu::clc),
// CLD - Clear Decimal Mode
0xD8 => (Addr::Impl, Cpu::cld),
// CLI - Clear Interrupt Disable
0x58 => (Addr::Impl, Cpu::cli),
// CLV - Clear Overflow Flag
0xB8 => (Addr::Impl, Cpu::clv),
// CMP - Compare
0xC9 => (Addr::Imm, Cpu::cmp),
0xC5 => (Addr::Zpr, Cpu::cmp),
0xD5 => (Addr::ZpXr, Cpu::cmp),
0xCD => (Addr::Absr, Cpu::cmp),
0xDD => (Addr::AbsXr, Cpu::cmp),
0xD9 => (Addr::AbsYr, Cpu::cmp),
0xC1 => (Addr::IndXr,Cpu::cmp),
0xD1 => (Addr::IndYr, Cpu::cmp),
// CPX - Compare X Register
0xE0 => (Addr::Imm, Cpu::cpx),
0xE4 => (Addr::Zpr, Cpu::cpx),
0xEC => (Addr::Absr, Cpu::cpx),
// CPY - Compare Y Register
0xC0 => (Addr::Imm, Cpu::cpy),
0xC4 => (Addr::Zpr, Cpu::cpy),
0xCC => (Addr::Absr, Cpu::cpy),
// DCP
0xC3 => (Addr::IndXrw, Cpu::dcp),
0xC7 => (Addr::Zprw, Cpu::dcp),
0xCF => (Addr::Absrw, Cpu::dcp),
0xD3 => (Addr::IndYrw, Cpu::dcp),
0xD7 => (Addr::ZpXrw, Cpu::dcp),
0xDB => (Addr::AbsYrw, Cpu::dcp),
0xDF => (Addr::AbsXrw, Cpu::dcp),
// DEC - Decrement Memory
0xC6 => (Addr::Zprw, Cpu::dec),
0xD6 => (Addr::ZpXrw, Cpu::dec),
0xCE => (Addr::Absrw, Cpu::dec),
0xDE => (Addr::AbsXrw, Cpu::dec),
// DEX - Decrement X Register
0xCA => (Addr::Impl, Cpu::dex),
// DEY - Decrement Y Register
0x88 => (Addr::Impl, Cpu::dey),
// EOR - Exclusive OR
0x49 => (Addr::Imm, Cpu::eor),
0x45 => (Addr::Zpr, Cpu::eor),
0x55 => (Addr::ZpXr, Cpu::eor),
0x4D => (Addr::Absr, Cpu::eor),
0x5D => (Addr::AbsXr, Cpu::eor),
0x59 => (Addr::AbsYr, Cpu::eor),
0x41 => (Addr::IndXr,Cpu::eor),
0x51 => (Addr::IndYr, Cpu::eor),
// IGN
0x04 | 0x44 | 0x64 => (Addr::Zpr, Cpu::ign),
0x0C => (Addr::Absr, Cpu::ign),
0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => (Addr::ZpXr, Cpu::ign),
0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => (Addr::AbsXr, Cpu::ign),
// INC - Increment Memory
0xE6 => (Addr::Zprw, Cpu::inc),
0xF6 => (Addr::ZpXrw, Cpu::inc),
0xEE => (Addr::Absrw, Cpu::inc),
0xFE => (Addr::AbsXrw, Cpu::inc),
// INX - Increment X Register
0xE8 => (Addr::Impl, Cpu::inx),
// INY - Increment Y Register
0xC8 => (Addr::Impl, Cpu::iny),
// ISC
0xE3 => (Addr::IndXrw, Cpu::isb),
0xE7 => (Addr::Zprw, Cpu::isb),
0xEF => (Addr::Absrw, Cpu::isb),
0xF3 => (Addr::IndYrw, Cpu::isb),
0xF7 => (Addr::ZpXrw, Cpu::isb),
0xFB => (Addr::AbsYrw, Cpu::isb),
0xFF => (Addr::AbsXrw, Cpu::isb),
// JMP - Jump
0x4C => self.absolute_jmp(), //jmp(Cpu::absolute),
0x6C => self.indirect_jmp(), //jmp(Cpu::indirect),
// JSR - Jump to Subroutine
0x20 => self.jsr(),//jsr(Cpu::absolute),
// LAX
0xA3 => (Addr::IndXr,Cpu::lax),
0xA7 => (Addr::Zpr, Cpu::lax),
0xAF => (Addr::Absr, Cpu::lax),
0xB3 => (Addr::IndYr, Cpu::lax),
0xB7 => (Addr::ZpYr, Cpu::lax),
0xBF => (Addr::AbsYr, Cpu::lax),
// LDA - Load Accumulator
0xA9 => (Addr::Imm, Cpu::lda),
0xA5 => (Addr::Zpr, Cpu::lda),
0xB5 => (Addr::ZpXr, Cpu::lda),
0xAD => (Addr::Absr, Cpu::lda),
0xBD => (Addr::AbsXr, Cpu::lda),
0xB9 => (Addr::AbsYr, Cpu::lda),
0xA1 => (Addr::IndXr,Cpu::lda),
0xB1 => (Addr::IndYr, Cpu::lda),
// LDX - Load X Register
0xA2 => (Addr::Imm, Cpu::ldx),
0xA6 => (Addr::Zpr, Cpu::ldx),
0xB6 => (Addr::ZpYr, Cpu::ldx),
0xAE => (Addr::Absr, Cpu::ldx),
0xBE => (Addr::AbsYr, Cpu::ldx),
// LDY - Load Y Register
0xA0 => (Addr::Imm, Cpu::ldy),
0xA4 => (Addr::Zpr, Cpu::ldy),
0xB4 => (Addr::ZpXr, Cpu::ldy),
0xAC => (Addr::Absr, Cpu::ldy),
0xBC => (Addr::AbsXr, Cpu::ldy),
// LSR - Logical Shift Right
0x4A => (Addr::Acc, Cpu::lsr),
0x46 => (Addr::Zprw, Cpu::lsr),
0x56 => (Addr::ZpXrw, Cpu::lsr),
0x4E => (Addr::Absrw, Cpu::lsr),
0x5E => (Addr::AbsXrw, Cpu::lsr),
// NOP - No Operation
0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => (Addr::Impl, Cpu::nop),
// ORA - Logical Inclusive OR
0x09 => (Addr::Imm, Cpu::ora),
0x05 => (Addr::Zpr, Cpu::ora),
0x15 => (Addr::ZpXr, Cpu::ora),
0x0D => (Addr::Absr, Cpu::ora),
0x1D => (Addr::AbsXr, Cpu::ora),
0x19 => (Addr::AbsYr, Cpu::ora),
0x01 => (Addr::IndXr,Cpu::ora),
0x11 => (Addr::IndYr, Cpu::ora),
// PHA - Push Accumulator
0x48 => self.pha(),
// PHP - Push Processor Status
0x08 => self.php(),
// PLA - Pull Accumulator
0x68 => self.pla(),
// PLP - Pull Processor Status
0x28 => self.plp(),
// RLA
0x23 => (Addr::IndXrw, Cpu::rla),
0x27 => (Addr::Zprw, Cpu::rla),
0x2F => (Addr::Absrw, Cpu::rla),
0x33 => (Addr::IndYrw, Cpu::rla),
0x37 => (Addr::ZpXrw, Cpu::rla),
0x3B => (Addr::AbsYrw, Cpu::rla),
0x3F => (Addr::AbsXrw, Cpu::rla),
// ROL - Rotate Left
0x2A => (Addr::Acc, Cpu::rol),
0x26 => (Addr::Zprw, Cpu::rol),
0x36 => (Addr::ZpXrw, Cpu::rol),
0x2E => (Addr::Absrw, Cpu::rol),
0x3E => (Addr::AbsXrw, Cpu::rol),
// ROR - Rotate Right
0x6A => (Addr::Acc, Cpu::ror),
0x66 => (Addr::Zprw, Cpu::ror),
0x76 => (Addr::ZpXrw, Cpu::ror),
0x6E => (Addr::Absrw, Cpu::ror),
0x7E => (Addr::AbsXrw, Cpu::ror),
// RRA
0x63 => (Addr::IndXrw, Cpu::rra),
0x67 => (Addr::Zprw, Cpu::rra),
0x6F => (Addr::Absrw, Cpu::rra),
0x73 => (Addr::IndYrw, Cpu::rra),
0x77 => (Addr::ZpXrw, Cpu::rra),
0x7B => (Addr::AbsYrw, Cpu::rra),
0x7F => (Addr::AbsXrw, Cpu::rra),
// RTI - Return from Interrupt
0x40 => self.rti(),
// RTS - Return from Subroutine
0x60 => self.rts(),
// SAX
0x83 => self.indexed_x_write(self.a & self.x),
0x87 => (Addr::Zpw, self.a & self.x),
0x8F => (Addr::Absw, self.a & self.x),
0x97 => (Addr::ZpYw, self.a & self.x),
// SBC - Subtract with Carry
0xE9 | 0xEB => (Addr::Imm, Cpu::sbc),
0xE5 => (Addr::Zpr, Cpu::sbc),
0xF5 => (Addr::ZpXr, Cpu::sbc),
0xED => (Addr::Absr, Cpu::sbc),
0xFD => (Addr::AbsXr, Cpu::sbc),
0xF9 => (Addr::AbsYr, Cpu::sbc),
0xE1 => (Addr::IndXr,Cpu::sbc),
0xF1 => (Addr::IndYr, Cpu::sbc),
// SEC - Set Carry Flag
0x38 => (Addr::Impl, Cpu::sec),
// SED - Set Decimal Flag
0xF8 => (Addr::Impl, Cpu::sed),
// SEI - Set Interrupt Disable
0x78 => (Addr::Impl, Cpu::sei),
// SLO
0x03 => (Addr::IndXrw, Cpu::slo),
0x07 => (Addr::Zprw, Cpu::slo),
0x0F => (Addr::Absrw, Cpu::slo),
0x13 => (Addr::IndYrw, Cpu::slo),
0x17 => (Addr::ZpXrw, Cpu::slo),
0x1B => (Addr::AbsYrw, Cpu::slo),
0x1F => (Addr::AbsXrw, Cpu::slo),
// SRE
0x43 => (Addr::IndXrw, Cpu::sre),
0x47 => (Addr::Zprw, Cpu::sre),
0x4f => (Addr::Absrw, Cpu::sre),
0x53 => (Addr::IndYrw, Cpu::sre),
0x57 => (Addr::ZpXrw, Cpu::sre),
0x5B => (Addr::AbsYrw, Cpu::sre),
0x5F => (Addr::AbsXrw, Cpu::sre),
// STA - Store Accumulator
0x85 => (Addr::Zpw, self.a),
0x95 => (Addr::ZpXw, self.a),
0x8D => (Addr::Absw, self.a),
0x9D => self.absolute_indexed_write(self.x, self.a),
0x99 => self.absolute_indexed_write(self.y, self.a),
0x81 => self.indexed_x_write(self.a),
0x91 => self.indexed_y_write(self.a),
// STX - Store X Register
0x86 => (Addr::Zpw, self.x),
0x96 => (Addr::ZpYw, self.x),
0x8E => (Addr::Absw, self.x),
// STY - Store Y Register
0x84 => (Addr::Zpw, self.y),
0x94 => (Addr::ZpXw, self.y),
0x8C => (Addr::Absw, self.y),
// SKB
0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => (Addr::Imm, Cpu::skb),
// TAX - Transfer Accumulator to X
0xAA => (Addr::Impl, Cpu::tax),
// TAY - Transfer Accumulator to Y
0xA8 => (Addr::Impl, Cpu::tay),
// TSX - Transfer Stack Pointer to X
0xBA => (Addr::Impl, Cpu::tsx),
// TXA - Transfer X to Accumulator
0x8A => (Addr::Impl, Cpu::txa),
// TXS - Transfer X to Stack Pointer
0x9A => (Addr::Impl, Cpu::txs),
// TYA - Transfer Y to Accumulator
0x98 => (Addr::Impl, Cpu::tya),