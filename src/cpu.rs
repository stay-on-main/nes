//mod bus;
use super::bus::{Bus};

//mod nes;

enum StatusBit {
    Carry = 1 << 0,
    Zero = 1 << 1,
    Interrupt = 1 << 2,
    Decimal = 1 << 3,
    Break = 1 << 4,
    Overflow = 1 << 6,
    Sign = 1 << 7,
}

pub struct Cpu {
    a: u8,
    pc: u16,
    s: u8,
    x: u8,
    y: u8,
    p: u8,
    bus: Bus,
}

impl Cpu {
    fn get_bit(&self, bit: StatusBit) -> bool {
        self.p & (bit as u8) != 0
    }

    fn update_bit(&mut self, bit: StatusBit, op: u8) {
        match bit {
            StatusBit::Zero => {
                if op == 0 {
                    self.p |= bit as u8;
                } else {
                    self.p &= !(bit as u8);
                }
            },
            StatusBit::Sign => {
                if (op & 0x80) != 0 {
                    self.p |= bit as u8;
                } else {
                    self.p &= !(bit as u8);
                }
            },
            StatusBit::Carry => {
                if op != 0 {
                    self.p |= bit as u8;
                } else {
                    self.p &= !(bit as u8);
                }
            },
            StatusBit::Overflow => {
                if op != 0 {
                    self.p |= bit as u8;
                } else {
                    self.p &= !(bit as u8);
                }
            },
            StatusBit::Interrupt => {
                if op != 0 {
                    self.p |= bit as u8;
                } else {
                    self.p &= !(bit as u8);
                }
            },
            StatusBit::Decimal => {
                if op != 0 {
                    self.p |= bit as u8;
                } else {
                    self.p &= !(bit as u8);
                }
            },
            _ => todo!(),
        }
    }

    fn fetch_u8(&mut self) -> u8 {
        let b = self.bus.read(self.pc);
        self.pc += 1;
        b
    }

    fn stack_push(&mut self, b: u8) {
        self.bus.write(0x100 + self.s as u16, b);
    }

    fn stack_pull(&mut self) -> u8 {
        self.bus.read(0x100 + self.s as u16)
    }

    fn stack_inc(&mut self) {
        self.s += 1;
        self.bus.clock();
    }

    fn pcl(&self) -> u8 {
        self.pc as u8
    }

    fn pch(&self) -> u8 {
        (self.pc >> 8) as u8
    }

    fn brk(&mut self) {
        // 2
        self.fetch_u8();
        // 3
        self.stack_push(self.pcl());
        self.s -= 1;
        // 4
        self.stack_push(self.pch());
        self.s -= 1;
        // 5
        self.stack_push(self.p | (1 << 6));// (StatusBit::Break as u8);
        self.s -= 1;
        // 6
        self.pc = self.bus.read(0xfffe) as u16; // pcl
        // 7
        self.pc |= (self.bus.read(0xffff) as u16) << 8; // pch
    }
    
    fn rti(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.stack_inc();
        // 4
        self.p = self.stack_pull() | (1 << 5);
        self.s += 1;
        // 5
        self.pc = self.stack_pull() as u16;
        self.s += 1;
        // 6
        self.pc |= (self.stack_pull() as u16) << 8;
    }
    
    fn rts(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.stack_inc();
        // 4
        self.pc = self.stack_pull() as u16; // pcl
        self.s += 1;
        // 5
        self.pc |= (self.stack_pull() as u16) << 8; // pch
        // 6
        self.pc += 1;
        self.bus.clock();
    }
    
    // pha, php
    fn pha(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.stack_push(self.a); // or self.p
        self.s -= 1;
    }
    
    fn php(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.stack_push(self.p | (StatusBit::Break as u8)); // or self.p
        self.s -= 1;
    }

    // pla, plp
    fn pla(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.s += 1;
        self.bus.clock();
        // 4
        self.a = self.stack_pull();
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }
    
    fn plp(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.s += 1;
        self.bus.clock();
        // 4
        self.p = self.stack_pull();
        self.p &= !(StatusBit::Break as u8);
        self.p |=  1 << 5;
    }

    fn jsr(&mut self) {
        // 2
        let abl = self.fetch_u8();
        // 3
        // internal operation (predecrement S?)
        self.bus.clock();
        // 4
        self.stack_push((self.pc >> 8) as u8);
        self.s -= 1;
        // 5
        self.stack_push(self.pc as u8);
        self.s -= 1;
        // 6
        self.pc = (self.fetch_u8() as u16) << 8; // pch
        self.pc |= abl as u16; // pcl
    }
    
    fn accumulator(&mut self, func: fn (&mut Cpu, u8) -> u8) {
        // 2
        self.bus.clock();
        self.a = func(self, self.a);
    }
    
    fn implied(&mut self, func: fn(&mut Cpu)) {
        // 2
        self.bus.read(self.pc);
        func(self);
    }
    
    fn immediate(&mut self, func: fn (&mut Cpu, u8)) {
        // 2
        let val = self.fetch_u8();
        func(self, val);
    }
    
    fn absolute_jmp(&mut self) {
        // 2
        let abl = self.fetch_u8();
        // 3
        self.pc = (self.fetch_u8() as u16) << 8; // pch
        self.pc |= abl as u16;
    }

    // LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, NOP
    fn absolute_read(&mut self, func: fn (&mut Cpu, u8)) {
        // 2
        let l = self.fetch_u8() as u16;
        // 3
        let h = self.fetch_u8() as u16;
        // 4
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        func(self, val);
    }

    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn absolute_read_modify_write(&mut self, func: fn (&mut Cpu, u8) -> u8) {
        // 2
        let l = self.fetch_u8() as u16;
        // 3
        let h = self.fetch_u8() as u16;
        // 4
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        // 5 
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        let val = func(self, val);
        // 6
        self.bus.write(addr, val);
    }

    // STA, STX, STY, SAX
    fn absolute_write(&mut self, reg_val: u8) {
        // 2
        let l = self.fetch_u8() as u16;
        // 3
        let h = self.fetch_u8() as u16;
        // 4
        let addr = (h << 8) | l;
        self.bus.write(addr, reg_val);
    }
    
    // LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, NOP
    fn zero_page_read(&mut self, func: fn (&mut Cpu, u8)) {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        let val = self.bus.read(addr);
        func(self, val);
    }
    
    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn zero_page_read_modify_write(&mut self, func: fn (&mut Cpu, u8) -> u8) {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        let val = self.bus.read(addr);
        // 4
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        //do the operation on 'val'
        let val = func(self, val);
        // 5
        self.bus.write(addr, val);
    }
    
    // STA, STX, STY, SAX
    fn zero_page_write(&mut self, reg_val: u8) {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        self.bus.write(addr, reg_val);
    }

    // LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, NOP
    fn zero_page_indexed_read(&mut self, index: u8, func: fn (&mut Cpu, u8)) {
        // 2
        let addr = self.fetch_u8();
        // 3
        self.bus.read(addr as u16);
        let addr = addr.wrapping_add(index) as u16;
        // 4
        let val = self.bus.read(addr);
        func(self, val);
    }
    
    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn zero_page_indexed_read_modify_write(&mut self, index: u8, func: fn (&mut Cpu, u8) -> u8) {
        // 2
        let addr = self.fetch_u8();
        // 3
        self.bus.read(addr as u16);
        let addr = addr.wrapping_add(index) as u16;
        // 4
        let val = self.bus.read(addr);
        // 5
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        //do the operation on 'val'
        let val = func(self, val);
        // 6
        self.bus.write(addr, val);
    }
    
    // STA, STX, STY, SAX
    fn zero_page_indexed_write(&mut self, index: u8, reg_val: u8) {
        // 2
        let addr = self.fetch_u8();// as u16;
        // 3
        self.bus.read(addr as u16);
        let addr = addr.wrapping_add(index);
        // 4
        self.bus.write(addr as u16, reg_val);
    }
    
    // LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, LAE, SHS, NOP
    fn absolute_indexed_read(&mut self, index: u8, func: fn (&mut Cpu, u8)) {
        // 2
        let l = self.fetch_u8();
        // 3
        let mut h = self.fetch_u8() as u16;
        let overflow = (l as u16 + index as u16) > 0xff;
        let l = l.wrapping_add(index) as u16;
        // 4
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        if overflow {
            // page boundary was crossed
            h += 1;
            self.bus.clock();
        }
        // 5
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        func(self, val);
    }

    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn absolute_indexed_read_modify_write(&mut self, index: u8, func: fn (&mut Cpu, u8) -> u8) {
        // 2
        let l = self.fetch_u8();
        // 3
        let mut h = self.fetch_u8() as u16;
        let overflow = (l as u16 + index as u16) > 0xff;
        let l = l.wrapping_add(index) as u16;
        // 4
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        self.bus.read((h << 8) | l);

        if overflow {
            // page boundary was crossed
            h += 1;
        }

        let addr = (h << 8) | l;
        // 5
        let val = self.bus.read(addr);
        // 6
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
         //do the operation on 'val'
        let val = func(self, val);
        // 7
        self.bus.write(addr, val);
    }
    
    // STA, STX, STY, SHA, SHX, SHY
    fn absolute_indexed_write(&mut self, index: u8, reg_val: u8) {
        // 2
        let l = self.fetch_u8();
        // 3
        let mut h = self.fetch_u8() as u16;
        let overflow = (l as u16 + index as u16) > 0xff;
        let l = l.wrapping_add(index)  as u16;
        // 4
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        self.bus.read((h << 8) | l);

        if overflow {
            h += 1;
        }
        // 5
        let addr = (h << 8) | l;
        self.bus.write(addr, reg_val);
    }
    
    // BCC, BCS, BNE, BEQ, BPL, BMI, BVC, BVS
    fn relative(&mut self, condition: bool) {
        // 2
        let operand = self.fetch_u8() as i8;
        //self.bus.read(self.pc);
        if condition {
            // 3
            self.bus.clock();
            let new_pc = (self.pc as i32 + operand as i32) as u16;

            if new_pc >> 8 != self.pc >> 8 {
                self.bus.clock();
            }

            self.pc = new_pc;
            //self.pcl += operand;
            
        } else {
            //self.pc += 1;
        }
        // 4
        //todo!();
    }
    
    // LDA, ORA, EOR, AND, ADC, CMP, SBC, LAX
    fn indexed_x_read(&mut self, func: fn (&mut Cpu, u8)) {
        // 2
        let addr = self.fetch_u8();
        // 3
        self.bus.read(addr as u16);
        let addr = addr.wrapping_add(self.x);
        // 4
        let l = self.bus.read(addr as u16) as u16;
        // 5
        let h = self.bus.read(addr.wrapping_add(1) as u16) as u16;
        // 6
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        func(self, val);
    }
    
    // SLO, SRE, RLA, RRA, ISB, DCP
    fn indexed_x_read_modify_write(&mut self, func: fn (&mut Cpu, u8) -> u8) {
        // 2
        let addr = self.fetch_u8();
        // 3
        self.bus.read(addr as u16);
        let addr = addr.wrapping_add(self.x);
        // 4
        let l = self.bus.read(addr as u16) as u16;
        // 5
        let h = self.bus.read(addr.wrapping_add(1) as u16) as u16;
        // 6
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        // 7
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        let val = func(self, val);
        // 8
        self.bus.write(addr, val);
    }
    
    // STA, SAX
    fn indexed_x_write(&mut self, reg_val: u8) {
        // 2
        let addr = self.fetch_u8();
        // 3
        let addr = addr.wrapping_add(self.x);
        self.bus.clock();
        // 4
        let l = self.bus.read(addr as u16) as u16;
        // 5
        let h = self.bus.read(addr.wrapping_add(1) as u16) as u16;
        // 6
        let addr = (h << 8) | l;
        self.bus.write(addr, reg_val);
    }

    // LDA, EOR, AND, ORA, ADC, SBC, CMP
    fn indexed_y_read(&mut self, func: fn (&mut Cpu, u8)) {
        // 2 fetch pointer address, increment PC
        let pointer_addr = self.fetch_u8();
        // 3 fetch effective address low
        let pointer_l = self.bus.read(pointer_addr as  u16);
        // 4
        let mut pointer_h = self.bus.read(pointer_addr.wrapping_add(1) as u16) as u16;
        let overflow = (pointer_l as u16 + self.y as u16) > 0xff;
        let pointer_l = pointer_l.wrapping_add(self.y) as u16;
        // 5
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        if overflow {
            pointer_h += 1;
            self.bus.clock();
        }

        let pointer = (pointer_h << 8) | pointer_l;
        // 6
        // + This cycle will be executed only if the effective address
        // was invalid during cycle #5, i.e. page boundary was crossed.
        let val = self.bus.read(pointer);
        func(self, val);
    }
    
    // SLO, SRE, RLA, RRA, ISB, DCP
    fn indexed_y_read_modify_write(&mut self, func: fn (&mut Cpu, u8) -> u8) {
        // 2
        let pointer_addr = self.fetch_u8() as u16;
        // 3
        let pointer_l = self.bus.read(pointer_addr);
        // 4
        let mut pointer_h = self.bus.read((pointer_addr + 1) & 0xff) as u16;
        let overflow = (pointer_l as u16 + self.y as u16) > 0xff;
        let pointer_l = pointer_l.wrapping_add(self.y) as u16;
        // 5
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        self.bus.read((pointer_h << 8) | pointer_l);

        if overflow {
            pointer_h += 1;
        }

        let pointer = (pointer_h << 8) | pointer_l;
        // 6
        let val = self.bus.read(pointer);
        // 7
        self.bus.write(pointer, val);
        // do the operation on it
        let val = func(self, val);
        // 8
        self.bus.write(pointer, val);
    }
    
    // STA, SHA
    fn indexed_y_write(&mut self, reg_val: u8) {
        // 2
        let pointer_addr = self.fetch_u8();
        // 3
        let pointer_l = self.bus.read(pointer_addr as u16);
        // 4
        let pointer_h = self.bus.read(pointer_addr.wrapping_add(1) as u16) as u16;
        let pointer_l = pointer_l.wrapping_add(self.y) as u16;
        // 5
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        let pointer = (pointer_h << 8) | pointer_l;
        self.bus.read(pointer);
        // 6
        self.bus.write(pointer, reg_val);
    }

    fn indirect_jmp(&mut self) {
        // 2
        let l = self.fetch_u8() as u16;
        // 3
        let h = (self.fetch_u8() as u16) << 8;
        // 4
        let low = self.bus.read(h | l);
        // 5
        // The PCH will always be fetched from the same page
        // than PCL, i.e. page boundary crossing is not handled.
        self.pc = (self.bus.read(h | ((l + 1) & 0xff)) as u16) << 8; // pch
        self.pc |= low as u16;
    }

    //================================================================
    fn adc(&mut self, byte: u8) {
        let mut temp = (self.a as u16) + (byte as u16);

        if self.get_bit(StatusBit::Carry) {
            temp += 1;
        }
        // The overflow flag is set when
        // the sign of the addends is the same and
        // differs from the sign of the sum
        self.update_bit(StatusBit::Overflow, !(self.a ^ byte) & (self.a ^ (temp as u8)) & 0x80);
        self.update_bit(StatusBit::Carry, if temp > 0xFF {1} else {0});
        self.a = temp as u8;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }

    fn and(&mut self, byte: u8) {
        self.a &= byte;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }

    fn cmp(&mut self, byte: u8) {
        let src = self.a.wrapping_sub(byte);
        self.update_bit(StatusBit::Carry, if self.a >= byte {1} else {0});
        self.update_bit(StatusBit::Sign, src);
        self.update_bit(StatusBit::Zero, src);
    }

    fn cpy(&mut self, byte: u8) {
        let src = self.y.wrapping_sub(byte);
        self.update_bit(StatusBit::Carry, if self.y >= byte {1} else {0});
        self.update_bit(StatusBit::Sign, src);
        self.update_bit(StatusBit::Zero, src);
    }

    fn cpx(&mut self, byte: u8) {
        let src = self.x.wrapping_sub(byte);
        self.update_bit(StatusBit::Carry, if self.x >= byte {1} else {0});
        self.update_bit(StatusBit::Sign, src);
        self.update_bit(StatusBit::Zero, src);
    }

    fn eor(&mut self, byte: u8) {
        self.a ^= byte;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }

    fn lda(&mut self, byte: u8) {
        self.a = byte;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }

    fn ldy(&mut self, byte: u8) {
        self.y = byte;
        self.update_bit(StatusBit::Sign, self.y);
        self.update_bit(StatusBit::Zero, self.y);
    }

    fn ldx(&mut self, byte: u8) {
        self.x = byte;
        self.update_bit(StatusBit::Sign, self.x);
        self.update_bit(StatusBit::Zero, self.x);
    }

    fn ora(&mut self, byte: u8) {
        self.a |= byte;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }

    fn sbc(&mut self, byte: u8) {
        let mut temp = (self.a as i32) - (byte as i32);
        
        if !self.get_bit(StatusBit::Carry) {
            temp -= 1;
        }

        self.update_bit(StatusBit::Overflow, ((self.a ^ (temp as u8)) & 0x80) & ((self.a ^ byte) & 0x80));
        self.update_bit(StatusBit::Carry, if self.a >= byte {1} else {0});
        self.a = temp as u8;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }

    fn lax(&mut self, byte: u8) {
        self.lda(byte);
        self.tax();
    }
    /*
    fn sax(&mut self) -> u8 {
        self.a & self.x
    }
    */
    fn bit(&mut self, byte: u8) {
        self.update_bit(StatusBit::Sign, byte);
        self.update_bit(StatusBit::Overflow, 0x40 & byte);
        self.update_bit(StatusBit::Zero, byte & self.a);
    }
    
    fn skb(&mut self, _: u8) {
        
    }

    fn ign(&mut self, _: u8) {
        
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.update_bit(StatusBit::Sign, self.x);
        self.update_bit(StatusBit::Zero, self.x);
    }

    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.update_bit(StatusBit::Sign, self.y);
        self.update_bit(StatusBit::Zero, self.y);
    }

    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.update_bit(StatusBit::Sign, self.y);
        self.update_bit(StatusBit::Zero, self.y);
    }

    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.update_bit(StatusBit::Sign, self.x);
        self.update_bit(StatusBit::Zero, self.x);
    }

    fn nop(&mut self) {
        
    }

    fn clc(&mut self) {
        self.update_bit(StatusBit::Carry, 0x00);
    }

    fn cld(&mut self) {
        self.update_bit(StatusBit::Decimal, 0x00);
    }

    fn cli(&mut self) {
        self.update_bit(StatusBit::Interrupt, 0x00);
    }

    fn clv(&mut self) {
        self.update_bit(StatusBit::Overflow, 0x00);
    }

    fn sec(&mut self) {
        self.update_bit(StatusBit::Carry, 0xff);
    }

    fn sed(&mut self) {
        self.update_bit(StatusBit::Decimal, 0xff);
    }

    fn sei(&mut self) {
        self.update_bit(StatusBit::Interrupt, 0xff);
    }
    
    fn tax(&mut self) {
        self.x = self.a;
        self.update_bit(StatusBit::Sign, self.x);
        self.update_bit(StatusBit::Zero, self.x);
    }

    fn tay(&mut self) {
        self.y = self.a;
        self.update_bit(StatusBit::Sign, self.y);
        self.update_bit(StatusBit::Zero, self.y);
    }

    fn tsx(&mut self) {
        self.x = self.s;
        self.update_bit(StatusBit::Sign, self.x);
        self.update_bit(StatusBit::Zero, self.x);
    }
    
    fn txa(&mut self) {
        self.a = self.x;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }
    
    fn txs(&mut self) {
        self.s = self.x;
    }
    
    fn tya(&mut self) {
        self.a = self.y;
        self.update_bit(StatusBit::Sign, self.a);
        self.update_bit(StatusBit::Zero, self.a);
    }
    // write
    /*
    fn sta(&mut self, byte: u8) {
        todo!();
    }
    */
    // modify
    fn dec(&mut self, byte: u8) -> u8 {
        let byte = byte.wrapping_sub(1);
        self.update_bit(StatusBit::Sign, byte);
        self.update_bit(StatusBit::Zero, byte);
        byte
    }

    fn dcp(&mut self, byte: u8) -> u8 {
        // Equivalent to DEC value then CMP value, except supporting more addressing modes.
        let byte = self.dec(byte);
        self.cmp(byte);
        byte
    }

    fn rra(&mut self, byte: u8) -> u8 {
        // Equivalent to ROR value then ADC value, except supporting more addressing modes
        let byte = self.ror(byte);
        self.adc(byte);
        byte
    }

    fn ror(&mut self, byte: u8) -> u8 {
        let mut res = byte >> 1;
        
        if self.get_bit(StatusBit::Carry) {
            res |= 0x80;
        }
        
        self.update_bit(StatusBit::Carry, byte & 0x01);
        self.update_bit(StatusBit::Sign, res);
        self.update_bit(StatusBit::Zero, res);
        res
    }

    fn rol(&mut self, byte: u8) -> u8 {
        let mut res = byte << 1;

        if self.get_bit(StatusBit::Carry) {
            res |= 0x01;
        }
        self.update_bit(StatusBit::Carry, byte & 0x80);
        self.update_bit(StatusBit::Sign, res);
        self.update_bit(StatusBit::Zero, res);
        res
    }

    fn slo(&mut self, byte: u8) -> u8 {
        // Equivalent to ASL value then ORA value, except supporting more addressing modes.
        let byte = self.asl(byte);
        self.ora(byte);
        byte
    }

    fn sre(&mut self, byte: u8) -> u8 {
        // Equivalent to LSR value then EOR value, except supporting more addressing modes
        let byte = self.lsr(byte);
        self.eor(byte);
        byte
    }

    fn isb(&mut self, byte: u8) -> u8 {
        // Equivalent to INC value then SBC value, except supporting more addressing modes.
        let byte = self.inc(byte);
        self.sbc(byte);
        byte
    }

    fn asl(&mut self, byte: u8) -> u8 {
        self.update_bit(StatusBit::Carry, byte & 0x80);
        let byte = byte << 1;
        self.update_bit(StatusBit::Sign, byte);
        self.update_bit(StatusBit::Zero, byte);
        byte
    }

    fn inc(&mut self, byte: u8) -> u8 {
        let byte = byte.wrapping_add(1);
        self.update_bit(StatusBit::Sign, byte);
        self.update_bit(StatusBit::Zero, byte);
        byte
    }

    fn lsr(&mut self, byte: u8) -> u8 {
        self.update_bit(StatusBit::Carry, byte & 0x01);
        let byte = byte >> 1;
        self.update_bit(StatusBit::Sign, byte);
        self.update_bit(StatusBit::Zero, byte);
        byte
    }

    fn rla(&mut self, byte: u8) -> u8 {
        // Equivalent to ROL value then AND value, except supporting more addressing modes
        let  byte = self.rol(byte);
        self.and(byte);
        byte
    }
    //================================================================
    pub fn clock(&mut self, start_clock: u32) {
        if self.bus.clk > start_clock {
            print!("{:02X}  ", self.pc);
            print!("A:{:02X} ", self.a);
            print!("X:{:02X} ", self.x);
            print!("Y:{:02X} ", self.y);
            print!("P:{:02X} ", self.p);
            print!("SP:{:02X}  ", self.s);
            print!("CYC:{} ", self.bus.clk);
            
            print!("PIX:{} LINE:{} ", self.bus.ppu.cycle, self.bus.ppu.scanline);
            println!();
        }
        //self.print_state();

        let instruction = self.fetch_u8();

        match instruction {
            // ADC - Add with Carry
            0x69 => self.immediate(Cpu::adc),
            0x65 => self.zero_page_read(Cpu::adc),
            0x75 => self.zero_page_indexed_read(self.x, Cpu::adc),
            0x6D => self.absolute_read(Cpu::adc),
            0x7D => self.absolute_indexed_read(self.x, Cpu::adc),
            0x79 => self.absolute_indexed_read(self.y, Cpu::adc),
            0x61 => self.indexed_x_read(Cpu::adc),
            0x71 => self.indexed_y_read(Cpu::adc),
            // AND - Logical AND
            0x29 => self.immediate(Cpu::and),
            0x25 => self.zero_page_read(Cpu::and),
            0x35 => self.zero_page_indexed_read(self.x, Cpu::and),
            0x2D => self.absolute_read(Cpu::and),
            0x3D => self.absolute_indexed_read(self.x, Cpu::and),
            0x39 => self.absolute_indexed_read(self.y, Cpu::and),
            0x21 => self.indexed_x_read(Cpu::and),
            0x31 => self.indexed_y_read(Cpu::and),
            // ASL - Arithmetic Shift Left
            0x0A => self.accumulator(Cpu::asl),
            0x06 => self.zero_page_read_modify_write(Cpu::asl),
            0x16 => self.zero_page_indexed_read_modify_write(self.x, Cpu::asl),
            0x0E => self.absolute_read_modify_write(Cpu::asl),
            0x1E => self.absolute_indexed_read_modify_write(self.x, Cpu::asl),
            // BCC - Branch if Carry Clear
            0x90 => self.relative(self.get_bit(StatusBit::Carry) == false),
            // BCS - Branch if Carry Set
            0xB0 => self.relative(self.get_bit(StatusBit::Carry) == true),
            // BEQ - Branch if Equal
            0xF0 => self.relative(self.get_bit(StatusBit::Zero) == true),
            // BIT - Bit Test
            0x24 => self.zero_page_read(Cpu::bit),
            0x2C => self.absolute_read(Cpu::bit),
            // BMI - Branch if Minus
            0x30 => self.relative(self.get_bit(StatusBit::Sign) == true),
            // BNE - Branch if Not Equal
            0xD0 => self.relative(self.get_bit(StatusBit::Zero) == false),
            // BPL - Branch if Positive
            0x10 => self.relative(self.get_bit(StatusBit::Sign) == false),
            // BRK - Force Interrupt
            0x00 => self.brk(),
            // BVC - Branch if Overflow Clear
            0x50 => self.relative(self.get_bit(StatusBit::Overflow) == false),
            // BVS - Branch if Overflow Set
            0x70 => self.relative(self.get_bit(StatusBit::Overflow) == true),
            // CLC - Clear Carry Flag
            0x18 => self.implied(Cpu::clc),
            // CLD - Clear Decimal Mode
            0xD8 => self.implied(Cpu::cld),
            // CLI - Clear Interrupt Disable
            0x58 => self.implied(Cpu::cli),
            // CLV - Clear Overflow Flag
            0xB8 => self.implied(Cpu::clv),
            // CMP - Compare
            0xC9 => self.immediate(Cpu::cmp),
            0xC5 => self.zero_page_read(Cpu::cmp),
            0xD5 => self.zero_page_indexed_read(self.x, Cpu::cmp),
            0xCD => self.absolute_read(Cpu::cmp),
            0xDD => self.absolute_indexed_read(self.x, Cpu::cmp),
            0xD9 => self.absolute_indexed_read(self.y, Cpu::cmp),
            0xC1 => self.indexed_x_read(Cpu::cmp),
            0xD1 => self.indexed_y_read(Cpu::cmp),
            // CPX - Compare X Register
            0xE0 => self.immediate(Cpu::cpx),
            0xE4 => self.zero_page_read(Cpu::cpx),
            0xEC => self.absolute_read(Cpu::cpx),
            // CPY - Compare Y Register
            0xC0 => self.immediate(Cpu::cpy),
            0xC4 => self.zero_page_read(Cpu::cpy),
            0xCC => self.absolute_read(Cpu::cpy),
            // DCP
            0xC3 => self.indexed_x_read_modify_write(Cpu::dcp),
            0xC7 => self.zero_page_read_modify_write(Cpu::dcp),
            0xCF => self.absolute_read_modify_write(Cpu::dcp),
            0xD3 => self.indexed_y_read_modify_write(Cpu::dcp),
            0xD7 => self.zero_page_indexed_read_modify_write(self.x, Cpu::dcp),
            0xDB => self.absolute_indexed_read_modify_write(self.y, Cpu::dcp),
            0xDF => self.absolute_indexed_read_modify_write(self.x, Cpu::dcp),
            // DEC - Decrement Memory
            0xC6 => self.zero_page_read_modify_write(Cpu::dec),
            0xD6 => self.zero_page_indexed_read_modify_write(self.x, Cpu::dec),
            0xCE => self.absolute_read_modify_write(Cpu::dec),
            0xDE => self.absolute_indexed_read_modify_write(self.x, Cpu::dec),
            // DEX - Decrement X Register
            0xCA => self.implied(Cpu::dex),
            // DEY - Decrement Y Register
            0x88 => self.implied(Cpu::dey),
            // EOR - Exclusive OR
            0x49 => self.immediate(Cpu::eor),
            0x45 => self.zero_page_read(Cpu::eor),
            0x55 => self.zero_page_indexed_read(self.x, Cpu::eor),
            0x4D => self.absolute_read(Cpu::eor),
            0x5D => self.absolute_indexed_read(self.x, Cpu::eor),
            0x59 => self.absolute_indexed_read(self.y, Cpu::eor),
            0x41 => self.indexed_x_read(Cpu::eor),
            0x51 => self.indexed_y_read(Cpu::eor),
            // IGN
            0x04 | 0x44 | 0x64 => self.zero_page_read(Cpu::ign),
            0x0C => self.absolute_read(Cpu::ign),
            0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => self.zero_page_indexed_read(self.x, Cpu::ign),
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => self.absolute_indexed_read(self.x, Cpu::ign),
            // INC - Increment Memory
            0xE6 => self.zero_page_read_modify_write(Cpu::inc),
            0xF6 => self.zero_page_indexed_read_modify_write(self.x, Cpu::inc),
            0xEE => self.absolute_read_modify_write(Cpu::inc),
            0xFE => self.absolute_indexed_read_modify_write(self.x, Cpu::inc),
            // INX - Increment X Register
            0xE8 => self.implied(Cpu::inx),
            // INY - Increment Y Register
            0xC8 => self.implied(Cpu::iny),
            // ISC
            0xE3 => self.indexed_x_read_modify_write(Cpu::isb),
            0xE7 => self.zero_page_read_modify_write(Cpu::isb),
            0xEF => self.absolute_read_modify_write(Cpu::isb),
            0xF3 => self.indexed_y_read_modify_write(Cpu::isb),
            0xF7 => self.zero_page_indexed_read_modify_write(self.x, Cpu::isb),
            0xFB => self.absolute_indexed_read_modify_write(self.y, Cpu::isb),
            0xFF => self.absolute_indexed_read_modify_write(self.x, Cpu::isb),
            // JMP - Jump
            0x4C => self.absolute_jmp(), //jmp(Cpu::absolute),
            0x6C => self.indirect_jmp(), //jmp(Cpu::indirect),
            // JSR - Jump to Subroutine
            0x20 => self.jsr(),//jsr(Cpu::absolute),
            // LAX
            0xA3 => self.indexed_x_read(Cpu::lax),
            0xA7 => self.zero_page_read(Cpu::lax),
            0xAF => self.absolute_read(Cpu::lax),
            0xB3 => self.indexed_y_read(Cpu::lax),
            0xB7 => self.zero_page_indexed_read(self.y, Cpu::lax),
            0xBF => self.absolute_indexed_read(self.y, Cpu::lax),
            // LDA - Load Accumulator
            0xA9 => self.immediate(Cpu::lda),
            0xA5 => self.zero_page_read(Cpu::lda),
            0xB5 => self.zero_page_indexed_read(self.x, Cpu::lda),
            0xAD => self.absolute_read(Cpu::lda), //lda(Cpu::absolute),
            0xBD => self.absolute_indexed_read(self.x, Cpu::lda),
            0xB9 => self.absolute_indexed_read(self.y, Cpu::lda),
            0xA1 => self.indexed_x_read(Cpu::lda),
            0xB1 => self.indexed_y_read(Cpu::lda),
            // LDX - Load X Register
            0xA2 => self.immediate(Cpu::ldx),
            0xA6 => self.zero_page_read(Cpu::ldx),
            0xB6 => self.zero_page_indexed_read(self.y, Cpu::ldx),
            0xAE => self.absolute_read(Cpu::ldx),
            0xBE => self.absolute_indexed_read(self.y, Cpu::ldx),
            // LDY - Load Y Register
            0xA0 => self.immediate(Cpu::ldy),
            0xA4 => self.zero_page_read(Cpu::ldy),
            0xB4 => self.zero_page_indexed_read(self.x, Cpu::ldy),
            0xAC => self.absolute_read(Cpu::ldy),
            0xBC => self.absolute_indexed_read(self.x, Cpu::ldy),
            // LSR - Logical Shift Right
            0x4A => self.accumulator(Cpu::lsr),
            0x46 => self.zero_page_read_modify_write(Cpu::lsr),
            0x56 => self.zero_page_indexed_read_modify_write(self.x, Cpu::lsr),
            0x4E => self.absolute_read_modify_write(Cpu::lsr),
            0x5E => self.absolute_indexed_read_modify_write(self.x, Cpu::lsr),
            // NOP - No Operation
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => self.implied(Cpu::nop),
            // ORA - Logical Inclusive OR
            0x09 => self.immediate(Cpu::ora),
            0x05 => self.zero_page_read(Cpu::ora),
            0x15 => self.zero_page_indexed_read(self.x, Cpu::ora),
            0x0D => self.absolute_read(Cpu::ora),
            0x1D => self.absolute_indexed_read(self.x, Cpu::ora),
            0x19 => self.absolute_indexed_read(self.y, Cpu::ora),
            0x01 => self.indexed_x_read(Cpu::ora),
            0x11 => self.indexed_y_read(Cpu::ora),
            // PHA - Push Accumulator
            0x48 => self.pha(),
            // PHP - Push Processor Status
            0x08 => self.php(),
            // PLA - Pull Accumulator
            0x68 => self.pla(),
            // PLP - Pull Processor Status
            0x28 => self.plp(),
            // RLA
            0x23 => self.indexed_x_read_modify_write(Cpu::rla),
            0x27 => self.zero_page_read_modify_write(Cpu::rla),
            0x2F => self.absolute_read_modify_write(Cpu::rla),
            0x33 => self.indexed_y_read_modify_write(Cpu::rla),
            0x37 => self.zero_page_indexed_read_modify_write(self.x, Cpu::rla),
            0x3B => self.absolute_indexed_read_modify_write(self.y, Cpu::rla),
            0x3F => self.absolute_indexed_read_modify_write(self.x, Cpu::rla),
            // ROL - Rotate Left
            0x2A => self.accumulator(Cpu::rol),
            0x26 => self.zero_page_read_modify_write(Cpu::rol),
            0x36 => self.zero_page_indexed_read_modify_write(self.x, Cpu::rol),
            0x2E => self.absolute_read_modify_write(Cpu::rol),
            0x3E => self.absolute_indexed_read_modify_write(self.x, Cpu::rol),
            // ROR - Rotate Right
            0x6A => self.accumulator(Cpu::ror),
            0x66 => self.zero_page_read_modify_write(Cpu::ror),
            0x76 => self.zero_page_indexed_read_modify_write(self.x, Cpu::ror),
            0x6E => self.absolute_read_modify_write(Cpu::ror),
            0x7E => self.absolute_indexed_read_modify_write(self.x, Cpu::ror),
            // RRA
            0x63 => self.indexed_x_read_modify_write(Cpu::rra),
            0x67 => self.zero_page_read_modify_write(Cpu::rra),
            0x6F => self.absolute_read_modify_write(Cpu::rra),
            0x73 => self.indexed_y_read_modify_write(Cpu::rra),
            0x77 => self.zero_page_indexed_read_modify_write(self.x, Cpu::rra),
            0x7B => self.absolute_indexed_read_modify_write(self.y, Cpu::rra),
            0x7F => self.absolute_indexed_read_modify_write(self.x, Cpu::rra),
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
            0xE9 | 0xEB => self.immediate(Cpu::sbc),
            0xE5 => self.zero_page_read(Cpu::sbc),
            0xF5 => self.zero_page_indexed_read(self.x, Cpu::sbc),
            0xED => self.absolute_read(Cpu::sbc),
            0xFD => self.absolute_indexed_read(self.x, Cpu::sbc),
            0xF9 => self.absolute_indexed_read(self.y, Cpu::sbc),
            0xE1 => self.indexed_x_read(Cpu::sbc),
            0xF1 => self.indexed_y_read(Cpu::sbc),
            // SEC - Set Carry Flag
            0x38 => self.implied(Cpu::sec),
            // SED - Set Decimal Flag
            0xF8 => self.implied(Cpu::sed),
            // SEI - Set Interrupt Disable
            0x78 => self.implied(Cpu::sei),
            // SLO
            0x03 => self.indexed_x_read_modify_write(Cpu::slo),
            0x07 => self.zero_page_read_modify_write(Cpu::slo),
            0x0F => self.absolute_read_modify_write(Cpu::slo),
            0x13 => self.indexed_y_read_modify_write(Cpu::slo),
            0x17 => self.zero_page_indexed_read_modify_write(self.x, Cpu::slo),
            0x1B => self.absolute_indexed_read_modify_write(self.y, Cpu::slo),
            0x1F => self.absolute_indexed_read_modify_write(self.x, Cpu::slo),
            // SRE
            0x43 => self.indexed_x_read_modify_write(Cpu::sre),
            0x47 => self.zero_page_read_modify_write(Cpu::sre),
            0x4f => self.absolute_read_modify_write(Cpu::sre),
            0x53 => self.indexed_y_read_modify_write(Cpu::sre),
            0x57 => self.zero_page_indexed_read_modify_write(self.x, Cpu::sre),
            0x5B => self.absolute_indexed_read_modify_write(self.y, Cpu::sre),
            0x5F => self.absolute_indexed_read_modify_write(self.x, Cpu::sre),
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
            0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => self.immediate(Cpu::skb),
            // TAX - Transfer Accumulator to X
            0xAA => self.implied(Cpu::tax),
            // TAY - Transfer Accumulator to Y
            0xA8 => self.implied(Cpu::tay),
            // TSX - Transfer Stack Pointer to X
            0xBA => self.implied(Cpu::tsx),
            // TXA - Transfer X to Accumulator
            0x8A => self.implied(Cpu::txa),
            // TXS - Transfer X to Stack Pointer
            0x9A => self.implied(Cpu::txs),
            // TYA - Transfer Y to Accumulator
            0x98 => self.implied(Cpu::tya),
            _ => {},
        }
    }

    pub fn new(mut bus: Bus) -> Self {
        let mut start_addr = bus.read(0xFFFC) as u16;//0xC000;//bus.read_u16(0xFFFC);
        start_addr |= (bus.read(0xFFFD) as u16) << 8;
        //print!("start: 0x{:x}", start_addr);
        Self {
            a: 0,
            pc: start_addr,
            s: 0xFD,
            x: 0,
            y: 0,
            p: 0x24,
            bus,
        }
    }
}

