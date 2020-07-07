struct Bus {

}

impl Bus {
    fn read(&mut self, addr: u16) -> u8 {
        todo!();
    }

    fn write(&mut self, addr: u16, val: u8) {
        todo!();
    }

    fn clock(&mut self) {
        todo!();
    }
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

    fn stack_dec(&mut self) {
        self.s -= 1;
        self.bus.clock();
    }

    fn pcl(&self) -> u8 {
        self.pc as u8
    }

    fn pch(&self) -> u8 {
        (self.pc >> 8) as u8
    }

    fn pcl_set(&mut self, b: u8) {
        self.pc = (self.pc & 0xff00) | (b as u16);
    }

    fn pch_set(&mut self, b: u8) {
        self.pc = (self.pc & 0xff) | ((b as u16) << 8);
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
        self.p = self.stack_pull();
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
    }
    
    // pha, php
    fn pha(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.stack_push(self.a); // or self.p
        self.s -= 1;
    }
    
    // pla, plp
    fn pla(&mut self) {
        // 2
        self.bus.read(self.pc);
        // 3
        self.s += 1;
        // 4
        self.a = self.stack_pull();
    }
    
    fn jsr(&mut self) {
        // 2
        let abl = self.fetch_u8();
        // 3
        self.stack_dec();
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
        let val = self.bus.read(self.pc);
        self.a = func(self, val);
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
    /*
    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn absolute_read_modify_write() {
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
        todo!(); //do the operation on 'val'
        // 6
        self.bus.write(addr, val);
    }
    */
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
    /*
    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn zero_page_read_modify_write() {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        let val = self.bus.read(addr);
        // 4
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        todo!(); //do the operation on 'val'
        // 5
        self.bus.write(addr, val);
    }
    */
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
        let addr = self.fetch_u8() as u16;
        // 3
        let addr = self.bus.read(addr);
        let addr = addr.wrapping_add(index);
        // 4
        let val = self.bus.read(addr as u16);
        func(self, val);
    }
    /*
    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn zero_page_indexed_read_modify_write() {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        let addr = self.bus.read(addr);
        let addr = addr.wrapping_add(self.x);
        // 4
        let val = self.bus.read(addr as u16)
        // 5
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        todo!(); //do the operation on 'val'
        // 6
        self.bus.write(addr, val);
    }
    */
    // STA, STX, STY, SAX
    fn zero_page_indexed_write(&mut self, index: u8, reg_val: u8) {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        let addr = self.bus.read(addr);
        let addr = addr.wrapping_add(index);
        // 4
        self.bus.write(addr as u16, reg_val);
    }
    
    // LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, LAE, SHS, NOP
    fn absolute_indexed_read(&mut self, index: u8, func: fn (&mut Cpu, u8)) {
        // 2
        let l = self.fetch_u8();
        // 3
        let h = self.fetch_u8() as u16;
        let l = l.wrapping_add(index) as u16;
        // 4
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        todo!(); // page boundary was crossed
        // 5
        let val = self.bus.read(addr);
        func(self, val);
    }
    /*
    // ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
    fn absolute_indexed_read_modify_write() {
        // 2
        let l = self.fetch_u8() as u16;
        // 3
        let h = self.fetch_u8() as u16;
        let l = l.wrapping_add(self.x);
        // 4
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        todo!(); // page boundary was crossed
        // 5
        let val = self.bus.read(addr);
        // 6
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        todo!(); //do the operation on 'val'
        // 7
        self.bus.write(addr, val);
    }
    */
    // STA, STX, STY, SHA, SHX, SHY
    fn absolute_indexed_write(&mut self, index: u8, reg_val: u8) {
        // 2
        let l = self.fetch_u8();
        // 3
        let h = self.fetch_u8() as u16;
        let l = l.wrapping_add(index)  as u16;
        // 4
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        let val = func(self, val);
        todo!(); // page boundary was crossed
        // 5
        self.bus.write(addr, val);
    }
    
    // BCC, BCS, BNE, BEQ, BPL, BMI, BVC, BVS
    fn relative(&mut self, condition: bool) {
        // 2
        let operand = self.fetch_u8();
        // 3
        self.bus.read(self.pc);

        if true {
            self.pcl += operand;
        } else {
            self.pc += 1;
        }
        // 4
        todo!();
    }
    
    // LDA, ORA, EOR, AND, ADC, CMP, SBC, LAX
    fn indexed_x_read(&mut self, func: fn (&mut Cpu, u8)) {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        let pointer = self.bus.read(addr);
        let pointer = pointer.wrapping_add(self.x);
        // 4
        let l = self.bus.read(pointer as u16) as u16;
        // 5
        let h = self.bus.read(pointer.wrapping_add(1) as u16) as u16;
        // 6
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        func(self, val);
    }
    /*
    // SLO, SRE, RLA, RRA, ISB, DCP
    fn indexed_x_read_modify_write() {
        // 2
        let addr = self.fetch_u8();
        // 3
        let pointer = self.bus.read(addr);
        let pointer = pointer.wrapping_add(self.x);
        // 4
        let l = self.bus.read(pointer as u16) as u16;
        // 5
        let h = self.bus.read(pointer.wrapping_add(1) as u16) as u16;
        // 6
        let addr = (h << 8) | l;
        let val = self.bus.read(addr);
        // 7
        // write the value back to effective address, and do the operation on it
        self.bus.write(addr, val);
        todo!(); //do the operation on 'val'
        // 8
        self.bus.write(addr, val);
    }
    */
    // STA, SAX
    fn indexed_x_write(&mut self, reg_val: u8) {
        // 2
        let addr = self.fetch_u8() as u16;
        // 3
        let pointer = self.bus.read(addr);
        let pointer = pointer.wrapping_add(self.x);
        // 4
        let l = self.bus.read(pointer as u16) as u16;
        // 5
        let h = self.bus.read(pointer.wrapping_add(1) as u16) as u16;
        // 6
        let addr = (h << 8) | l;
        self.bus.write(addr, reg_val);
    }

    // LDA, EOR, AND, ORA, ADC, SBC, CMP
    fn indexed_y_read(&mut self, func: fn (&mut Cpu, u8)) {
        // 2
        let pointer_addr = self.fetch_u8() as u16;
        // 3
        let pointer_l = self.bus.read(pointer_addr);
        // 4
        let pointer_h = self.bus.read((pointer_addr + 1) & 0xff) as u16;
        let pointer_l = pointer_l.wrapping_add(self.y) as u16;
        // 5
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        let pointer = (pointer_h << 8) | pointer_l;
        let val = self.bus.read(pointer);
        // 6
        // + This cycle will be executed only if the effective address
        // was invalid during cycle #5, i.e. page boundary was crossed.
        self.bus.read(pointer);
    }
    /*
    // SLO, SRE, RLA, RRA, ISB, DCP
    fn indexed_y_read_modify_write()
    {
        // 2
        let pointer_addr = self.fetch_u8();
        // 3
        let pointer_l = self.bus.read(addr);
        // 4
        let pointer_h = self.bus.read(addr) as u16;
        let pointer_l = pointer_l.wrapping_add(self.y) as u16;
        // 5
        // The high byte of the effective address may be invalid
        // at this time, i.e. it may be smaller by $100.
        let pointer = (pointer_h << 8) | pointer_l;
        let val = self.bus.read(pointer);
        // 6
        let val = self.bus.read(pointer);
        // 7
        self.bus.write(pointer, val);
        todo!();
        // do the operation on it
        // 8
        self.bus.write(pointer, val);
    }
    */
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
        let val = self.bus.read(pointer);
        // 6
        self.bus.write(pointer, reg_val);
    }

    fn indirect_jmp(&mut self) {
        // 2
        let l = self.fetch_u8() as u16;
        // 3
        let h = self.fetch_u8() as u16;
        // 4
        let addr = (h << 8) | l;
        let low = self.bus.read(addr);
        // 5
        // The PCH will always be fetched from the same page
        // than PCL, i.e. page boundary crossing is not handled.
        self.pc = (self.bus.read(addr + 1) as u16) << 8; // pch
        self.pc |= low as u16;
    }

    //================================================================
    fn adc(&mut self, byte: u8) {
        todo!();
    }

    fn and(&mut self, byte: u8) {
        todo!();
    }

    fn cmp(&mut self, byte: u8) {
        todo!();
    }

    fn eor(&mut self, byte: u8) {
        todo!();
    }

    fn lda(&mut self, byte: u8) {
        todo!();
    }

    fn ldy(&mut self, byte: u8) {
        todo!();
    }

    fn ldx(&mut self, byte: u8) {
        todo!();
    }

    fn ora(&mut self, byte: u8) {
        todo!();
    }

    fn sbc(&mut self, byte: u8) {
        todo!();
    }

    fn lax(&mut self, byte: u8) {
        todo!();
    }

    fn sax(&mut self, byte: u8) {
        todo!();
    }

    fn nop(&mut self, byte: u8) {
        todo!();
    }

    fn bit(&mut self, byte: u8) {
        todo!();
    }

    // write
    fn sta(&mut self, byte: u8) {
        todo!();
    }
    //================================================================
    pub fn clock(&mut self) {
        //print!("{:02X}  ", self.pc);
        //self.print_state();

        let instruction = self.fetch_u8();
        //print!("read instruction {:2x}", instruction);
        match instruction {
            // ADC - Add with Carry
            0x69 => self.immediate(Cpu::adc), //  self.adc(Cpu::immediate),
            0x65 => self.zero_page_read(Cpu::adc),
            0x75 => self.zero_page_indexed_read(self.x, Cpu::adc), //adc(Cpu::zero_page_x),
            0x6D => self.absolute_read(Cpu::adc), //adc(Cpu::absolute),
            0x7D => self.absolute_indexed_read(self.x, Cpu::adc), // adc(Cpu::absolute_x),
            0x79 => self.absolute_indexed_read(self.y, Cpu::adc), // adc(Cpu::absolute_y),
            0x61 => self.indexed_x_read(Cpu::adc), //adc(Cpu::indirect_x),
            0x71 => self.indexed_y_read(Cpu::adc), // adc(Cpu::indirect_y),
            // AND - Logical AND
            0x29 => self.immediate(Cpu::and), //and(Cpu::immediate),
            0x25 => self.zero_page_read(Cpu::and), //and(Cpu::zero_page),
            0x35 => self.zero_page_indexed_read(self.x, Cpu::and), //and(Cpu::zero_page_x),
            0x2D => self.absolute_read(Cpu::and), //and(Cpu::absolute),
            0x3D => self.absolute_indexed_read(self.x, Cpu::and), //and(Cpu::absolute_x),
            0x39 => self.absolute_indexed_read(self.y, Cpu::and), //and(Cpu::absolute_y),
            0x21 => self.indexed_x_read(Cpu::and), //and(Cpu::indirect_x),
            0x31 => self.indexed_y_read(Cpu::and), //and(Cpu::indirect_y),
            // ASL - Arithmetic Shift Left
            0x0A => self.asl_acc(),
            0x06 => self.asl(Cpu::zero_page),
            0x16 => self.asl(Cpu::zero_page_x),
            0x0E => self.asl(Cpu::absolute),
            0x1E => self.asl(Cpu::absolute_x),
            // BCC - Branch if Carry Clear
            0x90 => self.bcc(Cpu::relative),
            // BCS - Branch if Carry Set
            0xB0 => self.bcs(Cpu::relative),
            // BEQ - Branch if Equal
            0xF0 => self.beq(Cpu::relative),
            // BIT - Bit Test
            0x24 => self.zero_page_read(Cpu::bit), //bit(Cpu::zero_page),
            0x2C => self.absolute_read(Cpu::bit), //bit(Cpu::absolute),
            // BMI - Branch if Minus
            0x30 => self.bmi(Cpu::relative),
            // BNE - Branch if Not Equal
            0xD0 => self.bne(Cpu::relative),
            // BPL - Branch if Positive
            0x10 => self.bpl(Cpu::relative),
            // BRK - Force Interrupt
            0x00 => self.brk(),
            // BVC - Branch if Overflow Clear
            0x50 => self. bvc(Cpu::relative),
            // BVS - Branch if Overflow Set
            0x70 => self.bvs(Cpu::relative),
            // CLC - Clear Carry Flag
            0x18 => self.clc(),
            // CLD - Clear Decimal Mode
            0xD8 => self.cld(),
            // CLI - Clear Interrupt Disable
            0x58 => self.cli(),
            // CLV - Clear Overflow Flag
            0xB8 => self.clv(),
            // CMP - Compare
            0xC9 => self.immediate(Cpu::cmp), //cmp(Cpu::immediate),
            0xC5 => self.zero_page_read(Cpu::cmp), //cmp(Cpu::zero_page),
            0xD5 => self.zero_page_indexed_read(self.x, Cpu::cmp), //cmp(Cpu::zero_page_x),
            0xCD => self.absolute_read(Cpu::cmp), //cmp(Cpu::absolute),
            0xDD => self.absolute_indexed_read(self.x, Cpu::cmp), //cmp(Cpu::absolute_x),
            0xD9 => self.absolute_indexed_read(self.y, Cpu::cmp), //cmp(Cpu::absolute_y),
            0xC1 => self.indexed_x_read(Cpu::cmp), //cmp(Cpu::indirect_x),
            0xD1 => self.indexed_y_read(Cpu::cmp), //cmp(Cpu::indirect_y),
            // CPX - Compare X Register
            0xE0 => self.cpx(Cpu::immediate),
            0xE4 => self.cpx(Cpu::zero_page),
            0xEC => self.cpx(Cpu::absolute),
            // CPY - Compare Y Register
            0xC0 => self.cpy(Cpu::immediate),
            0xC4 => self.cpy(Cpu::zero_page),
            0xCC => self.cpy(Cpu::absolute),
            // DCP
            0xC3 => self.dcp(Cpu::indirect_x), // DCP (d,X) ($C3 dd; 8 cycles)
            0xC7 => self.dcp(Cpu::zero_page), // DCP d ($C7 dd; 5 cycles)
            0xCF => self.dcp(Cpu::absolute), // DCP a ($CF aa aa; 6 cycles)
            0xD3 => self.dcp(Cpu::indirect_y), // DCP (d),Y ($D3 dd; 8 cycles)
            0xD7 => self.dcp(Cpu::zero_page_x), // DCP d,X ($D7 dd; 6 cycles)
            0xDB => self.dcp(Cpu::absolute_y), // DCP a,Y ($DB aa aa; 7 cycles)
            0xDF => self.dcp(Cpu::absolute_x), // DCP a,X ($DF aa aa; 7 cycles)
            // DEC - Decrement Memory
            0xC6 => self.dec(Cpu::zero_page),
            0xD6 => self.dec(Cpu::zero_page_x),
            0xCE => self.dec(Cpu::absolute),
            0xDE => self.dec(Cpu::absolute_x),
            // DEX - Decrement X Register
            0xCA => self.dex(),
            // DEY - Decrement Y Register
            0x88 => self.dey(),
            // EOR - Exclusive OR
            0x49 => self.immediate(Cpu::eor), //eor(Cpu::immediate),
            0x45 => self.zero_page_read(Cpu::eor), //eor(Cpu::zero_page),
            0x55 => self.zero_page_indexed_read(self.x, Cpu::eor), //eor(Cpu::zero_page_x),
            0x4D => self.absolute_read(Cpu::eor), //eor(Cpu::absolute),
            0x5D => self.absolute_indexed_read(self.x, Cpu::eor), //eor(Cpu::absolute_x),
            0x59 => self.absolute_indexed_read(self.y, Cpu::eor),  //eor(Cpu::absolute_y),
            0x41 => self.indexed_x_read(Cpu::eor), //eor(Cpu::indirect_x),
            0x51 => self.indexed_y_read(Cpu::eor), //eor(Cpu::indirect_y),
            // IGN
            0x04 | 0x44 | 0x64 => self.ign(Cpu::zero_page),
            0x0C => self.ign(Cpu::absolute),
            0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => self.ign(Cpu::zero_page_x),
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => self.ign(Cpu::absolute_x),
            // INC - Increment Memory
            0xE6 => self.inc(Cpu::zero_page),
            0xF6 => self.inc(Cpu::zero_page_x),
            0xEE => self.inc(Cpu::absolute),
            0xFE => self.inc(Cpu::absolute_x),
            // INX - Increment X Register
            0xE8 => self.inx(),
            // INY - Increment Y Register
            0xC8 => self.iny(),
            // ISC
            0xE3 => self.isc(Cpu::indirect_x), // ISC (d,X) ($E3 dd; 8 cycles)
            0xE7 => self.isc(Cpu::zero_page), // ISC d ($E7 dd; 5 cycles)
            0xEF => self.isc(Cpu::absolute),  // ISC a ($EF aa aa; 6 cycles)
            0xF3 => self.isc(Cpu::indirect_y), // ISC (d),Y ($F3 dd; 8 cycles)
            0xF7 => self.isc(Cpu::zero_page_x), // ISC d,X ($F7 dd; 6 cycles)
            0xFB => self.isc(Cpu::absolute_y), // ISC a,Y ($FB aa aa; 7 cycles)
            0xFF => self.isc(Cpu::absolute_x), // ISC a,X ($FF aa aa; 7 cycles)
            // JMP - Jump
            0x4C => self.absolute_jmp(), //jmp(Cpu::absolute),
            0x6C => self.indirect_jmp(), //jmp(Cpu::indirect),
            // JSR - Jump to Subroutine
            0x20 => self.jsr(),//jsr(Cpu::absolute),
            // LAX
            0xA3 => self.indexed_x_read(Cpu::lax), //lax(Cpu::indirect_x),
            0xA7 => self.zero_page_read(Cpu::lax), //lax(Cpu::zero_page), //LAX d ($A7 dd; 3 cycles)
            0xAF => self.absolute_read(Cpu::lax), //lax(Cpu::absolute), // LAX a ($AF aa aa; 4 cycles)
            0xB3 => self.indexed_y_read(Cpu::lax), //lax(Cpu::indirect_y), // LAX (d),Y ($B3 dd; 5 cycles)
            0xB7 => self.zero_page_indexed_read(self.y, Cpu::lax), //lax(Cpu::zero_page_y), // LAX d,Y ($B7 dd; 4 cycles)
            0xBF => self.absolute_indexed_read(self.y, Cpu::lax), //lax(Cpu::absolute_y), // LAX a,Y ($BF aa aa; 4 cycles)
            // LDA - Load Accumulator
            0xA9 => self.immediate(Cpu::lda), //lda(Cpu::immediate),
            0xA5 => self.zero_page_read(Cpu::lda), //lda(Cpu::zero_page),
            0xB5 => self.zero_page_indexed_read(self.x, Cpu::lda), //lda(Cpu::zero_page_x),
            0xAD => self.absolute_read(Cpu::lda), //lda(Cpu::absolute),
            0xBD => self.absolute_indexed_read(self.x, Cpu::lda), //lda(Cpu::absolute_x),
            0xB9 => self.absolute_indexed_read(self.y, Cpu::lda), //lda(Cpu::absolute_y),
            0xA1 => self.indexed_x_read(Cpu::lda), //lda(Cpu::indirect_x),
            0xB1 => self.indexed_y_read(Cpu::lda), //lda(Cpu::indirect_y),
            // LDX - Load X Register
            0xA2 => self.immediate(Cpu::ldx), //ldx(Cpu::immediate),
            0xA6 => self.zero_page_read(Cpu::ldx), //ldx(Cpu::zero_page),
            0xB6 => self.zero_page_indexed_read(self.y, Cpu::ldx), //ldx(Cpu::zero_page_y),
            0xAE => self.absolute_read(Cpu::ldx), //ldx(Cpu::absolute),
            0xBE => self.absolute_indexed_read(self.y, Cpu::ldx), //ldx(Cpu::absolute_y),
            // LDY - Load Y Register
            0xA0 => self.immediate(Cpu::ldy), //ldy(Cpu::immediate),
            0xA4 => self.zero_page_read(Cpu::ldy), //ldy(Cpu::zero_page),
            0xB4 => self.zero_page_indexed_read(self.x, Cpu::ldy), //ldy(Cpu::zero_page_x),
            0xAC => self.absolute_read(Cpu::ldy), //ldy(Cpu::absolute),
            0xBC => self.absolute_indexed_read(self.x, Cpu::ldy), //ldy(Cpu::absolute_x),
            // LSR - Logical Shift Right
            0x4A => self.lsr_acc(),
            0x46 => self.lsr(Cpu::zero_page),
            0x56 => self.lsr(Cpu::zero_page_x),
            0x4E => self.lsr(Cpu::absolute),
            0x5E => self.lsr(Cpu::absolute_x),
            // NOP - No Operation
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => self.nop(),
            // ORA - Logical Inclusive OR
            0x09 => self.immediate(Cpu::ora), //ora(Cpu::immediate),
            0x05 => self.zero_page_read(Cpu::ora), //ora(Cpu::zero_page),
            0x15 => self.zero_page_indexed_read(self.x, Cpu::ora), //ora(Cpu::zero_page_x),
            0x0D => self.absolute_read(Cpu::ora), //ora(Cpu::absolute),
            0x1D => self.absolute_indexed_read(self.x, Cpu::ora), //ora(Cpu::absolute_x),
            0x19 => self.absolute_indexed_read(self.y, Cpu::ora), //ora(Cpu::absolute_y),
            0x01 => self.indexed_x_read(Cpu::ora), //ora(Cpu::indirect_x),
            0x11 => self.indexed_y_read(Cpu::ora), //ora(Cpu::indirect_y),
            // PHA - Push Accumulator
            0x48 => self.pha(),
            // PHP - Push Processor Status
            0x08 => self.php(),
            // PLA - Pull Accumulator
            0x68 => self.pla(),
            // PLP - Pull Processor Status
            0x28 => self.plp(),
            // RLA
            0x23 => self.rla(Cpu::indirect_x), // RLA (d,X) ($23 dd; 8 cycles)
            0x27 => self.rla(Cpu::zero_page), // RLA d ($27 dd; 5 cycles)
            0x2F => self.rla(Cpu::absolute), // RLA a ($2F aa aa; 6 cycles)
            0x33 => self.rla(Cpu::indirect_y), // RLA (d),Y ($33 dd; 8 cycles)
            0x37 => self.rla(Cpu::zero_page_x), // RLA d,X ($37 dd; 6 cycles)
            0x3B => self.rla(Cpu::absolute_y), // RLA a,Y ($3B aa aa; 7 cycles)
            0x3F => self.rla(Cpu::absolute_x), // RLA a,X ($3F aa aa; 7 cycles)
            // ROL - Rotate Left
            0x2A => self.rol_acc(),
            0x26 => self.rol(Cpu::zero_page),
            0x36 => self.rol(Cpu::zero_page_x),
            0x2E => self.rol(Cpu::absolute),
            0x3E => self.rol(Cpu::absolute_x),
            // ROR - Rotate Right
            0x6A => self.ror_acc(),
            0x66 => self.ror(Cpu::zero_page),
            0x76 => self.ror(Cpu::zero_page_x),
            0x6E => self.ror(Cpu::absolute),
            0x7E => self.ror(Cpu::absolute_x),
            // RRA
            0x63 => self.rra(Cpu::indirect_x), // RRA (d,X) ($63 dd; 8 cycles)
            0x67 => self.rra(Cpu::zero_page), // RRA d ($67 dd; 5 cycles)
            0x6F => self.rra(Cpu::absolute), // RRA a ($6F aa aa; 6 cycles)
            0x73 => self.rra(Cpu::indirect_y), // RRA (d),Y ($73 dd; 8 cycles)
            0x77 => self.rra(Cpu::zero_page_x), // RRA d,X ($77 dd; 6 cycles)
            0x7B => self.rra(Cpu::absolute_y), // RRA a,Y ($7B aa aa; 7 cycles)
            0x7F => self.rra(Cpu::absolute_x), // RRA a,X ($7F aa aa; 7 cycles)
            // RTI - Return from Interrupt
            0x40 => self.rti(),
            // RTS - Return from Subroutine
            0x60 => self.rts(),
            // SAX
            0x83 => self.indexed_y_read(Cpu::sax), //sax(Cpu::indirect_x), // SAX (d,X) ($83 dd; 6 cycles)
            0x87 => self.zero_page_read(Cpu::sax), //sax(Cpu::zero_page), // SAX d ($87 dd; 3 cycles)
            0x8F => self.absolute_read(Cpu::sax), //sax(Cpu::absolute), // SAX a ($8F aa aa; 4 cycles)
            0x97 => self.zero_page_indexed_read(self.y, Cpu::sax), //sax(Cpu::zero_page_y), // SAX d,Y ($97 dd; 4 cycles)
            // SBC - Subtract with Carry
            0xE9 | 0xEB => self.immediate(Cpu::sbc), //sbc(Cpu::immediate),
            0xE5 => self.zero_page_read(Cpu::sbc), //sbc(Cpu::zero_page),
            0xF5 => self.zero_page_indexed_read(self.x, Cpu::sbc), //sbc(Cpu::zero_page_x),
            0xED => self.absolute_read(Cpu::sbc), //sbc(Cpu::absolute),
            0xFD => self.absolute_indexed_read(self.x, Cpu::sbc), //sbc(Cpu::absolute_x),
            0xF9 => self.absolute_indexed_read(self.y, Cpu::sbc), //sbc(Cpu::absolute_y),
            0xE1 => self.indexed_x_read(Cpu::sbc), //sbc(Cpu::indirect_x),
            0xF1 => self.indexed_y_read(Cpu::sbc), //sbc(Cpu::indirect_y),
            // SEC - Set Carry Flag
            0x38 => self.sec(),
            // SED - Set Decimal Flag
            0xF8 => self.sed(),
            // SEI - Set Interrupt Disable
            0x78 => self.sei(),
            // SLO
            0x03 => self.slo(Cpu::indirect_x), // SLO (d,X) ($03 dd; 8 cycles)
            0x07 => self.slo(Cpu::zero_page), // SLO d ($07 dd; 5 cycles)
            0x0F => self.slo(Cpu::absolute), // SLO a ($0F aa aa; 6 cycles)
            0x13 => self.slo(Cpu::indirect_y), // SLO (d),Y ($13 dd; 8 cycles)
            0x17 => self.slo(Cpu::zero_page_x), // SLO d,X ($17 dd; 6 cycles)
            0x1B => self.slo(Cpu::absolute_y), // SLO a,Y ($1B aa aa; 7 cycles)
            0x1F => self.slo(Cpu::absolute_x), // SLO a,X ($1F aa aa; 7 cycles)
            // SRE
            0x43 => self.sre(Cpu::indirect_x), // SRE (d,X) ($43 dd; 8 cycles)
            0x47 => self.sre(Cpu::zero_page), // SRE d ($47 dd; 5 cycles)
            0x4f => self.sre(Cpu::absolute), // SRE a ($4F aa aa; 6 cycles)
            0x53 => self.sre(Cpu::indirect_y), // SRE (d),Y ($53 dd; 8 cycles)
            0x57 => self.sre(Cpu::zero_page_x), // SRE d,X ($57 dd; 6 cycles)
            0x5B => self.sre(Cpu::absolute_y), // SRE a,Y ($5B aa aa; 7 cycles)
            0x5F => self.sre(Cpu::absolute_x), // SRE a,X ($5F aa aa; 7 cycles)
            // STA - Store Accumulator
            0x85 => self.zero_page_write(self.a), //sta(Cpu::zero_page),
            0x95 => self.zero_page_indexed_write(self.x, self.a), //sta(Cpu::zero_page_x),
            0x8D => self.absolute_write(self.a), //sta(Cpu::absolute),
            0x9D => self.absolute_indexed_write(self.x, self.a), //sta(Cpu::absolute_x),
            0x99 => self.absolute_indexed_write(self.y, self.a), //sta(Cpu::absolute_y),
            0x81 => self.indexed_x_write(self.a), //sta(Cpu::indirect_x),
            0x91 => self.indexed_y_write(self.a), //sta(Cpu::indirect_y),
            // STX - Store X Register
            0x86 => self.zero_page_write(self.x), //stx(Cpu::zero_page),
            0x96 => self.zero_page_indexed_write(self.y, self.x), //stx(Cpu::zero_page_y),
            0x8E => self.absolute_write(self.x), //stx(Cpu::absolute),
            // STY - Store Y Register
            0x84 => self.zero_page_write(self.y), //sty(Cpu::zero_page),
            0x94 => self.zero_page_indexed_write(self.x, self.y), //sty(Cpu::zero_page_x),
            0x8C => self.absolute_write(self.y), //sty(Cpu::absolute),
            // SKB
            0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => self.skb(Cpu::immediate),
            // TAX - Transfer Accumulator to X
            0xAA => self.tax(),
            // TAY - Transfer Accumulator to Y
            0xA8 => self.tay(),
            // TSX - Transfer Stack Pointer to X
            0xBA => self.tsx(),
            // TXA - Transfer X to Accumulator
            0x8A => self.txa(),
            // TXS - Transfer X to Stack Pointer
            0x9A => self.txs(),
            // TYA - Transfer Y to Accumulator
            0x98 => self.tya(),
        }
    }
}

fn main() {

}