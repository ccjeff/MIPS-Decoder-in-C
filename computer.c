#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */ 

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem-0x00001000));         //so I made this edit here, because you're fetching from the memory for instructions
                                                            //but I stored it where it was supposed to be stored (aka in the actually memory)
                                                            //this change makes where I stored words (from sw) actually be reflected in what
                                                            //their values are.

    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Separating opCode and rest of instruction */
    unsigned int restOfInstr = 0, mask = 33554432U;
    int i;
    for(i=0;i<26;i++) {
        restOfInstr |= (instr & mask);
        mask >>= 1;
    }
    instr >>= 26;
    /*setting op code*/
    d->op = instr; 

    //printf("opCode: %u\nrestOfInstr: %u\n",d->op,restOfInstr);

    /*if the opCode is 2 or 3 we have a J instruction*/
    if ( (d->op==2) || (d->op==3) ) {
        d->type = J;
        mask = 4026531840U; // 1111 0000 .... 0000
        d->regs.j.target = restOfInstr;
        d->regs.j.target <<= 2;
        d->regs.j.target |= (mask & mips.pc);
        //printf("Target address after process: %08x\n",d->regs.j.target);

    /*if the opCode is zero we have an R instruction*/
    } else if(!d->op) {

        d->type = R;
        mask = 63U; //111111 needed to get first 6 bits

        d->regs.r.funct = restOfInstr & mask;
        mask >>= 1;
        restOfInstr >>= 6;
        d->regs.r.shamt = restOfInstr & mask;
        restOfInstr >>= 5;
        d->regs.r.rd = restOfInstr & mask;
        restOfInstr >>= 5;
        d->regs.r.rt = restOfInstr & mask;
        restOfInstr >>= 5;
        d->regs.r.rs = restOfInstr & mask;

        //printf("funct: %i\nshamt: %i\nrd: %i\nrt: %i\nrs: %i\n",d->regs.r.funct,d->regs.r.shamt,d->regs.r.rd,d->regs.r.rt,d->regs.r.rs);
        rVals->R_rs = mips.registers[d->regs.r.rs];
        rVals->R_rt = mips.registers[d->regs.r.rt];
        rVals->R_rd = mips.registers[d->regs.r.rd];
    } else {
        d->type = I;
        unsigned int immediateSign = 1;
        mask = 65535U; //16 1's to extract immediate

        d->regs.i.addr_or_immed = mask & restOfInstr;
        restOfInstr >>= 15;
        immediateSign &= restOfInstr; //extract sign to extend
        restOfInstr >>= 1;
        if(immediateSign){ //our immediate is negative
            d->regs.i.addr_or_immed |= 4294901760U; //16 1's in the upper half
        }
        mask >>= 11;
        d->regs.i.rt = mask & restOfInstr;
        restOfInstr >>= 5;
        d->regs.i.rs = mask & restOfInstr;
        //printf("Sign-extended addr_or_immed: %i\nrt: %i\nrs: %i\n",d->regs.i.addr_or_immed,d->regs.i.rt,d->regs.i.rs);
        rVals->R_rs = mips.registers[d->regs.i.rs];
        rVals->R_rt = mips.registers[d->regs.i.rt];
    }
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    /* Your code goes here */
    
    if(!d->op){
        switch (d->regs.r.funct){
            case 0x21: //addu
                printf("addu\t$%i, $%i, $%i\n",d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
                break;
            case 0x23: //subu
                printf("subu\t$%i, $%i, $%i\n",d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
                break;
            case 0x0: //sll
                printf("sll\t$%i, $%i, $%i\n",d->regs.r.rd,d->regs.r.rt,d->regs.r.shamt);
                break;
            case 0x2: //srl 
                printf("srl\t$%i, $%i, $%i\n",d->regs.r.rd,d->regs.r.rt,d->regs.r.shamt);
                break;
            case 0x24: //and 
                printf("and\t$%i, $%i, $%i\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
                break;
            case 0x25: //or 
                printf("or\t$%i, $%i, $%i\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
                break;
            case 0x2a: //slt
                printf("slt\t$%i, $%i, $%i\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
                break;
            case 0x8: //jr 
                printf("jr\t$%i\n", d->regs.r.rs);
                break;
            default :
                exit(0);
        }
    } else {
        switch (d->op){
            case 0x9: //addiu
                printf("addiu\t$%i, $%i, %i\n",d->regs.i.rt,d->regs.i.rs,d->regs.i.addr_or_immed);
                break;
            case 0xc: //andi
                printf("andi\t$%i, $%i, %i\n",d->regs.i.rt,d->regs.i.rs,d->regs.i.addr_or_immed);
                break;
            case 0xd: //ori
                printf("ori\t$%i, $%i, %i\n",d->regs.i.rt,d->regs.i.rs,d->regs.i.addr_or_immed);
                break;
            case 0xf: //lui
                printf("lui\t$%i, $%i, 0x%x\n",d->regs.i.rt,d->regs.i.rs,d->regs.i.addr_or_immed);
                break;
            case 0x4: //beq
                printf("beq\t$%i, $%i, 0x%08x\n",d->regs.i.rs,d->regs.i.rt,((d->regs.i.addr_or_immed<<2)+mips.pc+4));
                break;
            case 0x5: //bne
                printf("bne\t$%i, $%i, 0x%08x\n",d->regs.i.rs,d->regs.i.rt,((d->regs.i.addr_or_immed<<2)+mips.pc+4));
                break;
            case 0x2: //j
                printf("j\t0x%08x\n", d->regs.j.target);
                break;
            case 0x3: //jal
                printf("jal\t0x%08x\n", d->regs.j.target);
                break;
            case 0x23: //lw
                printf("lw\t$%i, %i($%i)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs);
                break;
            case 0x2b: //sw
                printf("sw\t$%i, %i($%i)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs);
                break;
            default :
                exit(0);
        }
    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    if(!d->op){
        switch (d->regs.r.funct){
            case 0x21: //addu
                return rVals->R_rs + rVals->R_rt;
                break;
            case 0x23: //subu
                return rVals->R_rs - rVals->R_rt;
                break;
            case 0x00: //sll
                return rVals->R_rt << d->regs.r.shamt;
                break;
            case 0x02: //srl 
                return rVals->R_rt >> d->regs.r.shamt;
                break;
            case 0x24: //and 
                return rVals->R_rs & rVals->R_rt;
                break;
            case 0x25: //or 
                return rVals->R_rs | rVals->R_rt;
                break;
            case 0x2a: //slt
                return (rVals->R_rs < rVals->R_rt)? 1 : 0;
                break;
            case 0x08: //jr
                return rVals->R_rs;
                break;
        }
    } else {
        switch (d->op){
            case 0x9: //addiu
                return rVals->R_rs + d->regs.i.addr_or_immed;
                break;
            case 0xc: //andi
                return rVals->R_rs & d->regs.i.addr_or_immed;
                break;
            case 0xd: //ori
                return rVals->R_rs | d->regs.i.addr_or_immed;
                break;
            case 0xf: //lui
                return d->regs.i.addr_or_immed << 16;
                break;
            case 0x4: //beq
                return rVals->R_rs == rVals->R_rt;
                break;
            case 0x5: //bne
                return rVals->R_rs != rVals->R_rt;
                break;
            case 0x2: //j
                return d->regs.j.target;
                break;
            case 0x3: //jal
                return d->regs.j.target;
                break;
            case 0x23: //lw
                return rVals->R_rs + d->regs.i.addr_or_immed;
                break;
            case 0x2b: //sw
                return rVals->R_rs + d->regs.i.addr_or_immed;
                break;
        }
    }
  return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    mips.pc+=4;
    /* Your code goes here */
    if(!d->op){
        if (d->regs.r.funct == 0x08) { //jr
            mips.pc = val;
        }
    } else {
        switch (d->op){
            case 0x4: //beq
                if(val) { //rt and rs are equal
                    mips.pc += d->regs.i.addr_or_immed << 2;
                }
                break;
            case 0x5: //bne
                if(!val) { //rt and rs are NOT equal
                    mips.pc += d->regs.i.addr_or_immed << 2;
                }                
                break;
            case 0x2: //j
                mips.pc = val;
                break;
            case 0x3: //jal
                mips.pc = val;
                break;
           
        }
    }

}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */
    unsigned int memoryIndex;
    switch (d->op) { 
        case 0x23: //lw
            if (val < 0x00403fff && val >= 0x00401000 && val%4 == 0)
            {
                *changedMem = -1;
                memoryIndex = ((val-0x00401000)/4);
                return mips.memory[memoryIndex];
            } else
            {
                printf("Memory Access Exception at 0x%08x: address 0x%08x", mips.pc, val);
                exit(0);
            }    
            break;
        case 0x2b: //sw
            if (val < 0x00403fff && val >= 0x00401000 && val%4 == 0)
            {
                memoryIndex = ((val-0x00401000)>>2);
                mips.memory[memoryIndex] = mips.registers[d->regs.i.rt];
                *changedMem = val;            
            } else
            {
                printf("Memory Access Exception at 0x%08x: address 0x%08x", mips.pc, val);
                exit(0);
            }
            return val;
            break;
        default :
            *changedMem = -1;
            return val;
    }
    return -777;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
    if(!d->op){
        switch (d->regs.r.funct){
            case 0x21: //addu
                mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
                break;
            case 0x23: //subu
                mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
                break;
            case 0x00: //sll
                mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
                break;
            case 0x02: //srl 
                 mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
                break;
            case 0x24: //and 
                 mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
                break;
            case 0x25: //or 
                 mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
                break;
            case 0x2a: //slt
                 mips.registers[d->regs.r.rd] = val;
                *changedReg = d->regs.r.rd;
                break;
            case 0x08: //jr
                *changedReg = -1;
                //does not write back
                break;
        }
    } else {
        switch (d->op){
            case 0x9: //addiu
                mips.registers[d->regs.i.rt] = val;
                *changedReg = d->regs.i.rt;
                break;
            case 0xc: //andi
                mips.registers[d->regs.i.rt] = val;
                *changedReg = d->regs.i.rt;
                break;
            case 0xd: //ori
                mips.registers[d->regs.i.rt] = val;
                *changedReg = d->regs.i.rt;
                break;
            case 0xf: //lui
                mips.registers[d->regs.i.rt] = val;
                *changedReg = d->regs.i.rt;
                break;
            case 0x4: //beq
                *changedReg = -1;
                break;
            case 0x5: //bne
                *changedReg = -1;
                break;
            case 0x2: //j
                *changedReg = -1;
                break;
            case 0x3: //jal
                mips.registers[31] = mips.pc-4;
                *changedReg = 31;
                break;
            case 0x23: //lw
                mips.registers[d->regs.i.rt] = val;
                *changedReg = d->regs.i.rt;
                break;
            case 0x2b: //sw
                *changedReg = -1;
                break;
        }
    }

}