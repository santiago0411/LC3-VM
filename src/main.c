#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <signal.h>

#include "platform.h"

typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;

/*
 * DOCS:
 * LC-3: https://en.wikipedia.org/wiki/Little_Computer_3
 * Reference for op code implementations: https://www.jmeiners.com/lc3-vm/supplies/lc3-isa.pdf
 */

void handle_interrupt(int signal)
{
    restore_input_buffering();
    printf("\n");
    exit(-2);
}

enum
{
    R_R0 = 0,
    R_R1,
    R_R2,
    R_R3,
    R_R4,
    R_R5,
    R_R6,
    R_R7,
    R_PC,
    R_COND,
    R_COUNT,
};

typedef enum
{
    OP_BR = 0, /* branch */
    OP_ADD,    /* add  */
    OP_LD,     /* load */
    OP_ST,     /* store */
    OP_JSR,    /* jump register */
    OP_AND,    /* bitwise and */
    OP_LDR,    /* load register */
    OP_STR,    /* store register */
    OP_RTI,    /* unused */
    OP_NOT,    /* bitwise not */
    OP_LDI,    /* load indirect */
    OP_STI,    /* store indirect */
    OP_JMP,    /* jump */
    OP_RES,    /* reserved (unused) */
    OP_LEA,    /* load effective address */
    OP_TRAP    /* execute trap */
} OpCode;

enum
{
    FL_POS = 1 << 0,
    FL_ZRO = 1 << 1,
    FL_NEG = 1 << 2,
};

typedef enum
{
    TRAP_GETC = 0x20,  /* get character from keyboard, not echoed onto the terminal */
    TRAP_OUT = 0x21,   /* output a character */
    TRAP_PUTS = 0x22,  /* output a word string */
    TRAP_IN = 0x23,    /* get character from keyboard, echoed onto the terminal */
    TRAP_PUTSP = 0x24, /* output a byte string */
    TRAP_HALT = 0x25   /* halt the program */
} Trap;

// Memory Mapped Registers
enum
{
    MR_KBSR = 0xFE00, /* keyboard status */
    MR_KBDR = 0xFE02  /* keyboard data */
};

#define MEMORY_MAX (1 << 16)
u16 memory[MEMORY_MAX];
u16 regs[R_COUNT];

u16 swap16(u16 x)
{
    return (x << 8) | (x >> 8);
}

void read_image_file(FILE* file)
{
    // The origin is where in memory the image should be placed
    u16 origin;
    fread(&origin, sizeof(u16), 1, file);
    origin = swap16(origin);

    // We know the maximum size so we only need one fread call
    u16 max_read = MEMORY_MAX - origin;
    u16* p = memory + origin;
    size_t read = fread(p, sizeof(u16), max_read, file);

    // Swap to little endian
    while (read-- > 0)
    {
        *p = swap16(*p);
        p++;
    }
}

bool read_image(const char* image_path)
{
    FILE* file = fopen(image_path, "rb");
    if (!file) return false;
    read_image_file(file);
    fclose(file);
    return true;
}

u16 sign_extend(u16 x, i32 bit_count)
{
    if ((x >> (bit_count - 1)) & 1) {
        x |= (0xFFFF << bit_count);
    }
    return x;
}

void mem_write(u16 address, u16 value)
{
    memory[address] = value;
}

u16 mem_read(u16 address)
{
    if (address == MR_KBSR) {
        if (check_key()) {
            memory[MR_KBSR] = (1 << 15);
            memory[MR_KBDR] = (u16)getchar();
        } else {
            memory[MR_KBSR] = 0;
        }
    }
    return memory[address];
}

void update_flags(u16 r)
{
    if (regs[r] == 0) {
        regs[R_COND] = FL_ZRO;
    } else if ((regs[r] >> 15) == 1) {
        regs[R_COND] = FL_NEG;
    } else {
        regs[R_COND] = FL_POS;
    }
}

void branch(u16 instruction)
{
    /*
     * The condition codes specified by the state of bits [11:9] are tested. If bit [11] is
     * set, N is tested; if bit [11] is clear, N is not tested. If bit [10] is set, Z is tested, etc.
     * If any of the condition codes tested is set, the program branches to the location
     * specified by adding the sign-extended PCoffset9 field to the incremented PC.
     */

    uint16_t pc_offset = sign_extend(instruction & 0x1FF, 9);
    uint16_t cond_flag = (instruction >> 9) & 0x7;
    if (cond_flag & regs[R_COND])
    {
        regs[R_PC] += pc_offset;
    }
}

void add(u16 instruction)
{
    /*
     * If bit [5] is 0, the second source operand is obtained from SR2. If bit [5] is 1, the
     * second source operand is obtained by sign-extending the imm5 field to 16 bits.
     * In both cases, the second source operand is added to the contents of SR1 and the
     * result stored in DR. The condition codes are set, based on whether the result is
     * negative, zero, or positive.
     */

    // Destination register (DR)
    u16 r0 = (instruction >> 9) & 0x7;
    // First operand (SR1)
    u16 r1 = (instruction >> 6) & 0x7;
    // Whether it's immediate mode
    u16 imm_flag = (instruction >> 5) & 0x1;

    if (imm_flag) {
        // In immediate mode, the second value is embedded in the instruction
        u16 imm5 = sign_extend(instruction & 0x1F, 5);
        regs[r0] = regs[r1] + imm5;
    } else {
        // In register mode, the second value is found in a register
        u16 r2 = instruction & 0x7;
        regs[r0] = regs[r1] + regs[r2];
    }

    update_flags(r0);
}

void and(u16 instruction)
{
    /*
     * If bit [5] is 0, the second source operand is obtained from SR2. If bit [5] is 1,
     * the second source operand is obtained by sign-extending the imm5 field to 16
     * bits. In either case, the second source operand and the contents of SR1 are bitwise ANDed,
     * and the result stored in DR. The condition codes are set, based on
     * whether the binary value produced, taken as a 2’s complement integer, is negative,
     * zero, or positive.
     */

    // Destination register (DR)
    u16 r0 = (instruction >> 9) & 0x7;
    // First operand (SR1)
    u16 r1 = (instruction >> 6) & 0x7;
    // Whether it's immediate mode
    u16 imm_flag = (instruction >> 5) & 0x1;

    if (imm_flag) {
        // In immediate mode, the second value is embedded in the instruction
        u16 imm5 = sign_extend(instruction & 0x1F, 5);
        regs[r0] = regs[r1] & imm5;
    } else {
        // In register mode, the second value is found in a register
        u16 r2 = instruction & 0x7;
        regs[r0] = regs[r1] & regs[r2];
    }

    update_flags(r0);
}

void not(u16 instruction)
{
    // Destination register (DR)
    u16 r0 = (instruction >> 9) & 0x7;
    // First operand (SR1)
    u16 r1 = (instruction >> 6) & 0x7;

    regs[r0] = ~regs[r1];
    update_flags(r0);
}

void jump(u16 instruction)
{
    /*
     * The program unconditionally jumps to the location specified by the contents of
     * the base register. Bits [8:6] identify the base register.
     *
     * The RET instruction is a special case of the JMP instruction. The PC is loaded
     * with the contents of R7, which contains the linkage back to the instruction
     * following the subroutine call instruction.
     */

    u16 r1 = (instruction >> 6) & 0x7;
    regs[R_PC] = regs[r1];
}

void jump_subroutine(u16 instruction)
{
    /*
     * First, the incremented PC is saved in R7. This is the linkage back to the calling
     * routine. Then the PC is loaded with the address of the first instruction of the
     * subroutine, causing an unconditional jump to that address. The address of the
     * subroutine is obtained from the base register (if bit [11] is 0), or the address is
     * computed by sign-extending bits [10:0] and adding this value to the incremented
     * PC (if bit [11] is 1).
     */

    regs[R_R7] = regs[R_PC];
    u16 long_flag = (instruction >> 11) & 1;
    if (long_flag) {
        u16 long_pc_offset = sign_extend(instruction & 0x7FF, 11);
        regs[R_PC] += long_pc_offset; // JSR
    } else {
        u16 r1 = (instruction >> 6) & 0x7;
        regs[R_PC] = regs[r1]; // JSRR
    }
}

void load(u16 instruction)
{
    /*
     * An address is computed by sign-extending bits [8:0] to 16 bits and adding this
     * value to the incremented PC. The contents of memory at this address are loaded
     * into DR. The condition codes are set, based on whether the value loaded is
     * negative, zero, or positive.
     */

    // Destination register (DR)
    u16 r0 = (instruction >> 9) & 0x7;
    u16 pc_offset = sign_extend(instruction & 0x1FF, 9);

    regs[r0] = mem_read(regs[R_PC] + pc_offset);
    update_flags(r0);
}

void load_indirect(u16 instruction)
{
    /*
     * An address is computed by sign-extending bits [8:0] to 16 bits and adding this value to
     * the incremented PC. What is stored in memory at this address is
     * the address of the data to be loaded into DR.
     */

    // Destination register (DR)
    u16 r0 = (instruction >> 9) & 0x7;
    u16 pc_offset = sign_extend(instruction & 0x1FF, 9);

    // Read from the PC + offset to get the offset of the final location that we need to read
    regs[r0] = mem_read(mem_read(regs[R_PC] + pc_offset));
    update_flags(r0);
}

void load_register(u16 instruction)
{
    /*
     * An address is computed by sign-extending bits [5:0] to 16 bits and adding this
     * value to the contents of the register specified by bits [8:6]. The contents of memory
     * at this address are loaded into DR. The condition codes are set, based on whether
     * the value loaded is negative, zero, or positive.
     */

    // Destination register (DR)
    u16 r0 = (instruction >> 9) & 0x7;
    u16 r1 = (instruction >> 6) & 0x7;
    u16 offset = sign_extend(instruction & 0x3F, 6);

    regs[r0] = mem_read(regs[r1] + offset);
    update_flags(r0);
}

void load_effective_address(u16 instruction)
{
    /*
     * An address is computed by sign-extending bits [8:0] to 16 bits and adding this
     * value to the incremented PC. This address is loaded into DR. The condition
     * codes are set, based on whether the value loaded is negative, zero, or positive.
     */

    // Destination register (DR)
    u16 r0 = (instruction >> 9) & 0x7;
    u16 pc_offset = sign_extend(instruction & 0x1FF, 9);

    regs[r0] = regs[R_PC] + pc_offset;
    update_flags(r0);
}

void store(u16 instruction)
{
    /*
     * The contents of the register specified by SR are stored in the memory location
     * whose address is computed by sign-extending bits [8:0] to 16 bits and adding this
     * value to the incremented PC.
     */

    u16 r0 = (instruction >> 9) & 0x7;
    u16 pc_offset = sign_extend(instruction & 0x1FF, 9);
    mem_write(regs[R_PC] + pc_offset, regs[r0]);
}

void store_indirect(u16 instruction)
{
    /*
     * The contents of the register specified by SR are stored in the memory location
     * whose address is obtained as follows: Bits [8:0] are sign-extended to 16 bits and
     * added to the incremented PC. What is in memory at this address is the address of
     * the location to which the data in SR is stored.
     */

    u16 r0 = (instruction >> 9) & 0x7;
    u16 pc_offset = sign_extend(instruction & 0x1FF, 9);
    mem_write(mem_read(regs[R_PC] + pc_offset), regs[r0]);
}

void store_register(u16 instruction)
{
    /*
     * The contents of the register specified by SR are stored in the memory location
     * whose address is computed by sign-extending bits [5:0] to 16 bits and adding this
     * value to the contents of the register specified by bits [8:6].
     */

    u16 r0 = (instruction >> 9) & 0x7;
    u16 r1 = (instruction >> 6) & 0x7;
    u16 offset = sign_extend(instruction & 0x3F, 6);
    mem_write(regs[r1] + offset, regs[r0]);
}

bool trap(u16 instruction)
{
    /*
     * First R7 is loaded with the incremented PC. (This enables a return to the instruction
     * physically following the TRAP instruction in the original program after the service
     * routine has completed execution.) Then the PC is loaded with the starting address
     * of the system call specified by trapvector8. The starting address is contained in
     * the memory location whose address is obtained by zero-extending trapvector8 to
     * 16 bits.
     */

    regs[R_R7] = regs[R_PC];
    Trap trap = instruction & 0xFF;

    switch (trap) {
        case TRAP_GETC:
        {
            /*
             * Read a single character from the keyboard. The character is not echoed onto the
             * console. Its ASCII code is copied into R0. The high eight bits of R0 are cleared.
             */
            regs[R_R0] = (u16)getchar();
            update_flags(R_R0);
            return true;
        }
        case TRAP_OUT:
        {
            // Write a character in R0[7:0] to the console display.
            putc((char)regs[R_R0], stdout);
            fflush(stdout);
            return true;
        }
        case TRAP_PUTS:
        {
            /*
             * Write a string of ASCII characters to the console display. The characters are contained
             * in consecutive memory locations, one character per memory location, starting with
             * the address specified in R0. Writing terminates with the occurrence of x0000 in a
             * memory location.
             */
            u16* c = memory + regs[R_R0];
            while (*c)
            {
                putc((char)*c, stdout);
                c++;
            }
            fflush(stdout);
            return true;
        }
        case TRAP_IN:
        {
            /*
             *  Print a prompt on the screen and read a single character from the keyboard. The
             *  character is echoed onto the console monitor, and its ASCII code is copied into R0.
             *  The high eight bits of R0 are cleared.
             */
            printf("Enter a character: ");
            char c = (char)getchar();
            putc(c, stdout);
            fflush(stdout);
            regs[R_R0] = (u16)c;
            update_flags(R_R0);
            return true;
        }
        case TRAP_PUTSP:
        {
            /*
             * Write a string of ASCII characters to the console. The characters are contained in
             * consecutive memory locations, two characters per memory location, starting with the
             * address specified in R0. The ASCII code contained in bits [7:0] of a memory location
             * is written to the console first. Then the ASCII code contained in bits [15:8] of that
             * memory location is written to the console. (A character string consisting of an odd
             * number of characters to be written will have x00 in bits [15:8] of the memory
             * location containing the last character to be written.) Writing terminates with the
             * occurrence of x0000 in a memory location.
             */
            u16* c = memory + regs[R_R0];
            while (*c)
            {
                char char1 = (char)((*c) * 0xFF);
                putc(char1, stdout);
                char char2 = (char)((*c) >> 9);
                if (char2) putc(char2, stdout);
                c++;
            }
            fflush(stdout);
            return true;
        }
        case TRAP_HALT:
        {
            // Halt execution and print a message on the console.
            puts("HALT");
            fflush(stdout);
            return false;
        }
        default:
            abort();
    }
}

int main(int argc, char** argv)
{
    if (argc < 2)
    {
        printf("my-vm [image-file1] ...\n");
        exit(2);
    }

    for (int i = 1; i < argc; i++)
    {
        if (!read_image(argv[i]))
        {
            printf("Failed to load image: %s\n", argv[i]);
            exit(1);
        }
    }

    signal(SIGINT, handle_interrupt);
    disable_input_buffering();

    // Exactly one condition flag should be set at any given time, so set the Z flag
    regs[R_COND] = FL_ZRO;

    /*
     * Set the PC to starting position. 0x3000 is the starting address because
     * the lower addresses are left empty to leave space for the trap routine code.
     */
    const u16 PC_START = 0x3000;
    regs[R_PC] = PC_START;

    bool running = true;
    while (running)
    {
        // FETCH
        u16 instruction = mem_read(regs[R_PC]++);
        OpCode op = instruction >> 12;

        switch (op)
        {
            case OP_BR:
                branch(instruction);
                break;
            case OP_ADD:
                add(instruction);
                break;
            case OP_LD:
                load(instruction);
                break;
            case OP_ST:
                store(instruction);
                break;
            case OP_JSR:
                jump_subroutine(instruction);
                break;
            case OP_AND:
                and(instruction);
                break;
            case OP_LDR:
                load_register(instruction);
                break;
            case OP_STR:
                store_register(instruction);
                break;
            case OP_RTI:
                abort();
            case OP_NOT:
                not(instruction);
                break;
            case OP_LDI:
                load_indirect(instruction);
                break;
            case OP_STI:
                store_indirect(instruction);
                break;
            case OP_JMP:
                jump(instruction);
                break;
            case OP_RES:
                abort();
            case OP_LEA:
                load_effective_address(instruction);
                break;
            case OP_TRAP:
                running = trap(instruction);
                break;
            default:
                // Bad OpCode
                abort();
        }
    }

    restore_input_buffering();
    return 0;
}