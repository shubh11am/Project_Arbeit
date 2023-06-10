        .file           "tests.c"
        # Generated using wir release 1.6 (2022-03-27)
        # for RISC-V ISA RV32IMC

        .section        .bss, "aw"
        .globl         B
        .align         8
        .type           B, @object
        .size           B, 4
B:
        .zero          4


        .section        .data, "aw"
        .globl         A
        .align         8
        .type           A, @object
        .size           A, 4
A:
        .word           4711


        .section        .text
        .balign         8
        .type           returnVoid, @function
        .global         returnVoid
returnVoid:
        addi            x2, x2, -64
        sw              x1, 60(x2)
        sw              x8, 56(x2)
        sw              x9, 52(x2)
        sw              x18, 48(x2)
        sw              x19, 44(x2)
        sw              x20, 40(x2)
        sw              x21, 36(x2)
        sw              x22, 32(x2)
        sw              x23, 28(x2)
        sw              x24, 24(x2)
        sw              x25, 20(x2)
        sw              x26, 16(x2)
        sw              x27, 12(x2)
        addi            x8, x2, 56
        c.mv            x5, x8
        lw              x27, -44(x5)
        lw              x26, -40(x5)
        lw              x25, -36(x5)
        lw              x24, -32(x5)
        lw              x23, -28(x5)
        lw              x22, -24(x5)
        lw              x21, -20(x5)
        lw              x20, -16(x5)
        lw              x19, -12(x5)
        lw              x18, -8(x5)
        lw              x9, -4(x5)
        lw              x8, 0(x5)
        lw              x1, 4(x5)
        addi            x2, x5, 8
        jalr            x0, x1, 0
        .size           returnVoid, .-returnVoid

        .balign         8
        .type           returnInt, @function
        .global         returnInt
returnInt:
        addi            x2, x2, -64
        sw              x1, 60(x2)
        sw              x8, 56(x2)
        sw              x9, 52(x2)
        sw              x18, 48(x2)
        sw              x19, 44(x2)
        sw              x20, 40(x2)
        sw              x21, 36(x2)
        sw              x22, 32(x2)
        sw              x23, 28(x2)
        sw              x24, 24(x2)
        sw              x25, 20(x2)
        sw              x26, 16(x2)
        sw              x27, 12(x2)
        addi            x8, x2, 56
        addi            x10, x0, 0
        c.mv            x5, x8
        lw              x27, -44(x5)
        lw              x26, -40(x5)
        lw              x25, -36(x5)
        lw              x24, -32(x5)
        lw              x23, -28(x5)
        lw              x22, -24(x5)
        lw              x21, -20(x5)
        lw              x20, -16(x5)
        lw              x19, -12(x5)
        lw              x18, -8(x5)
        lw              x9, -4(x5)
        lw              x8, 0(x5)
        lw              x1, 4(x5)
        addi            x2, x5, 8
        jalr            x0, x1, 0
        .size           returnInt, .-returnInt

        .balign         8
        .type           returnInt10Arguments, @function
        .global         returnInt10Arguments
returnInt10Arguments:
        addi            x2, x2, -64
        sw              x1, 60(x2)
        sw              x8, 56(x2)
        sw              x9, 52(x2)
        sw              x18, 48(x2)
        sw              x19, 44(x2)
        sw              x20, 40(x2)
        sw              x21, 36(x2)
        sw              x22, 32(x2)
        sw              x23, 28(x2)
        sw              x24, 24(x2)
        sw              x25, 20(x2)
        sw              x26, 16(x2)
        sw              x27, 12(x2)
        addi            x8, x2, 56
        addi            x10, x0, 0
        c.mv            x5, x8
        lw              x27, -44(x5)
        lw              x26, -40(x5)
        lw              x25, -36(x5)
        lw              x24, -32(x5)
        lw              x23, -28(x5)
        lw              x22, -24(x5)
        lw              x21, -20(x5)
        lw              x20, -16(x5)
        lw              x19, -12(x5)
        lw              x18, -8(x5)
        lw              x9, -4(x5)
        lw              x8, 0(x5)
        lw              x1, 4(x5)
        addi            x2, x5, 8
        jalr            x0, x1, 0
        .size           returnInt10Arguments, .-returnInt10Arguments

        .balign         8
        .type           add, @function
        .global         add
add:
        addi            x2, x2, -96
        # RA SPILL x_4835
        sw              x10, 8(x2)
        addi            x31, x11, 0
        addi            x30, x12, 0
        addi            x29, x13, 0
        addi            x28, x14, 0
        addi            x7, x15, 0
        addi            x6, x16, 0
        addi            x5, x17, 0
        sw              x1, 92(x2)
        sw              x8, 88(x2)
        sw              x9, 84(x2)
        sw              x18, 80(x2)
        sw              x19, 76(x2)
        sw              x20, 72(x2)
        sw              x21, 68(x2)
        sw              x22, 64(x2)
        sw              x23, 60(x2)
        sw              x24, 56(x2)
        sw              x25, 52(x2)
        sw              x26, 48(x2)
        sw              x27, 44(x2)
        addi            x8, x2, 88
        lw              x18, 8(x8)
        lw              x9, 12(x8)
        # RA SPILL x_4897
        lw              x10, 8(x2)
        addi            x11, x31, 0
        addi            x12, x30, 0
        addi            x13, x29, 0
        addi            x14, x28, 0
        addi            x15, x7, 0
        addi            x16, x6, 0
        addi            x17, x5, 0
        sw              x18, 0(x2)
        sw              x9, 4(x2)
        # RA SPILL x5
        sw              x5, 12(x2)
        # RA SPILL x6
        sw              x6, 16(x2)
        # RA SPILL x7
        sw              x7, 20(x2)
        # RA SPILL x28
        sw              x28, 24(x2)
        # RA SPILL x29
        sw              x29, 28(x2)
        # RA SPILL x30
        sw              x30, 32(x2)
        # RA SPILL x31
        sw              x31, 36(x2)
        jal             x1, returnInt10Arguments
.L4963:
        # RA SPILL x31
        lw              x31, 36(x2)
        # RA SPILL x30
        lw              x30, 32(x2)
        # RA SPILL x29
        lw              x29, 28(x2)
        # RA SPILL x28
        lw              x28, 24(x2)
        # RA SPILL x7
        lw              x7, 20(x2)
        # RA SPILL x6
        lw              x6, 16(x2)
        # RA SPILL x5
        lw              x5, 12(x2)
        addi            x9, x0, 1
        # RA SPILL x_6742
        lw              x18, 8(x2)
        add             x9, x9, x18
        add             x9, x9, x31
        add             x9, x9, x30
        add             x9, x9, x29
        add             x9, x9, x28
        add             x9, x9, x7
        add             x9, x9, x6
        add             x9, x9, x5
        lw              x5, 8(x8)
        add             x9, x9, x5
        lw              x5, 12(x8)
        add             x9, x9, x5
        c.mv            x10, x9
        c.mv            x5, x8
        lw              x27, -44(x5)
        lw              x26, -40(x5)
        lw              x25, -36(x5)
        lw              x24, -32(x5)
        lw              x23, -28(x5)
        lw              x22, -24(x5)
        lw              x21, -20(x5)
        lw              x20, -16(x5)
        lw              x19, -12(x5)
        lw              x18, -8(x5)
        lw              x9, -4(x5)
        lw              x8, 0(x5)
        lw              x1, 4(x5)
        addi            x2, x5, 8
        jalr            x0, x1, 0
        .size           add, .-add

        .balign         8
        .type           fakulaet, @function
        .global         fakulaet
fakulaet:
        addi            x2, x2, -80
        addi            x5, x10, 0
        sw              x1, 76(x2)
        sw              x8, 72(x2)
        sw              x9, 68(x2)
        sw              x18, 64(x2)
        sw              x19, 60(x2)
        sw              x20, 56(x2)
        sw              x21, 52(x2)
        sw              x22, 48(x2)
        sw              x23, 44(x2)
        sw              x24, 40(x2)
        sw              x25, 36(x2)
        sw              x26, 32(x2)
        sw              x27, 28(x2)
        addi            x8, x2, 72
        slti            x6, x5, 2
        beq             x6, x0, .L5212
.L5209:
        addi            x10, x0, 1
        c.mv            x5, x8
        lw              x27, -44(x5)
        lw              x26, -40(x5)
        lw              x25, -36(x5)
        lw              x24, -32(x5)
        lw              x23, -28(x5)
        lw              x22, -24(x5)
        lw              x21, -20(x5)
        lw              x20, -16(x5)
        lw              x19, -12(x5)
        lw              x18, -8(x5)
        lw              x9, -4(x5)
        lw              x8, 0(x5)
        lw              x1, 4(x5)
        addi            x2, x5, 8
        jalr            x0, x1, 0
.L5212:
        addi            x6, x0, 1
        sub             x10, x5, x6
        # RA SPILL x5
        sw              x5, 0(x2)
        jal             x1, fakulaet
.L5454:
        # RA SPILL x5
        lw              x5, 0(x2)
        mul             x10, x5, x10
        c.mv            x5, x8
        lw              x27, -44(x5)
        lw              x26, -40(x5)
        lw              x25, -36(x5)
        lw              x24, -32(x5)
        lw              x23, -28(x5)
        lw              x22, -24(x5)
        lw              x21, -20(x5)
        lw              x20, -16(x5)
        lw              x19, -12(x5)
        lw              x18, -8(x5)
        lw              x9, -4(x5)
        lw              x8, 0(x5)
        lw              x1, 4(x5)
        addi            x2, x5, 8
        jalr            x0, x1, 0
        .size           fakulaet, .-fakulaet

        .balign         8
        .type           main, @function
        .global         main
main:
        addi            x2, x2, -64
        sw              x1, 60(x2)
        sw              x8, 56(x2)
        sw              x9, 52(x2)
        sw              x18, 48(x2)
        sw              x19, 44(x2)
        sw              x20, 40(x2)
        sw              x21, 36(x2)
        sw              x22, 32(x2)
        sw              x23, 28(x2)
        sw              x24, 24(x2)
        sw              x25, 20(x2)
        sw              x26, 16(x2)
        sw              x27, 12(x2)
        addi            x8, x2, 56
        jal             x1, returnVoid
.L5672:
        jal             x1, returnInt
.L5681:
        addi            x10, x0, 1
        addi            x11, x0, 2
        addi            x12, x0, 3
        addi            x13, x0, 4
        addi            x14, x0, 5
        addi            x15, x0, 6
        addi            x16, x0, 7
        addi            x17, x0, 8
        addi            x6, x0, 9
        addi            x5, x0, 10
        sw              x6, 0(x2)
        sw              x5, 4(x2)
        jal             x1, returnInt10Arguments
.L5823:
        addi            x10, x0, 1
        addi            x11, x0, 2
        addi            x12, x0, 3
        addi            x13, x0, 4
        addi            x14, x0, 5
        addi            x15, x0, 6
        addi            x16, x0, 7
        addi            x17, x0, 8
        addi            x6, x0, 9
        addi            x5, x0, 10
        sw              x6, 0(x2)
        sw              x5, 4(x2)
        jal             x1, add
.L5965:
        lui             x5, %hi(A)
        lw              x5, %lo(A)(x5)
        lui             x6, %hi(B)
        sw              x5, %lo(B)(x6)
        addi            x10, x0, 0
        c.mv            x5, x8
        lw              x27, -44(x5)
        lw              x26, -40(x5)
        lw              x25, -36(x5)
        lw              x24, -32(x5)
        lw              x23, -28(x5)
        lw              x22, -24(x5)
        lw              x21, -20(x5)
        lw              x20, -16(x5)
        lw              x19, -12(x5)
        lw              x18, -8(x5)
        lw              x9, -4(x5)
        lw              x8, 0(x5)
        lw              x1, 4(x5)
        addi            x2, x5, 8
        jalr            x0, x1, 0
        .size           main, .-main

        .section        .text
        .balign          8
        .section        .bss, "aw"
        .balign          8
        .section        .data, "aw"
        .balign          8
        .section        .sbss, "aw"
        .balign          8
        .section        .sdata, "aw"
        .balign          8
        .section        .zbss, "aw"
        .balign          8
        .section        .zdata, "aw"
        .balign          8
        .section        .rodata, ""
        .balign          8
        .section        .stack, "w"
        .balign          8
