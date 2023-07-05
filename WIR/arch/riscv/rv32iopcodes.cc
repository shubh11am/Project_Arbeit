/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32iopcodes.cc
  @brief This file declares the RISC-V RV32I V2.0's opcodes.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32i.h>


//
// Preprocessor macros
//

#define MA true                   // Memory Access?
#define noMA false
#define ST true                   // Memory Store?
#define noST false
#define LD true                   // Memory Load?
#define noLD false
#define MV true                   // Register-Register Move?
#define noMV false
#define FC true                   // Function Call?
#define noFC false
#define IFC true                  // Indirect Function Call?
#define noIFC false
#define RT true                   // Function Return?
#define noRT false
#define CJ true                   // Conditional Jump?
#define noCJ false
#define UJ true                   // Unconditional Jump?
#define noUJ false
#define IJ true                   // Indirect Jump?
#define noIJ false
#define AD true                   // Assembly Data Directive?
#define noAD false
#define SE true                   // Has Side Effects?
#define noSE false


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

const RV32I::OpCode RV32I::OpCode::ADD    { string { "add" } };
const RV32I::OpCode RV32I::OpCode::ADDI   { string { "addi" } };
const RV32I::OpCode RV32I::OpCode::AND    { string { "and" } };
const RV32I::OpCode RV32I::OpCode::ANDI   { string { "andi" } };
const RV32I::OpCode RV32I::OpCode::AUIPC  { string { "auipc" } };
const RV32I::OpCode RV32I::OpCode::BEQ    { string { "beq" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, CJ, noUJ, noIJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::BGE    { string { "bge" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, CJ, noUJ, noIJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::BGEU   { string { "bgeu" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, CJ, noUJ, noIJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::BLT    { string { "blt" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, CJ, noUJ, noIJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::BLTU   { string { "bltu" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, CJ, noUJ, noIJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::BNE    { string { "bne" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, CJ, noUJ, noIJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::CSRRC  { string { "csrrc" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::CSRRCI { string { "csrrci" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::CSRRS  { string { "csrrs" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::CSRRSI { string { "csrrsi" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::CSRRW  { string { "csrrw" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::CSRRWI { string { "csrrwi" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::EBREAK { string { "ebreak" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::ECALL  { string { "ecall" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD, SE };
const RV32I::OpCode RV32I::OpCode::J      { string { "jal" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, UJ, noIJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::JAL    { string { "jal" },
                                            noMA, noST, noLD, noMV, FC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::JALR   { string { "jalr" },
                                            noMA, noST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, IJ, noAD, noSE };
const RV32I::OpCode RV32I::OpCode::LB     { string { "lb" },
                                            noMA, noST, LD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::LBU    { string { "lbu" },
                                            noMA, noST, LD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::LH     { string { "lh" },
                                            noMA, noST, LD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::LHU    { string { "lhu" },
                                            noMA, noST, LD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::LUI    { string { "lui" } };
const RV32I::OpCode RV32I::OpCode::LW     { string { "lw" },
                                            noMA, noST, LD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };

// MOV is a pseudo-operation de facto realized by an ADDI:
//   addi $tgt, $src, 0
const RV32I::OpCode RV32I::OpCode::MOV    { string { "addi" },
                                            noMA, noST, noLD, MV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::OR     { string { "or" } };
const RV32I::OpCode RV32I::OpCode::ORI    { string { "ori" } };
const RV32I::OpCode RV32I::OpCode::SB     { string { "sb" },
                                            noMA, ST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::SH     { string { "sh" },
                                            noMA, ST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::SLL    { string { "sll" } };
const RV32I::OpCode RV32I::OpCode::SLLI   { string { "slli" } };
const RV32I::OpCode RV32I::OpCode::SLT    { string { "slt" } };
const RV32I::OpCode RV32I::OpCode::SLTI   { string { "slti" } };
const RV32I::OpCode RV32I::OpCode::SLTIU  { string { "sltiu" } };
const RV32I::OpCode RV32I::OpCode::SLTU   { string { "sltu" } };
const RV32I::OpCode RV32I::OpCode::SRA    { string { "sra" } };
const RV32I::OpCode RV32I::OpCode::SRAI   { string { "srai" } };
const RV32I::OpCode RV32I::OpCode::SRL    { string { "srl" } };
const RV32I::OpCode RV32I::OpCode::SRLI   { string { "srli" } };
const RV32I::OpCode RV32I::OpCode::SUB    { string { "sub" } };
const RV32I::OpCode RV32I::OpCode::SW     { string { "sw" },
                                            noMA, ST, noLD, noMV, noFC, noIFC,
                                            noRT, noCJ, noUJ, noIJ, noAD,
                                            noSE };
const RV32I::OpCode RV32I::OpCode::XOR    { string { "xor" } };
const RV32I::OpCode RV32I::OpCode::XORI   { string { "xori" } };

}       // namespace WIR
