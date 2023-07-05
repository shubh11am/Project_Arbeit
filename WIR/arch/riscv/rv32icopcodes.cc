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
  @file rv32icopcodes.cc
  @brief This file declares the RISC-V RV32IC V2.0's opcodes.

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
#include <arch/riscv/rv32ic.h>


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
#define CJMP true                 // Conditional Jump?
#define noCJMP false
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

const RV32IC::OpCode RV32IC::OpCode::CADD      { string { "c.add" } };
const RV32IC::OpCode RV32IC::OpCode::CADDI     { string { "c.addi" } };
const RV32IC::OpCode RV32IC::OpCode::CADDI16SP { string { "c.addi16sp" } };
const RV32IC::OpCode RV32IC::OpCode::CADDI4SPN { string { "c.addi4spn" } };
const RV32IC::OpCode RV32IC::OpCode::CAND      { string { "c.and" } };
const RV32IC::OpCode RV32IC::OpCode::CANDI     { string { "c.andi" } };
const RV32IC::OpCode RV32IC::OpCode::CBEQZ     { string { "c.beqz" },
                                                 noMA, noST, noLD, noMV, noFC,
                                                 noIFC, noRT, CJMP, noUJ, noIJ,
                                                 noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CBNEZ     { string { "c.bnez" },
                                                 noMA, noST, noLD, noMV, noFC,
                                                 noIFC, noRT, CJMP, noUJ, noIJ,
                                                 noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CEBREAK   { string { "c.ebreak" },
                                                 noMA, noST, noLD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, SE };
const RV32IC::OpCode RV32IC::OpCode::CJ        { string { "c.j" },
                                                 noMA, noST, noLD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, UJ, noIJ,
                                                 noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CJAL      { string { "c.jal" },
                                                 noMA, noST, noLD, noMV, FC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CJALR     { string { "c.jalr" },
                                                 noMA, noST, noLD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ, IJ,
                                                 noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CJR       { string { "c.jr" },
                                                 noMA, noST, noLD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ, IJ,
                                                 noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CLI       { string { "c.li" } };
const RV32IC::OpCode RV32IC::OpCode::CLUI      { string { "c.lui" } };
const RV32IC::OpCode RV32IC::OpCode::CLW       { string { "c.lw" },
                                                 noMA, noST, LD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CLWSP     { string { "c.lwsp" },
                                                 noMA, noST, LD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CMV       { string { "c.mv" },
                                                 noMA, noST, noLD, MV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CNOP      { string { "c.nop" },
                                                 noMA, noST, noLD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, SE };
const RV32IC::OpCode RV32IC::OpCode::COR       { string { "c.or" } };
const RV32IC::OpCode RV32IC::OpCode::CSLLI     { string { "c.slli" } };
const RV32IC::OpCode RV32IC::OpCode::CSRAI     { string { "c.srai" } };
const RV32IC::OpCode RV32IC::OpCode::CSRLI     { string { "c.srli" } };
const RV32IC::OpCode RV32IC::OpCode::CSUB      { string { "c.sub" } };
const RV32IC::OpCode RV32IC::OpCode::CSW       { string { "c.sw" },
                                                 noMA, ST, noLD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CSWSP     { string { "c.swsp" },
                                                 noMA, ST, noLD, noMV, noFC,
                                                 noIFC, noRT, noCJMP, noUJ,
                                                 noIJ, noAD, noSE };
const RV32IC::OpCode RV32IC::OpCode::CXOR      { string { "c.xor" } };

}       // namespace WIR
