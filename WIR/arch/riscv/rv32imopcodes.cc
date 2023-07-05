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
  @file rv32imopcodes.cc
  @brief This file declares the RISC-V RV32IM V2.0's opcodes.

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
#include <arch/riscv/rv32im.h>


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

const RV32IM::OpCode RV32IM::OpCode::DIV    { string { "div" } };
const RV32IM::OpCode RV32IM::OpCode::DIVU   { string { "divu" } };
const RV32IM::OpCode RV32IM::OpCode::MUL    { string { "mul" } };
const RV32IM::OpCode RV32IM::OpCode::MULH   { string { "mulh" } };
const RV32IM::OpCode RV32IM::OpCode::MULHSU { string { "mulhsu" } };
const RV32IM::OpCode RV32IM::OpCode::MULHU  { string { "mulhu" } };
const RV32IM::OpCode RV32IM::OpCode::REM    { string { "rem" } };
const RV32IM::OpCode RV32IM::OpCode::REMU   { string { "remu" } };

}       // namespace WIR
