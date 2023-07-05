/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2015 - 2022, Heiko Falk.

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file mipsopcodes.cc
  @brief This file declares the MIPS opcodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Opcodes
const MIPS::OpCode MIPS::OpCode::ADD            { string { "add" } };
const MIPS::OpCode MIPS::OpCode::ADDI           { string { "addi" } };
const MIPS::OpCode MIPS::OpCode::ADDIU          { string { "addiu" } };
const MIPS::OpCode MIPS::OpCode::ADDU           { string { "addu" } };
const MIPS::OpCode MIPS::OpCode::AND            { string { "and" } };
const MIPS::OpCode MIPS::OpCode::ANDI           { string { "andi" } };
const MIPS::OpCode MIPS::OpCode::BEQ            { string { "beq" },
                                                  false, false, false, false,
                                                  false, false, false,
                                                  true, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::BNE            { string { "bne" },
                                                  false, false, false, false,
                                                  false, false, false,
                                                  true, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::DIV            { string { "div" } };
const MIPS::OpCode MIPS::OpCode::DIVU           { string { "divu" } };
const MIPS::OpCode MIPS::OpCode::J              { string { "j" },
                                                  false, false, false, false,
                                                  false, false, false,
                                                  false, true, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::JAL            { string { "jal" },
                                                  false, false, false, false,
                                                  true, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::JR             { string { "jr" },
                                                  false, false, false, false,
                                                  false, false, true,
                                                  false, true, true, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::JALR           { string { "jalr" },
                                                  false, false, false, false,
                                                  true, true, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::LB             { string { "lb" },
                                                  false, false, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::LBU            { string { "lbu" },
                                                  false, false, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::LH             { string { "lh" },
                                                  false, false, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::LHU            { string { "lhu" },
                                                  false, false, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::LUI            { string { "lui" } };
const MIPS::OpCode MIPS::OpCode::LW             { string { "lw" },
                                                  false, false, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::MFCO           { string { "mfco" } };
const MIPS::OpCode MIPS::OpCode::MFHI           { string { "mfhi" } };
const MIPS::OpCode MIPS::OpCode::MFLO           { string { "mflo" } };
const MIPS::OpCode MIPS::OpCode::MULT           { string { "mult" } };
const MIPS::OpCode MIPS::OpCode::MULTU          { string { "multu" } };
const MIPS::OpCode MIPS::OpCode::NOR            { string { "nor" } };
const MIPS::OpCode MIPS::OpCode::OR             { string { "or" } };
const MIPS::OpCode MIPS::OpCode::ORI            { string { "ori" } };
const MIPS::OpCode MIPS::OpCode::SB             { string { "sb" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::SH             { string { "sh" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::SLL            { string { "sll" } };
const MIPS::OpCode MIPS::OpCode::SLLV           { string { "sllv" } };
const MIPS::OpCode MIPS::OpCode::SLT            { string { "slt" } };
const MIPS::OpCode MIPS::OpCode::SLTI           { string { "slti" } };
const MIPS::OpCode MIPS::OpCode::SLTIU          { string { "sltiu" } };
const MIPS::OpCode MIPS::OpCode::SLTU           { string { "sltu" } };
const MIPS::OpCode MIPS::OpCode::SRA            { string { "sra" } };
const MIPS::OpCode MIPS::OpCode::SRAV           { string { "srav" } };
const MIPS::OpCode MIPS::OpCode::SRL            { string { "srl" } };
const MIPS::OpCode MIPS::OpCode::SRLV           { string { "srlv" } };
const MIPS::OpCode MIPS::OpCode::SUB            { string { "sub" } };
const MIPS::OpCode MIPS::OpCode::SUBU           { string { "subu" } };
const MIPS::OpCode MIPS::OpCode::SW             { string { "sw" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const MIPS::OpCode MIPS::OpCode::XOR            { string { "xor" } };
const MIPS::OpCode MIPS::OpCode::XORI           { string { "xori" } };

}       // namespace WIR
