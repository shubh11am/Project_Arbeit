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
  @file armopcodes.cc
  @brief This file declares the ARM opcodes common to all architecture versions.

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
#include <arch/arm/armbase.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Opcodes
const ARM_Base::OpCode ARM_Base::OpCode::ADC    { string { "adc" } };
const ARM_Base::OpCode ARM_Base::OpCode::ADD    { string { "add" } };
const ARM_Base::OpCode ARM_Base::OpCode::AND    { string { "and" } };
const ARM_Base::OpCode ARM_Base::OpCode::B      { string { "b" },
                                                  false, false, false, false,
                                                  false, false, false, true,
                                                  true, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::BL     { string { "bl" },
                                                  false, false, false, false,
                                                  true, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::BIC    { string { "bic" } };
const ARM_Base::OpCode ARM_Base::OpCode::CDP    { string { "cdp" } };
const ARM_Base::OpCode ARM_Base::OpCode::CMN    { string { "cmn" } };
const ARM_Base::OpCode ARM_Base::OpCode::CMP    { string { "cmp" } };
const ARM_Base::OpCode ARM_Base::OpCode::EOR    { string { "eor" } };
const ARM_Base::OpCode ARM_Base::OpCode::LDC    { string { "ldc" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDM    { string { "ldm" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDR    { string { "ldr" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDRB   { string { "ldrb" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDRBT  { string { "ldrbt" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDRH   { string { "ldrh" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDRSB  { string { "ldrsb" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDRSH  { string { "ldrsh" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::LDRT   { string { "ldrt" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARM_Base::OpCode ARM_Base::OpCode::MCR    { string { "mcr" } };
const ARM_Base::OpCode ARM_Base::OpCode::MLA    { string { "mla" } };
const ARM_Base::OpCode ARM_Base::OpCode::MOV    { string { "mov" } };
const ARM_Base::OpCode ARM_Base::OpCode::MRC    { string { "mrc" } };
const ARM_Base::OpCode ARM_Base::OpCode::MRS    { string { "mrs" } };
const ARM_Base::OpCode ARM_Base::OpCode::MSR    { string { "msr" } };
const ARM_Base::OpCode ARM_Base::OpCode::MUL    { string { "mul" } };
const ARM_Base::OpCode ARM_Base::OpCode::MVN    { string { "mvn" } };
const ARM_Base::OpCode ARM_Base::OpCode::ORR    { string { "orr" } };
const ARM_Base::OpCode ARM_Base::OpCode::RSB    { string { "rsb" } };
const ARM_Base::OpCode ARM_Base::OpCode::RSC    { string { "rsc" } };
const ARM_Base::OpCode ARM_Base::OpCode::SBC    { string { "sbc" } };
const ARM_Base::OpCode ARM_Base::OpCode::SMLAL  { string { "smlal" } };
const ARM_Base::OpCode ARM_Base::OpCode::SMULL  { string { "smull" } };
const ARM_Base::OpCode ARM_Base::OpCode::STC    { string { "stc" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::STM    { string { "stm" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::STR    { string { "str" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::STRB   { string { "strb" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::STRBT  { string { "strbt" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::STRH   { string { "strh" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::STRT   { string { "strt" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::SUB    { string { "sub" } };
const ARM_Base::OpCode ARM_Base::OpCode::SWI    { string { "swi" },
                                                  false, false, false, false,
                                                  false, true, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::SWP    { string { "swp" },
                                                  false, true, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::SWPB   { string { "swpb" },
                                                  false, true, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARM_Base::OpCode ARM_Base::OpCode::TEQ    { string { "teq" } };
const ARM_Base::OpCode ARM_Base::OpCode::TST    { string { "tst" } };
const ARM_Base::OpCode ARM_Base::OpCode::UMLAL  { string { "umlal" } };
const ARM_Base::OpCode ARM_Base::OpCode::UMULL  { string { "umull" } };

}       // namespace WIR
