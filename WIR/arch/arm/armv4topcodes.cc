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
  @file armv4topcodes.cc
  @brief This file declares the ARMv4T-specific opcodes.

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
#include <arch/arm/armv4t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Opcodes
const ARMv4T::OpCode ARMv4T::OpCode::ASR        { string { "asr" } };
const ARMv4T::OpCode ARMv4T::OpCode::BX         { string { "bx" },
                                                  false, false, false, false,
                                                  false, false, false,
                                                  false, false, true, false,
                                                  false };
const ARMv4T::OpCode ARMv4T::OpCode::LDMIA      { string { "ldmia" },
                                                  false, false, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARMv4T::OpCode ARMv4T::OpCode::LSL        { string { "lsl" } };
const ARMv4T::OpCode ARMv4T::OpCode::LSR        { string { "lsr" } };
const ARMv4T::OpCode ARMv4T::OpCode::NEG        { string { "neg" } };
const ARMv4T::OpCode ARMv4T::OpCode::POP        { string { "pop" },
                                                  false, false, true, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARMv4T::OpCode ARMv4T::OpCode::PUSH       { string { "push" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARMv4T::OpCode ARMv4T::OpCode::ROR        { string { "ror" } };
const ARMv4T::OpCode ARMv4T::OpCode::STMIA      { string { "stmia" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };

}       // namespace WIR
