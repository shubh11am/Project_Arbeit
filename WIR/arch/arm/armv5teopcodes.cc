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
  @file armv5teopcodes.cc
  @brief This file declares the ARMv5TE-specific opcodes.

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
#include <arch/arm/armv5te.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Opcodes
const ARMv5TE::OpCode ARMv5TE::OpCode::LDRD     { string { "ldrd" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARMv5TE::OpCode ARMv5TE::OpCode::MCRR     { string { "mcrr" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::MRRC     { string { "mrrc" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::PLD      { string { "pld" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::QADD     { string { "qadd" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::QDADD    { string { "qdadd" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::QDSUB    { string { "qdsub" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::QSUB     { string { "qsub" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLABB   { string { "smlabb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLABT   { string { "smlabt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLATB   { string { "smlatb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLATT   { string { "smlatt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLALBB  { string { "smlalbb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLALBT  { string { "smlalbt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLALTB  { string { "smlaltb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLALTT  { string { "smlaltt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLAWB   { string { "smlawb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMLAWT   { string { "smlawt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMULBB   { string { "smulbb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMULBT   { string { "smulbt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMULTB   { string { "smultb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMULTT   { string { "smultt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMULWB   { string { "smulwb" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::SMULWT   { string { "smulwt" } };
const ARMv5TE::OpCode ARMv5TE::OpCode::STRD     { string { "strd" },
                                                  false, true, false, false,
                                                  false, false, false, false,
                                                  false, false, false, false };

}       // namespace WIR
