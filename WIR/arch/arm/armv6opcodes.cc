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
  @file armv6opcodes.cc
  @brief This file declares the ARMv6-specific opcodes.

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
#include <arch/arm/armv6.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Opcodes
const ARMv6::OpCode ARMv6::OpCode::CPS          { string { "cps" } };
const ARMv6::OpCode ARMv6::OpCode::CPSID        { string { "cpsid" } };
const ARMv6::OpCode ARMv6::OpCode::CPSIE        { string { "cpsie" } };
const ARMv6::OpCode ARMv6::OpCode::CPY          { string { "cpy" },
                                                  false, false, false, true,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARMv6::OpCode ARMv6::OpCode::LDREX        { string { "ldrex" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARMv6::OpCode ARMv6::OpCode::MCRR2        { string { "mcrr2" } };
const ARMv6::OpCode ARMv6::OpCode::MRRC2        { string { "mrrc2" } };
const ARMv6::OpCode ARMv6::OpCode::PKHBT        { string { "pkhbt" } };
const ARMv6::OpCode ARMv6::OpCode::PKHTB        { string { "pkhtb" } };
const ARMv6::OpCode ARMv6::OpCode::QADD16       { string { "qadd16" } };
const ARMv6::OpCode ARMv6::OpCode::QADD8        { string { "qadd8" } };
const ARMv6::OpCode ARMv6::OpCode::QADDSUBX     { string { "qaddsubx" } };
const ARMv6::OpCode ARMv6::OpCode::QSUB16       { string { "qsub16" } };
const ARMv6::OpCode ARMv6::OpCode::QSUB8        { string { "qsub8" } };
const ARMv6::OpCode ARMv6::OpCode::QSUBADDX     { string { "qsubaddx" } };
const ARMv6::OpCode ARMv6::OpCode::REV          { string { "rev" } };
const ARMv6::OpCode ARMv6::OpCode::REV16        { string { "rev16" } };
const ARMv6::OpCode ARMv6::OpCode::REVSH        { string { "revsh" } };
const ARMv6::OpCode ARMv6::OpCode::RFE          { string { "rfe" },
                                                  false, false, false, false,
                                                  false, false, true, false,
                                                  false, false, false, false };
const ARMv6::OpCode ARMv6::OpCode::SADD16       { string { "sadd16" } };
const ARMv6::OpCode ARMv6::OpCode::SADD8        { string { "sadd8" } };
const ARMv6::OpCode ARMv6::OpCode::SADDSUBX     { string { "saddsubx" } };
const ARMv6::OpCode ARMv6::OpCode::SEL          { string { "sel" } };
const ARMv6::OpCode ARMv6::OpCode::SETEND       { string { "setend" } };
const ARMv6::OpCode ARMv6::OpCode::SHADD16      { string { "shadd16" } };
const ARMv6::OpCode ARMv6::OpCode::SHADD8       { string { "shadd8" } };
const ARMv6::OpCode ARMv6::OpCode::SHADDSUBX    { string { "shaddsubx" } };
const ARMv6::OpCode ARMv6::OpCode::SHSUB16      { string { "shsub16" } };
const ARMv6::OpCode ARMv6::OpCode::SHSUB8       { string { "shsub8" } };
const ARMv6::OpCode ARMv6::OpCode::SHSUBADDX    { string { "shsubaddx" } };
const ARMv6::OpCode ARMv6::OpCode::SMLAD        { string { "smlad" } };
const ARMv6::OpCode ARMv6::OpCode::SMLADX       { string { "smladx" } };
const ARMv6::OpCode ARMv6::OpCode::SMLALD       { string { "smlald" } };
const ARMv6::OpCode ARMv6::OpCode::SMLALDX      { string { "smlaldx" } };
const ARMv6::OpCode ARMv6::OpCode::SMLSD        { string { "smlsd" } };
const ARMv6::OpCode ARMv6::OpCode::SMLSDX       { string { "smlsdx" } };
const ARMv6::OpCode ARMv6::OpCode::SMLSLD       { string { "smlsld" } };
const ARMv6::OpCode ARMv6::OpCode::SMLSLDX      { string { "smlsldx" } };
const ARMv6::OpCode ARMv6::OpCode::SMMLA        { string { "smmla" } };
const ARMv6::OpCode ARMv6::OpCode::SMMLAR       { string { "smmlar" } };
const ARMv6::OpCode ARMv6::OpCode::SMMLS        { string { "smmls" } };
const ARMv6::OpCode ARMv6::OpCode::SMMLSR       { string { "smmlsr" } };
const ARMv6::OpCode ARMv6::OpCode::SMMUL        { string { "smmul" } };
const ARMv6::OpCode ARMv6::OpCode::SMMULR       { string { "smmulr" } };
const ARMv6::OpCode ARMv6::OpCode::SMUAD        { string { "smuad" } };
const ARMv6::OpCode ARMv6::OpCode::SMUADX       { string { "smuadx" } };
const ARMv6::OpCode ARMv6::OpCode::SMUSD        { string { "smusd" } };
const ARMv6::OpCode ARMv6::OpCode::SMUSDX       { string { "smusdx" } };
const ARMv6::OpCode ARMv6::OpCode::SRS          { string { "srs" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARMv6::OpCode ARMv6::OpCode::SSAT         { string { "ssat" } };
const ARMv6::OpCode ARMv6::OpCode::SSAT16       { string { "ssat16" } };
const ARMv6::OpCode ARMv6::OpCode::SSUB16       { string { "ssub16" } };
const ARMv6::OpCode ARMv6::OpCode::SSUB8        { string { "ssub8" } };
const ARMv6::OpCode ARMv6::OpCode::SSUBADDX     { string { "ssubaddx" } };
const ARMv6::OpCode ARMv6::OpCode::STREX        { string { "strex" },
                                                  false, true, false, false,
                                                  false, false, false,
                                                  false, false, false, false,
                                                  false };
const ARMv6::OpCode ARMv6::OpCode::SXTAB        { string { "sxtab" } };
const ARMv6::OpCode ARMv6::OpCode::SXTAB16      { string { "sxtab16" } };
const ARMv6::OpCode ARMv6::OpCode::SXTAH        { string { "sxtah" } };
const ARMv6::OpCode ARMv6::OpCode::SXTB         { string { "sxtb" } };
const ARMv6::OpCode ARMv6::OpCode::SXTB16       { string { "sxtb16" } };
const ARMv6::OpCode ARMv6::OpCode::SXTH         { string { "sxth" } };
const ARMv6::OpCode ARMv6::OpCode::UADD16       { string { "uadd16" } };
const ARMv6::OpCode ARMv6::OpCode::UADD8        { string { "uadd8" } };
const ARMv6::OpCode ARMv6::OpCode::UADDSUBX     { string { "uaddsubx" } };
const ARMv6::OpCode ARMv6::OpCode::UHADD16      { string { "uhadd16" } };
const ARMv6::OpCode ARMv6::OpCode::UHADD8       { string { "uhadd8" } };
const ARMv6::OpCode ARMv6::OpCode::UHADDSUBX    { string { "uhaddsubx" } };
const ARMv6::OpCode ARMv6::OpCode::UHSUB16      { string { "uhsub16" } };
const ARMv6::OpCode ARMv6::OpCode::UHSUB8       { string { "uhsub8" } };
const ARMv6::OpCode ARMv6::OpCode::UHSUBADDX    { string { "uhsubaddx" } };
const ARMv6::OpCode ARMv6::OpCode::UMAAL        { string { "umaal" } };
const ARMv6::OpCode ARMv6::OpCode::UQADD16      { string { "uqadd16" } };
const ARMv6::OpCode ARMv6::OpCode::UQADD8       { string { "uqadd8" } };
const ARMv6::OpCode ARMv6::OpCode::UQADDSUBX    { string { "uqaddsubx" } };
const ARMv6::OpCode ARMv6::OpCode::UQSUB16      { string { "uqsub16" } };
const ARMv6::OpCode ARMv6::OpCode::UQSUB8       { string { "uqsub8" } };
const ARMv6::OpCode ARMv6::OpCode::UQSUBADDX    { string { "uqsubaddx" } };
const ARMv6::OpCode ARMv6::OpCode::USAD8        { string { "usad8" } };
const ARMv6::OpCode ARMv6::OpCode::USADA8       { string { "usada8" } };
const ARMv6::OpCode ARMv6::OpCode::USAT         { string { "usat" } };
const ARMv6::OpCode ARMv6::OpCode::USAT16       { string { "usat16" } };
const ARMv6::OpCode ARMv6::OpCode::USUB16       { string { "usub16" } };
const ARMv6::OpCode ARMv6::OpCode::USUB8        { string { "usub8" } };
const ARMv6::OpCode ARMv6::OpCode::USUBADDX     { string { "usubaddx" } };
const ARMv6::OpCode ARMv6::OpCode::UXTAB        { string { "uxtab" } };
const ARMv6::OpCode ARMv6::OpCode::UXTAB16      { string { "uxtab16" } };
const ARMv6::OpCode ARMv6::OpCode::UXTAH        { string { "uxtah" } };
const ARMv6::OpCode ARMv6::OpCode::UXTB         { string { "uxtb" } };
const ARMv6::OpCode ARMv6::OpCode::UXTB16       { string { "uxtb16" } };
const ARMv6::OpCode ARMv6::OpCode::UXTH         { string { "uxth" } };

}       // namespace WIR
