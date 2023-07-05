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
  @file armv5topcodes.cc
  @brief This file declares the ARMv5T-specific opcodes.

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
#include <arch/arm/armv5t.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Opcodes
const ARMv5T::OpCode ARMv5T::OpCode::BKPT       { string { "bkpt" },
                                                  false, false, false, false,
                                                  false, false, false,
                                                  false, false, true, false,
                                                  false };
const ARMv5T::OpCode ARMv5T::OpCode::BLX        { string { "blx" },
                                                  false, false, false, false,
                                                  true, false, false,
                                                  false, false, false, false,
                                                  false };
const ARMv5T::OpCode ARMv5T::OpCode::CDP2       { string { "cdp2" } };
const ARMv5T::OpCode ARMv5T::OpCode::CLZ        { string { "clz" } };
const ARMv5T::OpCode ARMv5T::OpCode::LDC2       { string { "ldc2" },
                                                  false, false, true, false,
                                                  false, false, false, false,
                                                  false, false, false, false };
const ARMv5T::OpCode ARMv5T::OpCode::MCR2       { string { "mcr2" } };
const ARMv5T::OpCode ARMv5T::OpCode::STC2       { string { "stc2" },
                                                  false, true, false, false,
                                                  false, false, false, false,
                                                  false, false, false, false };

}       // namespace WIR
