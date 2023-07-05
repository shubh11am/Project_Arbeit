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
  @file armregistertypes.cc
  @brief This file declares the ARM register types.

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

// Register types
const ARM_Base::RegisterType ARM_Base::RegisterType::reg { string { "r" },
                                                           string { "r_" },
                                                           string { "" },
                                                           string { "" }, 32 };
const ARM_Base::RegisterType ARM_Base::RegisterType::lo { string { "r" },
                                                          string { "l_" },
                                                          string { "" },
                                                          string { "" }, 32 };
const ARM_Base::RegisterType ARM_Base::RegisterType::hi { string { "r" },
                                                          string { "h_" },
                                                          string { "" },
                                                          string { "" }, 32 };

}       // namespace WIR
