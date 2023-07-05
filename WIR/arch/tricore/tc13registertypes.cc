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
  @file tc13registertypes.cc
  @brief This file declares the Infineon TriCore TC13's register types.

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
#include <arch/tricore/tc13.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Register types
const TC13::RegisterType TC13::RegisterType::aReg { string { "a" },
                                                    string { "a_" },
                                                    string { "" },
                                                    string { "" }, 32 };
const TC13::RegisterType TC13::RegisterType::dReg { string { "d" },
                                                    string { "d_" },
                                                    string { "" },
                                                    string { "" }, 32 };
const TC13::RegisterType TC13::RegisterType::eReg { string { "e" },
                                                    string { "e_" },
                                                    string { "" },
                                                    string { "" }, 64 };
const TC13::RegisterType TC13::RegisterType::pReg { string { "p" },
                                                    string { "p_" },
                                                    string { "" },
                                                    string { "" }, 64 };
const TC13::RegisterType TC13::RegisterType::pswBit { string { "PSW." },
                                                      string { "PSW_" },
                                                      string { "" },
                                                      string { "" }, 1 };

}       // namespace WIR
