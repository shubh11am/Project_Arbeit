/*

   This header file belongs to the

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
  @file mipsio.h
  @brief This file provides MIPS/SPIM-specific stream I/O routines for the %WIR
         library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _MIPS_IO_H
#define _MIPS_IO_H


//
// Include section
//


//
// Header section
//

namespace WIR {

/*!
  @brief mips is an I/O manipulator that provides a MIPS assembler dump of %WIR.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &mips( std::ostream &os );

/*!
  @brief dumpMIPSOperation dumps a %WIR operation to an output stream in a
         MIPS/SPIM-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] o A reference to the %WIR operation to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpMIPSOperation( std::ostream &os, const WIR_Operation &o );

}       // namespace WIR

#endif  // _MIPS_IO_H
