/*

   This header file belongs to the

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
  @file rvio.h
  @brief This file provides RISC-V-specific stream I/O routines for the %WIR
         library.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


#ifndef _RV_IO_H
#define _RV_IO_H


//
// Include section
//

// Include standard headers
#include <iostream>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Comment;
class WIR_FileInfo;
class WIR_Operation;
class WIR_RegisterParameter;


/*!
  @brief riscv is an I/O manipulator that provides a RISC-V assembler dump of a
         %WIR.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
std::ostream &riscv( std::ostream &os );

/*!
  @brief dumpRVBasicBlock dumps a %WIR basic block to an output stream in a
         RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] b A const reference to the %WIR basic block to be dumped.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
void dumpRVBasicBlock( std::ostream &os, const WIR_BasicBlock &b );

/*!
  @brief dumpRVCompilationUnit dumps a %WIR compilation unit to an output stream
         in a RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] c A const reference to the %WIR compilation unit to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpRVCompilationUnit( std::ostream &os, const WIR_CompilationUnit &c );

/*!
  @brief dumpRVData dumps a %WIR data object to an output stream in a RISC-V-
         specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] d A const reference to the %WIR data object to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpRVData( std::ostream &os, const WIR_Data &d );

/*!
  @brief dumpRVDataSection dumps a data section to an output stream in a
         RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] sec A pointer to the %WIR data section to be dumped.
  @param[in] l A const reference to a list of data objects to be dumped within
               the specified section.
  @param[in] firstSec A Boolean denoting whether the very first data section is
                      dumped or not (just used for line break formatting)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpRVDataSection( std::ostream &os, WIR_Section *sec,
                        const std::list<std::reference_wrapper<WIR_Data>> &l,
                        bool firstSec );

/*!
  @brief dumpRVFunction dumps a %WIR function to an output stream in a
         RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] f A const reference to the %WIR function to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpRVFunction( std::ostream &os, const WIR_Function &f );

/*!
  @brief dumpRVOperation dumps a %WIR operation to an output stream in a
          RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] o A const reference to the %WIR operation to be dumped.

  While dumping an operation's parameters, this function omits any implicit
  parameters and only outputs explicit ones in order to produce valid RISC-V
  assembly output that is accepted by a subsequent assembler.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
void dumpRVOperation( std::ostream &os, const WIR_Operation &o );

/*!
  @brief dumpRVRegisterParameter dumps a %WIR register parameter to an output
         stream in a  RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] p A const reference to the %WIR register parameter to be dumped.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
void dumpRVRegisterParameter( std::ostream &os,
                              const WIR_RegisterParameter &p );

/*!
  @brief dumpRVComment dumps a %WIR comment container to an output stream in a
         RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] c A const reference to the %WIR comment to be dumped.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
void dumpRVComment( std::ostream &os, const WIR_Comment &c );

/*!
  @brief dumpRVFileInfo dumps a %WIR fileinfo container to an output stream in a
         RISC-V-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] f A const reference to the %WIR file information to be dumped.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/
void dumpRVFileInfo( std::ostream &os, const WIR_FileInfo &f );

}       // namespace WIR

#endif  // _RV_IO_H
