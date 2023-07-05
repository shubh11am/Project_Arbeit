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
  @file tcio.h
  @brief This file provides TriCore-specific stream I/O routines for the %WIR
         library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_IO_H
#define _TC_IO_H


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
class WIR_Data;
class WIR_Function;
class WIR_Operation;
class WIR_RegisterParameter;
class WIR_Section;
class WIR_System;


/*!
  @brief tricore is an I/O manipulator that provides a TriCore assembler dump of
         a %WIR.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &tricore( std::ostream &os );

/*!
  @brief dumpTCBasicBlock dumps a %WIR basic block to an output stream in a
         TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] b A const reference to the %WIR basic block to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCBasicBlock( std::ostream &os, const WIR_BasicBlock &b );

/*!
  @brief dumpTCCompilationUnit dumps a %WIR compilation unit to an output stream
         in a TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] c A const reference to the %WIR compilation unit to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCCompilationUnit( std::ostream &os, const WIR_CompilationUnit &c );

/*!
  @brief dumpTCData dumps a %WIR data object to an output stream in a TriCore-
         specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] d A const reference to the %WIR data object to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCData( std::ostream &os, const WIR_Data &d );

/*!
  @brief dumpTCDataSection dumps a data section to an output stream in a
         TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] sec A pointer to the %WIR data section to be dumped.
  @param[in] l A const reference to a list of data objects to be dumped within
               the specified section.
  @param[in] firstSec A Boolean denoting whether the very first data section is
                      dumped or not (just used for line break formatting)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCDataSection( std::ostream &os, WIR_Section *sec,
                        const std::list<std::reference_wrapper<WIR_Data>> &l,
                        bool firstSec );

/*!
  @brief dumpTCFunction dumps a %WIR function to an output stream in a
         TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] f A const reference to the %WIR function to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCFunction( std::ostream &os, const WIR_Function &f );

/*!
  @brief dumpTCLdScript dumps a %WIR system's memory layout as linker script to
         an output stream in a TriCore- and GNU-ld specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] sys A const reference to the %WIR system to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCLdScript( std::ostream &os, const WIR_System &sys );

/*!
  @brief dumpTCLdScriptSection dumps a %WIR section as linker script entry to
         an output stream in a TriCore- and GNU-ld specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] sec A const reference to the %WIR section to be dumped.
  @return true iff the given section was actually dumped, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool dumpTCLdScriptSection( std::ostream &os, const WIR_Section &sec );

/*!
  @brief dumpTCOperation dumps a %WIR operation to an output stream in a
         TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] o A const reference to the %WIR operation to be dumped.

  While dumping an operation's parameters, this function omits any implicit
  parameters and only outputs explicit ones in order to produce valid TriCore
  assembly output that is accepted by a subsequent assembler.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCOperation( std::ostream &os, const WIR_Operation &o );

/*!
  @brief dumpTCRegisterParameter dumps a %WIR register parameter to an output
         stream in a TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] p A const reference to the %WIR register parameter to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCRegisterParameter( std::ostream &os,
                              const WIR_RegisterParameter &p );

/*!
  @brief dumpTCComment dumps a %WIR comment container to an output stream in a
         TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] c A const reference to the %WIR comment to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCComment( std::ostream &os, const WIR_Comment &c );

/*!
  @brief dumpTCFileInfo dumps a %WIR fileinfo container to an output stream in a
         TriCore-specific fashion.

  @param[in] os A reference to an output stream.
  @param[in] f A const reference to the %WIR file information to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpTCFileInfo( std::ostream &os, const WIR_FileInfo &f );

}       // namespace WIR

#endif  // _TC_IO_H
