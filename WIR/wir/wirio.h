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
  @file wirio.h
  @brief This file provides elementary stream I/O routines for the %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_IO_H
#define _WIR_IO_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <iostream>
#include <list>
#include <set>
#include <string>

// Include WIR headers
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseImmediateParameter;
class WIR_BasicBlock;
class WIR_Comment;
class WIR_CompilationUnit;
class WIR_Data;
class WIR_FileInfo;
class WIR_Function;
class WIR_Instruction;
class WIR_Operation;
class WIR_AddressingModeParameter;
class WIR_ConditionFieldParameter;
class WIR_LabelParameter;
class WIR_RegisterParameter;
class WIR_Section;
class WIR_System;


//
// Type definitions for I/O functions.
//

/*!
  @brief WIR_BasicBlockDumper represents pointers to functions for
         processor-specific stream I/O of %WIR basic blocks.
*/
typedef void (* WIR_BasicBlockDumper)( std::ostream &,
                                       const WIR_BasicBlock & );

/*!
  @brief WIR_BlockLabelDumper represents pointers to functions for
         processor-specific formatting of labels of %WIR basic blocks.
*/
typedef void (* WIR_BlockLabelDumper)( std::ostream &,
                                       const WIR_BasicBlock & );

/*!
  @brief WIR_CommentDumper represents pointers to functions for
         processor-specific stream I/O of %WIR comments.
*/
typedef void (* WIR_CommentDumper)( std::ostream &, const WIR_Comment & );

/*!
  @brief WIR_CompilationUnitDumper represents pointers to functions for
         processor-specific stream I/O of %WIR compilation units.
*/
typedef void (* WIR_CompilationUnitDumper)( std::ostream &,
                                            const WIR_CompilationUnit & );

/*!
  @brief WIR_DataDumper represents pointers to functions for processor-specific
         stream I/O of %WIR data objects.
*/
typedef void (* WIR_DataDumper)( std::ostream &, const WIR_Data & );

/*!
  @brief WIR_DataDumper represents pointers to functions for processor-specific
         stream I/O of %WIR data objects.
*/
typedef void (* WIR_DataSectionDumper)( std::ostream &, WIR_Section *,
                                        const std::list<std::reference_wrapper<WIR_Data>> &,
                                        bool );

/*!
  @brief WIR_FileInfoDumper represents pointers to functions for
         processor-specific stream I/O of %WIR file information.
*/
typedef void (* WIR_FileInfoDumper)( std::ostream &, const WIR_FileInfo & );

/*!
  @brief WIR_FunctionDumper represents pointers to functions for
         processor-specific stream I/O of %WIR functions.
*/
typedef void (* WIR_FunctionDumper)( std::ostream &, const WIR_Function & );

/*!
  @brief WIR_InstructionDumper represents pointers to functions for
         processor-specific stream I/O of %WIR instructions.
*/
typedef void (* WIR_InstructionDumper)( std::ostream &,
                                        const WIR_Instruction & );

/*!
  @brief WIR_OperationDumper represents pointers to functions for
         processor-specific stream I/O of %WIR operations.
*/
typedef void (* WIR_OperationDumper)( std::ostream &, const WIR_Operation & );

/*!
  @brief WIR_AddressingModeParameterDumper represents pointers to functions for
         processor-specific stream I/O of %WIR addressing mode parameters.
*/
typedef void (* WIR_AddressingModeParameterDumper)( std::ostream &,
                                                    const WIR_AddressingModeParameter & );

/*!
  @brief WIR_ConditionFieldParameterDumper represents pointers to functions for
         processor-specific stream I/O of %WIR condition field parameters.
*/
typedef void (* WIR_ConditionFieldParameterDumper)( std::ostream &,
                                                    const WIR_ConditionFieldParameter & );

/*!
  @brief WIR_ImmediateParameterDumper represents pointers to functions for
         processor-specific stream I/O of %WIR immediate parameters.
*/
typedef void (* WIR_ImmediateParameterDumper)( std::ostream &,
                                               const WIR_BaseImmediateParameter & );

/*!
  @brief WIR_LabelParameterDumper represents pointers to functions for
         processor-specific stream I/O of %WIR label parameters.
*/
typedef void (* WIR_LabelParameterDumper)( std::ostream &,
                                           const WIR_LabelParameter & );

/*!
  @brief WIR_LdScriptDumper represents pointers to functions for the output of
         linker scripts.
*/
typedef void (* WIR_LdScriptDumper)( std::ostream &,
                                     const WIR_System & );

/*!
  @brief WIR_LdScriptSectionDumper represents pointers to functions for the
         output of ELF executable exections into linker scripts.
*/
typedef bool (* WIR_LdScriptSectionDumper)( std::ostream &,
                                            const WIR_Section & );

/*!
  @brief WIR_RegisterParameterDumper represents pointers to functions for
         processor-specific stream I/O of %WIR register parameters.
*/
typedef void (* WIR_RegisterParameterDumper)( std::ostream &,
                                              const WIR_RegisterParameter & );

/*!
  @brief WIR_SystemDumper represents pointers to functions for
         processor-specific stream I/O of %WIR systems.
*/
typedef void (* WIR_SystemDumper)( std::ostream &,
                                   const WIR_System & );


//
// I/O manipulators.
//

/*!
  @brief WIR_CommentCont adds a new index for I/O streams in order to switch the
         output of WIR_Comment containers during a WIR dump on or off.

  @return The I/O stream index which can be used to specify whether comments
          shall be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int WIR_CommentCont( void );

/*!
  @brief WIR_FileInfoCont adds a new index for I/O streams in order to switch
         the output of WIR_FileInfo containers during a WIR dump on or off.

  @return The I/O stream index which can be used to specify whether file
          information shall be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int WIR_FileInfoCont( void );

/*!
  @brief WIR_ImplicitParams adds a new index for I/O streams in order to switch
         the output of implicit parameters during a WIR dump on or off.

  @return The I/O stream index which can be used to specify whether file
          information shall be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int WIR_ImplicitParams( void );

/*!
  @brief WIR_Indentation adds a new index for I/O streams in order to indent
         output of %WIR dumps by a given number of white spaces.

  @return The I/O stream index which can be used to specify the number of
          indentation characters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int WIR_Indentation( void );

/*!
  @brief WIR_LdScript adds a new index for I/O streams in order to switch the
         output of a linker script during a %WIR system dump on or off.

  @return The I/O stream index which can be used to specify whether a linker
          script shall be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
int WIR_LdScript( void );

/*!
  @brief wir is an I/O manipulator that provides a simple and generic %WIR dump.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &wir( std::ostream &os );

/*!
  @brief comment switches the output of comment containers during a WIR dump on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &comment( std::ostream &os );

/*!
  @brief nocomment switches the output of comment containers during a WIR dump
         off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &nocomment( std::ostream &os );

/*!
  @brief defuse switches the output of DEF/USE information for register
         parameters during a %WIR dump on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &defuse( std::ostream &os );

/*!
  @brief nodefuse switches the output of DEF/USE information for register
         parameters during a %WIR dump off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &nodefuse( std::ostream &os );

/*!
  @brief fileinfo switches the output of fileinfo containers during a WIR dump
         on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &fileinfo( std::ostream &os );

/*!
  @brief nofileinfo switches the output of fileinfo containers during a WIR dump
         off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &nofileinfo( std::ostream &os );

/*!
  @brief functionregisters switches the output of a function's virtual registers
         during a %WIR dump on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &functionregisters( std::ostream &os );

/*!
  @brief nofunctionregisters switches the output of a function's virtual
         registers during a %WIR dump off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &nofunctionregisters( std::ostream &os );

/*!
  @brief implicitparams switches the output of implicit parameters during a %WIR
         dump on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &implicitparams( std::ostream &os );

/*!
  @brief noimplicitparams switches the output of implicit parameters during a
         %WIR dump off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &noimplicitparams( std::ostream &os );

/*!
  @brief jumptargets switches the output of explicit jump targets during a %WIR
         dump on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &jumptargets( std::ostream &os );

/*!
  @brief nojumptargets switches the output of explicit jump targets during a
         %WIR dump off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &nojumptargets( std::ostream &os );

/*!
  @brief ldscript switches the output of a linker script during a %WIR system's
         dump on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &ldscript( std::ostream &os );

/*!
  @brief noldscript switches the output of a linker script during a %WIR
         system's  dump off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &noldscript( std::ostream &os );

/*!
  @brief precolors switches the output of precolor information of functions
         during a %WIR dump on.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &precolors( std::ostream &os );

/*!
  @brief noprecolors switches the output of precolor information of functions
         during a %WIR dump off.

  @param[in] os A reference to the output stream to be manipulated.
  @return A reference to the manipulated output stream.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
std::ostream &noprecolors( std::ostream &os );


//
// WIR I/O functions.
//

/*!
  @brief dumpWIRBasicBlock dumps a %WIR basic block to an output stream in a
         generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] b A const reference to the %WIR basic block to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRBasicBlock( std::ostream &os, const WIR_BasicBlock &b );

/*!
  @brief dumpWIRBlockLabel dumps a %WIR basic block label to an output stream in
         a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] b A const reference to the %WIR basic block to be dumped.

  A basic block label starts with '.L' followed by the block's numerical ID.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRBlockLabel( std::ostream &os, const WIR_BasicBlock &b );

/*!
  @brief dumpWIRComment dumps a %WIR comment container to an output stream in a
         generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] c A const reference to the %WIR comment to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRComment( std::ostream &os, const WIR_Comment &c );

/*!
  @brief dumpWIRCompilationUnit dumps a %WIR compilation unit to an output
         stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] c A const reference to the %WIR compilation unit to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRCompilationUnit( std::ostream &os, const WIR_CompilationUnit &c );

/*!
  @brief dumpWIRData dumps a %WIR data object to an output stream in a generic,
         processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] d A const reference to the %WIR data object to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRData( std::ostream &os, const WIR_Data &d );

/*!
  @brief dumpWIRDataSection dumps a data section to an output stream in a
         generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] sec A pointer to the %WIR data section to be dumped, or nullptr if
                 data objects are not attached to a section.
  @param[in] l A const reference to a list of data objects to be dumped within
               the specified section.
  @param[in] firstSec A Boolean denoting whether the very first data section is
                      dumped or not (just used for line break formatting)

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRDataSection( std::ostream &os, WIR_Section *sec,
                         const std::list<std::reference_wrapper<WIR_Data>> &l,
                         bool firstSec );

/*!
  @brief dumpWIRFileInfo dumps a %WIR fileinfo container to an output stream in
         a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] f A const reference to the %WIR file information to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRFileInfo( std::ostream &os, const WIR_FileInfo &f );

/*!
  @brief dumpWIRFunction dumps a %WIR function to an output stream in a generic,
         processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] f A const reference to the %WIR function to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRFunction( std::ostream &os, const WIR_Function &f );

/*!
  @brief dumpWIRInstruction dumps a %WIR instruction to an output stream in a
         generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] i A const reference to the %WIR instruction to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRInstruction( std::ostream &os, const WIR_Instruction &i );

/*!
  @brief dumpWIROperation dumps a %WIR operation to an output stream in a
         generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] o A const reference to the %WIR operation to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIROperation( std::ostream &os, const WIR_Operation &o );

/*!
  @brief dumpWIRAddressingModeParameter dumps a %WIR addressing mode parameter
         to an output stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] p A const reference to the %WIR addressing mode parameter to be
               dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRAddressingModeParameter( std::ostream &os,
                                     const WIR_AddressingModeParameter &p );

/*!
  @brief dumpWIRConditionFieldParameter dumps a %WIR condition field parameter
         to an output stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] p A const reference to the %WIR condition field parameter to be
               dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRConditionFieldParameter( std::ostream &os,
                                     const WIR_ConditionFieldParameter &p );

/*!
  @brief dumpWIRImmediateParameter dumps a %WIR immediate parameter to an output
         stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] p A const reference to the %WIR immediate parameter to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRImmediateParameter( std::ostream &os,
                                const WIR_BaseImmediateParameter &p );

/*!
  @brief dumpWIRLabelParameter dumps a %WIR label parameter to an output stream
         in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] p A const reference to the %WIR label parameter to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRLabelParameter( std::ostream &os, const WIR_LabelParameter &p );

/*!
  @brief dumpWIRRegisterParameter dumps a %WIR register parameter to an output
         stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] p A const reference to the %WIR register parameter to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRRegisterParameter( std::ostream &os,
                               const WIR_RegisterParameter &p );

/*!
  @brief dumpLdScript dumps a %WIR system's memory layout as linker script to an
         output stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] sys A const reference to the %WIR system to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpLdScript( std::ostream &os, const WIR_System &sys );

/*!
  @brief dumpLDScriptRegions dumps a %WIR system's memory regions as linker
         script to an output stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] sys A const reference to the %WIR system to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpLDScriptRegions( std::ostream &os, const WIR_System &sys );

/*!
  @brief dumpLDScriptSections dumps a %WIR system's section mapping as linker
         script to an output stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] sys A const reference to the %WIR system to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpLDScriptSections( std::ostream &os, const WIR_System &sys );

/*!
  @brief dumpLDScriptSection dumps one section mapping as linker script to an
         output stream in a generic, processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] sec A const reference to the section to be written.
  @param[in,out] processedSections A reference to a set storing the names of
                                   sections that have already been processed.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpLDScriptSection( std::ostream &os, const WIR_Section &sec,
                          std::set<std::string> &processedSections );

/*!
  @brief dumpLDScriptSection dumps a %WIR special section as linker script entry
         to an output stream in a generic GNU-ld fashion.

  @param[in] os A reference to an output stream.
  @param[in] sec A const reference to the %WIR section to be dumped.
  @return true iff the given section was actually dumped, false otherwise.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
bool dumpLDScriptSection( std::ostream &os, const WIR_Section &sec );

/*!
  @brief dumpLDScriptSubSections dumps subsections of a %WIR section as linker
         script entry to an output stream in a generic GNU-ld fashion.

  @param[in] os A reference to an output stream.
  @param[in] sec A const reference to the %WIR section to be dumped.
  @param[in,out] processedSections A reference to a set storing the names of
                                   sections that have already been processed.
  @param[in] sectionBegin A const reference to a string for beginning a
                          subsection declaration.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpLDScriptSubSections( std::ostream &os, const WIR_Section &sec,
                              std::set<std::string> &processedSections,
                              const std::string &sectionBegin );

/*!
  @brief dumpWIRSystem dumps a %WIR system to an output stream in a generic,
         processor-independent fashion.

  @param[in] os A reference to an output stream.
  @param[in] s A const reference to the %WIR system to be dumped.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
void dumpWIRSystem( std::ostream &os, const WIR_System &s );

}       // namespace WIR

#endif  // _WIR_IO_H
