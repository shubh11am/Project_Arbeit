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
  @file wirio.cc
  @brief This file implements elementary stream I/O routines for the %WIR
         library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iomanip>
#include <list>
#include <map>
#include <set>
#include <string>
#include <utility>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


/*
  WIR_CommentCont adds a new index for I/O streams in order to switch the output
  of WIR_Comment containers during a WIR dump on or off.
*/
int WIR_CommentCont( void )
{
  DSTART( "int WIR_CommentCont()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_DefUse adds a new index for I/O streams in order to switch the output of
  DEF/USE information for register parameters during a WIR dump on or off.
*/
static int WIR_DefUse( void )
{
  DSTART( "int WIR_DefUse()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_FileInfoCont adds a new index for I/O streams in order to switch the
  output of WIR_FileInfo containers during a WIR dump on or off.
*/
int WIR_FileInfoCont( void )
{
  DSTART( "int WIR_FileInfoCont()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_FunctionRegisters adds a new index for I/O streams in order to switch the
  output of virtual registers during a dump of a WIR function on or off.
*/
static int WIR_FunctionRegisters( void )
{
  DSTART( "int WIR_FunctionRegisters()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_ImplicitParams adds a new index for I/O streams in order to switch the
  output of implicit parameters during a WIR dump on or off.
*/
int WIR_ImplicitParams( void )
{
  DSTART( "int WIR_ImplicitParams()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_Indentation adds a new index for I/O streams in order to indent output of
  WIR dumps by a given number of white spaces.
*/
int WIR_Indentation( void )
{
  DSTART( "int WIR_Indentation()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_JumpTargets adds a new index for I/O streams in order to switch the output
  of explicit jump targets during a WIR dump on or off.
*/
static int WIR_JumpTargets( void )
{
  DSTART( "int WIR_JumpTargets()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_LdScript adds a new index for I/O streams in order to switch the output
  of a linker script during a WIR system dump on or off.
*/
int WIR_LdScript( void )
{
  DSTART( "int WIR_LdScript()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  WIR_Precolors adds a new index for I/O streams in order to switch the output
  of precoloring information during a WIR dump on or off.
*/
static int WIR_Precolors( void )
{
  DSTART( "int WIR_Precolors()" );

  static int i = ios_base::xalloc();
  return( i );
};


/*
  wir is an I/O manipulator that provides a simple and generic WIR dump.
*/
std::ostream &wir( std::ostream &os )
{
  DSTART( "ostream& wir(ostream&)" );

  os.iword( WIR_ProcessorIO() ) = 0;
  return( os );
};


/*
  comment switches the output of comment containers during a WIR dump on.
*/
std::ostream &comment( std::ostream &os )
{
  DSTART( "ostream& comment(ostream&)" );

  os.iword( WIR_CommentCont() ) = 1;
  return( os );
};


/*
  nocomment switches the output of comment containers during a WIR dump off.
*/
std::ostream &nocomment( std::ostream &os )
{
  DSTART( "ostream& nocomment(ostream&)" );

  os.iword( WIR_CommentCont() ) = 0;
  return( os );
};


/*
  defuse switches the output of DEF/USE information for register parameters
  during a WIR dump on.
*/
std::ostream &defuse( std::ostream &os )
{
  DSTART( "ostream& defuse(ostream&)" );

  os.iword( WIR_DefUse() ) = 1;
  return( os );
};


/*
  nodefuse switches the output of DEF/USE information for register parameters
  during a WIR dump off.
*/
std::ostream &nodefuse( std::ostream &os )
{
  DSTART( "ostream& nodefuse(ostream&)" );

  os.iword( WIR_DefUse() ) = 0;
  return( os );
};


/*
  fileinfo switches the output of fileinfo containers during a WIR dump on.
*/
std::ostream &fileinfo( std::ostream &os )
{
  DSTART( "ostream& fileinfo(ostream&)" );

  os.iword( WIR_FileInfoCont() ) = 1;
  return( os );
};


/*
  nofileinfo switches the output of fileinfo containers during a WIR dump off.
*/
std::ostream &nofileinfo( std::ostream &os )
{
  DSTART( "ostream& nofileinfo(ostream&)" );

  os.iword( WIR_FileInfoCont() ) = 0;
  return( os );
};


/*
  functionregisters switches the output of a function's virtual registers during
  a WIR dump on.
*/
std::ostream &functionregisters( std::ostream &os )
{
  DSTART( "ostream& functionregisters(ostream&)" );

  os.iword( WIR_FunctionRegisters() ) = 1;
  return( os );
};


/*
  nofunctionregisters switches the output of a function's virtual registers
  during a WIR dump off.
*/
std::ostream &nofunctionregisters( std::ostream &os )
{
  DSTART( "ostream& nofunctionregisters(ostream&)" );

  os.iword( WIR_FunctionRegisters() ) = 0;
  return( os );
};


/*
  implicitparams switches the output of implicit parameters during a WIR dump
  on.
*/
std::ostream &implicitparams( std::ostream &os )
{
  DSTART( "ostream& implicitparams(ostream&)" );

  os.iword( WIR_ImplicitParams() ) = 1;
  return( os );
};


/*
  noimplicitparams switches the output of implicit parameters during a WIR dump
  off.
*/
std::ostream &noimplicitparams( std::ostream &os )
{
  DSTART( "ostream& noimplicitparams(ostream&)" );

  os.iword( WIR_ImplicitParams() ) = 0;
  return( os );
};


/*
  jumptargets switches the output of explicit jump targets during a WIR dump on.
*/
std::ostream &jumptargets( std::ostream &os )
{
  DSTART( "ostream& jumptargets(ostream&)" );

  os.iword( WIR_JumpTargets() ) = 1;
  return( os );
};


/*
  nojumptargets switches the output of explicit jump targets during a WIR dump
  off.
*/
std::ostream &nojumptargets( std::ostream &os )
{
  DSTART( "ostream& nojumptargets(ostream&)" );

  os.iword( WIR_JumpTargets() ) = 0;
  return( os );
};


/*
  ldscript switches the output of a linker script during a WIR system's dump on.
*/
std::ostream &ldscript( std::ostream &os )
{
  DSTART( "ostream& ldscript(ostream&)" );

  os.iword( WIR_LdScript() ) = 1;
  return( os );
};


/*
  noldscript switches the output of a linker script during a WIR system's dump
  off.
*/
std::ostream &noldscript( std::ostream &os )
{
  DSTART( "ostream& noldscript(ostream&)" );

  os.iword( WIR_LdScript() ) = 0;
  return( os );
};


/*
  precolors switches the output of precolor information of functions during a
  WIR dump on.
*/
std::ostream &precolors( std::ostream &os )
{
  DSTART( "ostream& precolors(ostream&)" );

  os.iword( WIR_Precolors() ) = 1;
  return( os );
};


/*
  noprecolors switches the output of precolor information of functions during a
  WIR dump off.
*/
std::ostream &noprecolors( std::ostream &os )
{
  DSTART( "ostream& noprecolors(ostream&)" );

  os.iword( WIR_Precolors() ) = 0;
  return( os );
};


/*
  dumpWIRBasicBlock dumps a %WIR basic block to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIRBasicBlock( std::ostream &os, const WIR_BasicBlock &b )
{
  DSTART( "void dumpWIRBasicBlock(ostream&, const WIR_BasicBlock&)" );

  // Dump comments and file information attached to the basic block first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : b.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : b.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  // Output label of basic block next.
  WIR_Registry::getBlockLabelDumper( os.iword( WIR_ProcessorIO() ) )( os, b );
  os << ":" << endl;

  // Output instructions.
  for ( WIR_Instruction &i : b )
    os << i;
};


/*
  dumpWIRBlockLabel dumps a %WIR basic block label to an output stream in a
  generic, processor-independent fashion.

  If a basic block is the first one within a function, its label is equal to the
  function's name. Otherwise, a basic block label starts with '.L' followed by
  the block's numerical ID.
*/
void dumpWIRBlockLabel( std::ostream &os, const WIR_BasicBlock &b )
{
  DSTART( "void dumpWIRBlockLabel(ostream&, const WIR_BasicBlock&)" );

  if ( b.isInserted() &&
       ( b.getFunction().getBasicBlocks().front().get() == b ) )
    os << b.getFunction().getName();
  else
    os << ".L" << dec << b.getID();
};


/*
  dumpWIRComment dumps a WIR comment container to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIRComment( std::ostream &os, const WIR_Comment &c )
{
  DSTART( "void dumpWIRComment(ostream&, const WIR_Comment&)" );

  os << "; " << c.getText();
};


/*
  dumpWIRCompilationUnit dumps a %WIR compilation unit to an output stream in a
  generic, processor-independent fashion.
*/
void dumpWIRCompilationUnit( std::ostream &os, const WIR_CompilationUnit &c )
{
  DSTART( "void dumpWIRCompilationUnit(ostream&, const WIR_CompilationUnit&)" );

  // Dump comments and file information attached to the compilation unit first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : c.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : c.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  os << "FILE " << c.getName() << endl << "{" << endl;

  // Output data objects.
  // Step 1: Group all data objects according to their sections.
  map<reference_wrapper<WIR_Section>, list<reference_wrapper<WIR_Data>>, WIR_Compare<WIR_Section>> dataLayout;

  for ( WIR_Data &d : c.getData() )
    if ( c.isInserted() ) {
      WIR_Section &sec =
        const_cast<WIR_Section &>( c.getSystem().findSymbol( d ).getSection() );
      dataLayout[ sec ].push_back( d );
    }

  // Step 2: Output the data objects section-wise.
  for ( auto it = dataLayout.begin(); it != dataLayout.end(); ++it )
    WIR_Registry::getDataSectionDumper( os.iword( WIR_ProcessorIO() ) )(
      os, &(it->first.get()), it->second, it == dataLayout.begin() );

  // Output functions.
  for ( auto it = c.getFunctions().begin(); it != c.getFunctions().end();
        ++it ) {
    if ( it != c.getFunctions().begin() )
      os << endl;
    os << endl << *it;
  }

  os << endl << "};" << endl;
};


/*
  dumpWIRData dumps a %WIR data object to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIRData( std::ostream &os, const WIR_Data &d )
{
  DSTART( "void dumpWIRData(ostream&, const WIR_Data&)" );

  // Dump comments and file information attached to the data object first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : d.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : d.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  os << "DATA " << d.getName() << endl << "{" << endl;

  if ( d.isInitialized() ) {
    for ( auto it = d.getInitData().begin(); it != d.getInitData().end();
          ++it ) {
      if ( it == d.getInitData().begin() )
        os << " ";
      for ( auto &s : it->get().getValues() )
        os << " " << s;
    }
    os << endl;
  }

  os << "};" << endl;
};


/*
  dumpWIRDataSection dumps a data section to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIRDataSection( std::ostream &os, WIR_Section *sec,
                         const std::list<std::reference_wrapper<WIR_Data>> &l,
                         bool firstSec )
{
  DSTART(
    "void dumpWIRDataSection(ostream&, WIR_Section*, const list<reference_wrapper<WIR_Data> >&, bool)" );

  // Dump section first.
  if ( !firstSec )
    os << endl << endl;

  if ( sec != nullptr )
    os << "SECTION " << sec->getName()
       << endl << "{" << endl << endl;

  // Dump data objects next.
  bool firstData = true;
  for ( WIR_Data &d : l ) {
    if ( firstData )
      firstData = false;
    else
      os << endl << endl;
    os << d;
  }

  if ( sec != nullptr )
    os << endl << "};" << endl;
};


/*
  dumpWIRFileInfo dumps a WIR fileinfo container to an output stream in a
  generic, processor-independent fashion.
*/
void dumpWIRFileInfo( std::ostream &os, const WIR_FileInfo &f )
{
  DSTART( "void dumpWIRFileInfo(ostream&, const WIR_FileInfo&)" );

  os << "; file \"" << f.getFileName() << "\", line " << dec
     << f.getLineNumber();
};


/*
  dumpWIRFunction dumps a %WIR function to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIRFunction( std::ostream &os, const WIR_Function &f )
{
  DSTART( "void dumpWIRFunction(ostream&, const WIR_Function&)" );

  // Dump comments and file information attached to the function first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : f.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : f.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  // Dump function header with the function name next.
  os << "FUNCTION " << f.getName() << endl << "{" << endl;

  // Determine whether additional information beyond a function's basic blocks
  // shall be dumped at all.
  bool dumpAdditionalInfos =
    ( os.iword( WIR_FunctionRegisters() ) == 1 ) ||
    ( os.iword( WIR_Precolors() ) == 1 );

  if ( dumpAdditionalInfos )
    os << "  {" << endl;

  // Dump virtual registers owned by a WIR function.
  if ( os.iword( WIR_FunctionRegisters() ) == 1 ) {
    bool dump = false;
    os << "    registers:";
    for ( WIR_VirtualRegister &r : f.getVirtualRegisters() ) {
      os << " " << r.getName();
      dump = true;
    }
    if ( !dump )
      os << " ";
    os << ";" << endl;
  }

  // Dump precolor information.
  if ( os.iword( WIR_Precolors() ) == 1 ) {
    bool dump = false;
    os << "    precolors:";
    for ( auto p : f.mPrecolors ) {
      auto &vreg = p.first.get();

      os << " " << vreg.getName() << "=" << (p.second)->getName();
      dump = true;
    }
    if ( !dump )
      os << " ";
    os << ";" << endl;
  }

  if ( dumpAdditionalInfos )
    os << "  }" << endl;

  // Output basic blocks.
  for ( WIR_BasicBlock &b : f )
    os << b;
  os << "};" << endl;
};


/*
  dumpWIRInstruction dumps a %WIR instruction to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIRInstruction( std::ostream &os, const WIR_Instruction &i )
{
  DSTART( "void dumpWIRInstruction(ostream&, const WIR_Instruction&)" );

  // Dump comments and file information attached to the instruction first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : i.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : i.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  // Output operations.
  auto opers = i.getOperations();
  for ( auto it = opers.begin(); it != opers.end(); ++it ) {
    // The first operation of an instruction is indented 8 chars, all following
    // operations are prefixed by a "||" and one indentation char.
    if ( it == opers.begin() )
      os.iword( WIR_Indentation() ) = 1;
    else
      os.iword( WIR_Indentation() ) = 0;

    os << (*it).get() << endl;
  }

  // Reset indentation level.
  os.iword( WIR_Indentation() ) = 0;
};


/*
  dumpWIROperation dumps a WIR operation to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIROperation( std::ostream &os, const WIR_Operation &o )
{
  DSTART( "void dumpWIROperation(ostream&, const WIR_Operation&)" );

  // Indent output.
  if ( os.iword( WIR_Indentation() ) == 1 )
    os << string( 8, ' ' );
  else
    os << "     || ";

  // Dump comments and file information attached to the operation first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : o.getContainers<WIR_Comment>() )
      os << cont << endl << string( 8, ' ' );
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : o.getContainers<WIR_FileInfo>() )
      os << cont << endl << string( 8, ' ' );

  // Output the operation's opcode and format.
  os << o.getOpCode().getName() << " " << o.getOperationFormat().getName();

  // Output parameters.
  auto params = o.getParameters();
  bool firstParam = true;

  for ( auto it = params.begin(); it != params.end(); ++it ) {
    WIR_Parameter &p = (*it).get();

    // Check whether implicit parameters shall be skipped.
    bool emitParam = true;
    if ( ( os.iword( WIR_ImplicitParams() ) == 0 ) && p.isImplicit() )
      emitParam = false;

    if ( emitParam ) {
      if ( !firstParam )
        os << ",";
      firstParam = false;

      // Output separator between parameters.
      os << " ";

      // Output current parameter itself.
      os << p;
    }
  }

  // Terminate output with a semicolon.
  os << ";";

  // Output explicit jump targets.
  if ( os.iword( WIR_JumpTargets() ) == 1 ) {
    os << " (ID = " << o.getID();

    if ( o.jumpTargetsAdded() ) {
      os << ", targets =";
      for ( WIR_BasicBlock &t : o.getJumpTargets() )
        os << " " << t.getID();
    }

    os << ")";
  }
};


/*
  dumpWIRAddressingModeParameter dumps a WIR addressing mode parameter to an
  output stream in a generic, processor-independent fashion.
*/
void dumpWIRAddressingModeParameter( std::ostream &os,
                                     const WIR_AddressingModeParameter &p )
{
  DSTART(
    "void dumpWIRAddressingModeParameter(ostream&, const WIR_AddressingModeParameter&)" );

  // Output the parameter's register name.
  os << p.getAddressingMode().getName();
};


/*
  dumpWIRConditionFieldParameter dumps a WIR condition field parameter to an
  output stream in a generic, processor-independent fashion.
*/
void dumpWIRConditionFieldParameter( std::ostream &os,
                                     const WIR_ConditionFieldParameter &p )
{
  DSTART(
    "void dumpWIRConditionFieldParameter(ostream&, const WIR_ConditionFieldParameter&)" );

  // Output the parameter's register name.
  os << p.getCondition().getName();
};


/*
  dumpWIRImmediateParameter dumps a WIR immediate parameter to an output stream
  in a generic, processor-independent fashion.
*/
void dumpWIRImmediateParameter( std::ostream &os,
                                const WIR_BaseImmediateParameter &p )
{
  DSTART(
    "void dumpWIRImmediateParameter(ostream&, const WIR_BaseImmediateParameter&)" );

  // Output the parameter's immediate value.
  os << p.getValueString();
};


/*
  dumpWIRLabelParameter dumps a WIR label parameter to an output stream in a
  generic, processor-independent fashion.
*/
void dumpWIRLabelParameter( std::ostream &os,
                            const WIR_LabelParameter &p )
{
  DSTART( "void dumpWIRLabelParameter(ostream&, const WIR_LabelParameter&)" );

  if ( p.getLabelType() == WIR_SymbolType::block ) {
    WIR_Registry::getBlockLabelDumper( os.iword( WIR_ProcessorIO() ) )(
      os, p.getBasicBlock() );
  } else

  if ( p.getLabelType() == WIR_SymbolType::data )
    os << p.getData().getName();
  else
    os << p.getFunction().getName();
};


/*
  dumpWIRRegisterParameter dumps a WIR register parameter to an output stream
  in a generic, processor-independent fashion.
*/
void dumpWIRRegisterParameter( std::ostream &os,
                               const WIR_RegisterParameter &p )
{
  DSTART(
    "void dumpWIRRegisterParameter(ostream&, const WIR_RegisterParameter&)" );

  // Output the parameter's register name.
  os << p.getRegister().getName();

  if ( os.iword( WIR_DefUse() ) != 0 ) {
    // Dump whether a register parameter is defined, used or defused.
    os << " (";

    switch ( p.getUsage() ) {

      case WIR_Usage::def: {
        os << "def";
        break;
      }

      case WIR_Usage::use: {
        os << "use";
        break;
      }

      case WIR_Usage::defuse: {
        os << "defuse";
        break;
      }
    }

    os << ", ID = " << p.getID() << ")";
  }
};


/*
  dumpLdScript dumps a WIR system's memory layout as linker script to an output
  stream in a generic, processor-independent fashion.
*/
void dumpLdScript( std::ostream &os, const WIR_System &sys )
{
  DSTART( "void dumpLdScript(ostream&, const WIR_System&)" );

  (void) os;
  (void) sys;
};


/*
  dumpLDScriptRegions dumps a WIR system's memory regions as linker script to
  an output stream in a generic, processor-independent fashion.
*/
void dumpLDScriptRegions( std::ostream &os, const WIR_System &sys )
{
  DSTART( "void dumpLDScriptRegions(ostream&, const WIR_System&)" );

  os << "/* Begin of available memory regions */" << endl << "MEMORY" << endl
     << "{" << endl;

  for ( WIR_MemoryRegion &r : sys.getComponents<WIR_MemoryRegion>() ) {
    os << "  " << left << setw( 15 ) << r.getName();
    string attr = "(";

    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::read )
      attr += "r";
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::write )
      attr += "w";
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::execute )
      attr += "x";
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::allocated )
      attr += "a";
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::initialized )
      attr += "i";
    attr += ")";
    os << left << setw( 6 ) << attr;

    os << ": org = 0x" << setw( 8 ) << hex << r.getBaseAddress() << ", len = "
       << right << setw( 7 ) << dec << r.getLength();

    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::cached )
      os << " /* cached */";

    os << endl;
  }

  os << "}" << endl << "/* End of available memory regions */" << endl << endl;
};


/*
  dumpLDScriptSections dumps a WIR system's section mapping as linker script to
  an output stream in a generic, processor-independent fashion.
*/
void dumpLDScriptSections( std::ostream &os, const WIR_System &sys )
{
  DSTART( "void dumpLDScriptSections(ostream&, const WIR_System&)" );

  set<string> processedSections;
  multimap<WIR_MemoryAddress, reference_wrapper<WIR_Section>> addressMap;
  for ( WIR_Symbol &sym : sys.getSymbols() )
    addressMap.insert(
      make_pair(
        sym.getBaseAddress(),
        reference_wrapper<WIR_Section>(
          const_cast<WIR_Section &>( sym.getSection() ) ) ) );

  os << "/* Begin of section mapping */" << endl << "SECTIONS" << endl << "{"
     << endl;

  for ( auto &p : addressMap )
    dumpLDScriptSection( os, p.second, processedSections );

  // Write remaining sections that have not yet been processed.
  for ( WIR_MemoryRegion &r : sys.getComponents<WIR_MemoryRegion>() )
    for ( WIR_Section &sec : r.getSections() )
      dumpLDScriptSection( os, sec, processedSections );

  // Write copy table.
  os << "  .copytab :" << endl
     << "  {" << endl
     << "    /*" << endl
     << "      Create the clear and copy tables that tell the startup code "
     << "which memory" << endl
     << "      areas to clear and to copy, respectively." << endl
     << "    */" << endl
     << "    . = ALIGN(4);" << endl
     << "    PROVIDE(__clear_table = .) ;" << endl
     << "    LONG(0 + ADDR(.bss));   LONG(SIZEOF(.bss));" << endl
     << "    LONG(0 + ADDR(.sbss));  LONG(SIZEOF(.sbss));" << endl
     << "    LONG(0 + ADDR(.zbss));  LONG(SIZEOF(.zbss));" << endl
     << "    LONG(-1);               LONG(-1);" << endl
     << "    PROVIDE(__copy_table = .) ;" << endl;

  // Process all sections to find relocation information.
  auto regions = sys.getComponents<WIR_MemoryRegion>();
  auto rodataRegionIt = regions.end();
  for ( auto it = regions.begin(); it != regions.end(); ++it )
    for ( WIR_Section &sec : it->get().getSections() ) {
      if ( sec.getName() == ".rodata" )
        rodataRegionIt = it;

      if ( processedSections.count( sec.getName() ) && sec.isLoadRegionSet() )
        os << "    LONG(LOADADDR(" << sec.getName() << "));"
           << "    LONG(0 + ADDR(" << sec.getName() << "));"
           << "    LONG(SIZEOF(" << sec.getName() << "));" << endl;
    }

  os << "    LONG(-1);                 LONG(-1);                  LONG(-1);"
     << endl
     << "  } > " << rodataRegionIt->get().getName() << endl << endl;

  // End of memory layout.
  os << "  _end = . ;" << endl
     << "  PROVIDE(end = _end) ;" << endl << endl;

  // Write DWARF debugging information.
  os << "  /* Define a default symbol for address 0. */" << endl
     << "  NULL = DEFINED (NULL) ? NULL : 0 ;" << endl << endl
     << "  /*" << endl
     << "    DWARF debug sections. Symbols in the DWARF debugging sections are "
     << "relative" << endl
     << "    to the beginning of the section, so we have them begin at 0."
     << endl << "  */" << endl << endl
     << "  /* DWARF 1 */" << endl
     << "  .comment         0 : { *(.comment) }" << endl
     << "  .debug           0 : { *(.debug) }" << endl
     << "  .line            0 : { *(.line) }" << endl << endl
     << "  /* GNU DWARF 1 extensions */" << endl
     << "  .debug_srcinfo   0 : { *(.debug_srcinfo) }" << endl
     << "  .debug_sfnames   0 : { *(.debug_sfnames) }" << endl << endl
     << "  /* DWARF 1.1 and DWARF 2 */" << endl
     << "  .debug_aranges   0 : { *(.debug_aranges) }" << endl
     << "  .debug_pubnames  0 : { *(.debug_pubnames) }" << endl << endl
     << "  /* DWARF 2 */" << endl
     << "  .debug_info      0 : { *(.debug_info) }" << endl
     << "  .debug_abbrev    0 : { *(.debug_abbrev) }" << endl
     << "  .debug_line      0 : { *(.debug_line) }" << endl
     << "  .debug_frame     0 : { *(.debug_frame) }" << endl
     << "  .debug_str       0 : { *(.debug_str) }" << endl
     << "  .debug_loc       0 : { *(.debug_loc) }" << endl
     << "  .debug_macinfo   0 : { *(.debug_macinfo) }" << endl
     << "  .debug_ranges    0 : { *(.debug_ranges) }" << endl;

  os << "}" << endl << "/* End of section mapping */" << endl;
};


/*
  dumpLDScriptSection dumps one section mapping as linker script to an output
  stream in a generic, processor-independent fashion.
*/
void dumpLDScriptSection( std::ostream &os, const WIR_Section &sec,
                          std::set<std::string> &processedSections )
{
  DSTART(
    "void dumpLDScriptSection(ostream&, const WIR_Section&, set<string>&)" );

  // Handle names of hierarchical sections.
  string sectionName = sec.getName();
  if ( sectionName.find( ".", 1 ) != string::npos )
    sectionName = sectionName.substr( 0, sectionName.find( ".", 1 ) );

  // Check whether section assignment has already been created for this base
  // section.
  if ( processedSections.count( sectionName ) )
    return;
  processedSections.insert( sectionName );

  // Start section assignment.
  string sectionBegin =
    "_" + sectionName.substr( 1, sectionName.size() - 1 ) + "_begin";
  string sectionEnd =
    "_" + sectionName.substr( 1, sectionName.size() - 1 ) + "_end";
  os << "  " << sectionName << " :" << endl << "  {" << endl << "    "
     << sectionBegin << " = .;" << endl;

  bool written =
    WIR_Registry::getLdScriptSectionDumper(
      os.iword( WIR_ProcessorIO() ) )( os, sec );

  // If current section is not handeled by processor-specific section I/O
  // function, it could be a special section.
  if ( !written )
    written = WIR_Registry::getLdScriptSectionDumper( 0 )( os, sec );

  // If no special section handling was done, simply write a standard section.
  if ( !written )
    os << "    *(" << sectionName << ")" << endl;

  // Write subsections, if any.
  dumpLDScriptSubSections( os, sec, processedSections, sectionBegin );

  // Finish standard sections.
  if ( !written )
    os << "    *(" << sectionName << ".*)" << endl;

  // Close section assignment block.
  os << "    " << sectionEnd << " = .;" << endl
     << "    . = ALIGN(8);" << endl
     << "  } > " << sec.getRegion().getName();

  // Write relocation information for content to be copied from ROM.
  if ( sec.isLoadRegionSet() )
    os << " AT > " << sec.getLoadRegion().getName();
  os << endl << endl;
};


/*
  dumpLDScriptSection dumps a WIR special section as linker script entry to an
  output stream in a generic GNU-ld fashion.
*/
bool dumpLDScriptSection( std::ostream &os, const WIR_Section &sec )
{
  DSTART( "bool dumpLDScriptSection(ostream&, const WIR_Section&)" );

  // Write .rodata section.
  if ( sec.getName() == ".rodata" ) {
    os << "    *(.rodata)" << endl
       << "    *(.rodata.*)" << endl
       << "    *(.gnu.linkonce.r.*)" << endl
       << "    *(.rodata1)" << endl
       << "    *(.toc)" << endl;
    return( true );
  }

  // Write .bss section.
  if ( sec.getName() == ".bss" ) {
    os << "    BSS_BASE = . ;" << endl
       << "    _bss_begin = .;" << endl
       << "    *(.bss)" << endl
       << "    *(.bss.*)" << endl
       << "    *(.gnu.linkonce.b.*)" << endl
       << "    *(COMMON)" << endl;
    return( true );
  }

  // Write .sbss section.
  if ( sec.getName() == ".sbss" ) {
    os << "    PROVIDE(__sbss_start = .);" << endl
       << "    *(.sbss)" << endl
       << "    *(.sbss.*)" << endl
       << "    *(.gnu.linkonce.sb.*)" << endl;
    return( true );
  }

  // Write .zbss section.
  if ( sec.getName() == ".zbss" ) {
    os << "    ZBSS_BASE = . ;" << endl
       << "    *(.zbss)" << endl
       << "    *(.zbss.*)" << endl
       << "    *(.gnu.linkonce.zb.*)" << endl
       << "    *(.bbss)" << endl
       << "    *(.bbss.*)" << endl
       << "    . = ALIGN(8);" << endl
       << "    ZBSS_END = . ;" << endl;
    return( true );
  }

  // Write C++ constructor/destructor handling sections to ensure gcc
  // compatibility.
  if ( sec.getName() == ".eh_frame" ) {
    os << "    /*" << endl
       << "      C++ exception handling tables." << endl
       << "      NOTE: gcc emits .eh_frame sections when compiling C sources "
       << "with" << endl
       << "      debugging enabled (-g). If you can be sure that your final "
       << "application" << endl
       << "      consists exclusively of C objects (i.e., no C++ objects), you "
       << "may use the" << endl
       << "      -R option of the \"strip\" and \"objcopy\" utilities to "
       << "remove the .eh_frame" << endl
       << "      section from the executable." << endl
       << "    */" << endl
       << "    *(.gcc_except_table)" << endl
       << "    __EH_FRAME_BEGIN__ = . ;" << endl
       << "    *(.eh_frame)" << endl
       << "    __EH_FRAME_END__ = . ;" << endl;
    return( true );
  }

  // Write section for constructors.
  if ( sec.getName() == ".ctors" ) {
    os << "    /* Constructors and destructors. */" << endl
       << "    __CTOR_LIST__ = . ;" << endl
       << "    LONG((__CTOR_END__ - __CTOR_LIST__) / 4 - 2);" << endl
       << "    *(.ctors)" << endl
       << "    LONG(0) ;" << endl
       << "    __CTOR_END__ = . ;" << endl;
    return( true );
  }

  // Write section for destructors.
  if ( sec.getName() == ".dtors" ) {
    os << "    __DTOR_LIST__ = . ;" << endl
       << "    LONG((__DTOR_END__ - __DTOR_LIST__) / 4 - 2);" << endl
       << "    *(.dtors)" << endl
       << "    LONG(0) ;" << endl
       << "    __DTOR_END__ = . ;" << endl;
    return( true );
  }

  return( false );
};


/*
  dumpLDScriptSubSections dumps subsections of a WIR section as linker script
  entry to an output stream in a generic GNU-ld fashion.
*/
void dumpLDScriptSubSections( std::ostream &os, const WIR_Section &sec,
                              std::set<std::string> &processedSections,
                              const std::string &sectionBegin )
{
  DSTART(
    "void dumpLDScriptSubSections(ostream&, const WIR_Section&, set<string>&, const string&)" );

  // List to store all subsections.
  list<reference_wrapper<WIR_Section>> subSectionList;

  // Detect subsections and store them.
  for ( WIR_Section &ssec : sec.getRegion().getSections() ) {
    if ( !processedSections.count( ssec.getName() ) &&
         ( ssec.getName().substr( 0, sec.getName().size() + 1 ) ==
             sec.getName() + "." ) ) {
      subSectionList.push_back( reference_wrapper<WIR_Section>( ssec ) );
      processedSections.insert( ssec.getName() );
    }
  }

  // Write list of subsections.
  WIR_MemoryAddress relocCounter = WIR_MemoryAddress( 0 );
  for ( WIR_Section &ssec : subSectionList ) {
    if ( ssec.isStartSet() && ( ssec.getStart() != relocCounter ) ) {
      relocCounter = ssec.getStart();
      os << "    . = " << sectionBegin << " + 0x" << hex << relocCounter << dec
         << ";" << endl;
    }

    if ( ssec.getAlignment() )
      os << "    . = ALIGN(" << ssec.getAlignment() << ");" << endl;

    os << "    *(" << ssec.getName() << ")" << endl
       << "    *(" << ssec.getName() << ".*)" << endl;
  }
};


/*
  dumpWIRSystem dumps a WIR system to an output stream in a generic,
  processor-independent fashion.
*/
void dumpWIRSystem( std::ostream &os, const WIR_System &s )
{
  DSTART( "void dumpWIRSystem(ostream&, const WIR_System&)" );

  // Dump comments and file information attached to the system first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : s.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : s.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  // Output compilation units.
  for ( auto it = s.getCompilationUnits().begin();
        it != s.getCompilationUnits().end(); ++it ) {
    if ( it != s.getCompilationUnits().begin() )
      os << endl << endl;
    os << *it;
  }
};

}       // namespace WIR
