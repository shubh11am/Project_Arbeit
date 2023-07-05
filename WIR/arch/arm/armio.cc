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
  @file armio.cc
  @brief This file implements ARM-specific stream I/O routines for the %WIR
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
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <sstream>

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


/*!
  @brief previousCodeSection is used to keep track of the currently used section
         for the assembly output of functions and basic blocks.

  previousSection is used to control the generation of .section directives for
  code.
*/
static WIR_id_t previousCodeSection;


/*
  arm is an I/O manipulator that provides an ARM assembler dump of a WIR.
*/
std::ostream &arm( std::ostream &os )
{
  DSTART( "ostream& arm(ostream&)" );

  os.iword( WIR_ProcessorIO() ) = ARM_Base::getProcessorTypeID();
  return( os );
};


/*
  dumpARMBasicBlock dumps a WIR basic block to an output stream in an ARM-
  specific fashion.
*/
void dumpARMBasicBlock( std::ostream &os, const WIR_BasicBlock &b )
{
  DSTART( "void dumpARMBasicBlock(ostream&, const WIR_BasicBlock&)" );

  // Check whether section/region directives have to be emitted.
  if ( b.isInserted() ) {
    WIR_Function &f = b.getFunction();
    if ( f.isInserted() ) {
      WIR_CompilationUnit &c = f.getCompilationUnit();
      if ( c.isInserted() ) {
        WIR_System &sys = c.getSystem();
        WIR_Symbol &sym = sys.findSymbol( b );
        auto &sec = sym.getSection();

        // Emit .section directive if necessary.
        if ( sec.getID() != previousCodeSection ) {
          os << string( 8, ' ' ) << ".section" << string( 8, ' ' )
             << sec.getName();

          if ( sec.getName() != ".text" ) {
            WIR_MemoryRegion &r = sec.getRegion();

            os << ", \"";

            if ( r.getAttributes() &
                 (unsigned long) WIR_MemoryRegionAttributes::allocated )
              os << "a";
            if ( r.getAttributes() &
                 (unsigned long) WIR_MemoryRegionAttributes::write )
              os << "w";
            if ( r.getAttributes() &
                 (unsigned long) WIR_MemoryRegionAttributes::execute )
              os << "x";

            os << "\"";
          }

          os << endl;
          previousCodeSection = sec.getID();
        }
      }
    }
  }

  // Dump comments and file information attached to the basic block.
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
  for ( WIR_Instruction &i : b.getInstructions() )
    os << i;
};


/*
  dumpARMCompilationUnit dumps a %WIR compilation unit to an output stream in an
  ARM-specific fashion.
*/
void dumpARMCompilationUnit( std::ostream &os, const WIR_CompilationUnit &c )
{
  DSTART( "void dumpARMCompilationUnit(ostream&, const WIR_CompilationUnit&)" );

  ARM_Base p;
  string processorName { p.getProcessorName() };
  string isaName { p.getISAName() };

  if ( c.isInserted() ) {
    auto processors = c.getSystem().getComponents<WIR_BaseProcessor>();

    if ( processors.size() == 1 ) {
      processorName = processors.begin()->get().getProcessorName();
      isaName = processors.begin()->get().getISAName();
    }
  }

  // Emit .file directive.
  os << string( 8, ' ' ) << ".file" << string( 11, ' ' ) << "\"" << c.getName()
     << "\"" << endl;
  os << string( 8, ' ' ) << "@ Generated using " << WIR_MODULE << " "
     << WIR_VERSION << endl;
  os << string( 8, ' ' ) << "@ for " << processorName << " ISA "
     << isaName << endl << endl;

  // Dump comments and file information attached to the compilation unit next.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : c.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : c.getContainers<WIR_FileInfo>() )
      os << cont << endl;

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
  bool dataObjectsDumped = false;
  for ( auto it = dataLayout.begin(); it != dataLayout.end(); ++it ) {
    WIR_Registry::getDataSectionDumper( os.iword( WIR_ProcessorIO() ) )(
      os, &(it->first.get()), it->second, it == dataLayout.begin() );
    if ( !(it->second.empty()) )
      dataObjectsDumped = true;
  }
  if ( dataObjectsDumped )
    os << endl << endl;

  // Output functions.
  bool firstFunctionDumped = false;
  for ( WIR_Function &f : c ) {
    // Skip empty functions.
    if ( f.getBasicBlocks().empty() )
      continue;

    if ( firstFunctionDumped )
      os << endl;
    os << f;
    firstFunctionDumped = true;
  }
};


/*
  dumpARMData dumps a WIR data object to an output stream in an ARM-specific
  fashion.
*/
void dumpARMData( std::ostream &os, const WIR_Data &d )
{
  DSTART( "void dumpARMData(ostream&, const WIR_Data&)" );

  if ( d.isInserted() && d.getCompilationUnit().isInserted() ) {
    WIR_System &sys = d.getCompilationUnit().getSystem();
    WIR_Symbol &sym = sys.findSymbol( d );
    const WIR_Section &sec = sym.getSection();

    // Emit .global directive.
    if ( sym.isGlobal() )
      os << string( 8, ' ' ) << ".global" << string( 9, ' ' ) << d.getName()
         << endl;

    if ( d.isInitialized() || ( sym.getSection().getName() != ".bss" ) ) {
      os << string( 8, ' ' ) << ".align" << string( 10, ' ' )
         << sec.getBlock() << endl;
      os << string( 8, ' ' ) << ".type" << string( 11, ' ' ) << d.getName()
         << ", %object" << endl;
      os << string( 8, ' ' ) << ".size" << string( 11, ' ' ) << d.getName()
         << ", " << d.getSize() << endl;
      os << d.getName() << ":" << endl;

      if ( !d.isInitialized() )
        os << string( 8, ' ' ) << ".space" << string( 10, ' ' ) << d.getSize()
           << endl;
      else {
        // Emit initialization data.
        for ( WIR_DataInit &i : d.getInitData() ) {
          switch ( i.getType() ) {

            case WIR_DataInitType::iascii: {
              os << string( 8, ' ' ) << ".ascii \""
                 << WIR_DataInit::escapeString( i.getValues().front() )
                 << "\"" << endl;
              break;
            };

            case WIR_DataInitType::iasciz: {
              os << string( 8, ' ' ) << ".asciz \""
                 << WIR_DataInit::escapeString( i.getValues().front() )
                 << "\"" << endl;
              break;
            };

            case WIR_DataInitType::ibyte: {
              os << string( 8, ' ' ) << ".byte " << i.getValues().front()
                 << endl;
              break;
            };

            case WIR_DataInitType::idouble:
            case WIR_DataInitType::idword: {
              os << string( 8, ' ' ) << ".word " << i.getValues().front()
                 << endl;

              ufWarnMsg << ufFile( __FILE__, __LINE__ ) << "Attention!"
                        << "Originally, one single 64-bit initializer was used "
                        << "(outas_arm.cc) which was translated into two "
                        << ".word directives with 32 bits each when dumping a "
                        << "data object. However, this involved using ICD-int. "
                        << "In order to keep WIR free of ICD-int, this "
                        << "modeling of 64-bit initializers was given up in "
                        << "WIR: An original 64-bit initializer must thus be "
                        << "broken down to two 32-bit initializers when "
                        << "creating the initialization data of a WIR_Data "
                        << "object. This requirement has not yet been checked, "
                        << "thus this warning!" << endl;
              break;
            };

            case WIR_DataInitType::ifloat: {
              os << string( 8, ' ' ) << ".word " << i.getValues().front()
                 << endl;
              break;
            };

            case WIR_DataInitType::ihword: {
              os << string( 8, ' ' ) << ".hword " << i.getValues().front()
                 << endl;
              break;
            };

            case WIR_DataInitType::ishort: {
              os << string( 8, ' ' ) << ".short " << i.getValues().front()
                 << endl;
              break;
            };

            case WIR_DataInitType::ispace: {
              os << string( 8, ' ' ) << ".space " << i.getSpace() << endl;
              break;
            };

            case WIR_DataInitType::isymbol:
            case WIR_DataInitType::iword: {
              os << string( 8, ' ' ) << ".word " << i.getValues().front()
                 << endl;
              break;
            };

          }
        }
      }
    } else {
      // Data objects from the .bss section can be initialized with .comm
      // directives which saves space in the binary executable.
      os << string( 8, ' ' ) << ".comm" << string( 11, ' ' ) << d.getName()
         << ", " << d.getSize() << ", " << sec.getBlock() << endl;
    }
  }
};


/*
  dumpARMDataSection dumps a data section to an output stream in an ARM-specific
  fashion.
*/
void dumpARMDataSection( std::ostream &os, WIR_Section *sec,
                         const std::list<std::reference_wrapper<WIR_Data>> &l,
                         bool firstSec )
{
  DSTART(
    "void dumpARMDataSection(ostream&, WIR_Section*, const list<reference_wrapper<WIR_Data> >&, bool)" );

  WIR_MemoryRegion &r = sec->getRegion();
  WIR_System &sys = r.getSystem();

  // Determine whether there are data objects to be dumped.
  bool dumpData = false;
  for ( WIR_Data &d : l ) {
    WIR_Symbol &sym = sys.findSymbol( d );
    if ( !sym.isExtern() ) {
      dumpData = true;
      break;
    }
  }

  if ( !dumpData )
    return;

  // Dump section/region directives first.
  if ( !firstSec )
    os << endl << endl;
  os << string( 8, ' ' ) << ".section" << string( 8, ' ' ) << sec->getName();
  if ( ( sec->getName() != ".bss" ) && ( sec->getName() != ".data" ) &&
       ( sec->getName() != ".rodata" ) ) {
    os << ", \"";

    // Dump region attributes.
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::allocated )
      os << "a";
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::write )
      os << "w";
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::execute )
      os << "x";

    os << "\"";
  }
  os << endl;

  // Dump data objects next.
  bool firstData = true;
  for ( WIR_Data &d : l ) {
    // Skip the current data object if it's extern.
    WIR_Symbol &sym = sys.findSymbol( d );
    if ( sym.isExtern() )
      continue;

    if ( firstData )
      firstData = false;
    else
      os << endl;
    os << d;
  }
};


/*
  dumpARMFunction dumps a %WIR function to an output stream in an ARM-specific
  fashion.
*/
void dumpARMFunction( std::ostream &os, const WIR_Function &f )
{
  DSTART( "void dumpARMFunction(ostream&, const WIR_Function&)" );

  WIR_Symbol *symPtr = nullptr;

  // Check whether section/region directives have to be emitted.
  if ( f.isInserted() ) {
    WIR_CompilationUnit &c = f.getCompilationUnit();
    if ( c.isInserted() ) {
      WIR_System &sys = c.getSystem();
      WIR_Symbol &sym = sys.findSymbol( f );
      symPtr = &sym;
      auto &sec = sym.getSection();

      if ( ( f == c.getFunctions().front().get() ) &&
           ( c == sys.getCompilationUnits().front().get() ) )
        // The current function f is the very first one of a complete WIR
        // system. There thus is no valid previous section.
        previousCodeSection = nullid;

      // Emit .section directive if necessary.
      if ( sec.getID() != previousCodeSection ) {
        os << string( 8, ' ' ) << ".section" << string( 8, ' ' )
           << sec.getName();

        if ( sec.getName() != ".text" ) {
          WIR_MemoryRegion &r = sec.getRegion();

          os << ", \"";

          if ( r.getAttributes() &
               (unsigned long) WIR_MemoryRegionAttributes::allocated )
            os << "a";
          if ( r.getAttributes() &
               (unsigned long) WIR_MemoryRegionAttributes::write )
            os << "w";
          if ( r.getAttributes() &
               (unsigned long) WIR_MemoryRegionAttributes::execute )
            os << "x";

          os << "\"";
        }

        os << endl;
        previousCodeSection = sec.getID();
      }

      // Emit .align directive. This directive must appear after the .section
      // directive. Otherwise, .align will have no effect, since the section
      // change will invalidate the location pointer.
      os << string( 8, ' ' ) << ".align" << string( 10, ' ' ) << sec.getBlock()
         << endl;
    }
  }

  // Dump comments and file information attached to the function first.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : f.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : f.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  // Emit .type directive.
  os << string( 8, ' ' ) << ".type" << string( 11, ' ' ) << f.getName()
     << ", %function" << endl;

  // Emit .global directive.
  if ( ( symPtr != nullptr) && symPtr->isGlobal() )
    os << string( 8, ' ' ) << ".global" << string( 9, ' ' ) << f.getName()
       << endl;

  // Output basic blocks.
  for ( WIR_BasicBlock &b : f.getBasicBlocks() )
    os << b;

  // Emit .size directive.
  os << string( 8, ' ' ) << ".size" << string( 11, ' ' ) << f.getName()
     << ", .-" << f.getName() << endl;
};


/*
  dumpARMLdScript dumps a WIR system's memory layout as linker script to an
  output stream in an ARM- and GNU-ld specific fashion.
*/
void dumpARMLdScript( std::ostream &os, const WIR_System &sys )
{
  DSTART( "void dumpARMLdScript(ostream&, const WIR_System&)" );

  auto &p = sys.getComponents<WIR_BaseProcessor>().begin()->get();

  os << "/*" << endl
     << "   Linker script for " << sys.getSystemName() << " ISA "
     << p.getISAName() << " executables." << endl
     << "   Generated using " << WIR_MODULE << " " << WIR_VERSION << endl
     << "*/" << endl << endl;

  // Write architecture-related headers.
  os << "ENTRY(_mainCRTStartup)" << endl << endl;

  // Write list of available memory regions.
  dumpLDScriptRegions( os, sys );

  // Write some global definitions.
  os << "/* Begin of ARM definitions */" << endl << hex;
  os << "/* End of ARM definitions */" << endl << endl;

  // Write section mapping.
  dumpLDScriptSections( os, sys );
};


/*
  dumpARMLdScriptSection dumps a WIR section as linker script entry to an output
  stream in an ARM- and GNU-ld specific fashion.
*/
bool dumpARMLdScriptSection( std::ostream &os, const WIR_Section &sec )
{
  DSTART( "bool dumpARMLdScriptSection(ostream&, const WIR_Section&)" );

  // Write dummy section for stack handling.
  if ( sec.getName() == ".stack" ) {
    size_t stackSize = sec.getRegion().getLength();

    os << "    /*" << endl << dec
       << "       Define a section to store pointer to stack." << endl
       << "       We do not allocate any memory objects here!" << endl
       << "    */" << endl
       << "    /*" << endl
       << "       Define the size of the heap although we do not use "
       << "dynamic " << endl << "       memory allocation." << endl
       << "    */" << endl
       << "    __heap_start_ = .;" << endl
       << "    __heap_end_ = . + " << ( stackSize / 2 ) << ";" << endl
       << "    /*" << endl
       << "       Define the sizes of the user and system stacks." << endl
       << "    */" << endl
       << "    __stack_start_ = . + " << stackSize << ";" << endl;
    return( true );
  }

  return( false );
};


/*
  dumpARMOperation dumps a %WIR operation to an output stream in an ARM-specific
  fashion.

  While dumping an operation's parameters, this function omits any implicit
  parameters and only outputs explicit ones in order to produce valid ARM
  assembly output that is accepted by a subsequent assembler.
*/
void dumpARMOperation( std::ostream &os, const WIR_Operation &o )
{
  DSTART( "void dumpARMOperation(ostream&, const WIR_Operation&)" );

  WIR_BaseProcessor::OperationFormat f = o.getOperationFormat();
  WIR_BaseProcessor::OpCode oc = o.getOpCode();

  // Each operation starts with an assembly directive clearly encoding its bit
  // size.
  os << dec << string( 8, ' ' )
     << string(
          ( ( o.getBitWidth() == 16 ) ||
            ( f == ARMv4T::OperationFormat::TL_2 ) ) ? ".thumb" : ".arm" )
     << endl;

  // Dump comments and file information attached to the operation next.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : o.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : o.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  // Construct the operation's mnemonic.
  stringstream mnemonic;
  bool irregularlyStructuredOpcode = false;

  if ( ( oc == ARM_Base::OpCode::LDRB ) ||
       ( oc == ARM_Base::OpCode::LDRBT ) ||
       ( oc == ARM_Base::OpCode::LDRH ) ||
       ( oc == ARM_Base::OpCode::LDRSB ) ||
       ( oc == ARM_Base::OpCode::LDRSH ) ||
       ( oc == ARM_Base::OpCode::LDRT ) ||
       ( oc == ARM_Base::OpCode::STRB ) ||
       ( oc == ARM_Base::OpCode::STRBT ) ||
       ( oc == ARM_Base::OpCode::STRH ) ||
       ( oc == ARM_Base::OpCode::STRT ) ||
       ( oc == ARM_Base::OpCode::SWPB ) ||
       ( oc == ARMv5TE::OpCode::LDRD ) ||
       ( oc == ARMv5TE::OpCode::STRD ) ) {
    // Special handling of crazily structured LDRB/LDRH/... opcodes...
    mnemonic << oc.getName().substr( 0, 3 );
    irregularlyStructuredOpcode = true;
  } else
    mnemonic << oc.getName();

  if ( o.getParameters().front().get().getType() ==
         WIR_ParameterType::cond ) {
    WIR_ConditionFieldParameter &c =
      dynamic_cast<WIR_ConditionFieldParameter &>(
        o.getParameters().front().get() );

    if ( c.getCondition() != ARM_Base::Condition::al )
      mnemonic << c;
  }

  if ( irregularlyStructuredOpcode )
    mnemonic << oc.getName().substr( 3, string::npos );
  else

  if ( ( oc == ARM_Base::OpCode::LDM ) || ( oc == ARM_Base::OpCode::STM ) ) {
    // Special handling of LDM/STM opcodes.
    WIR_AddressingModeParameter &m =
      dynamic_cast<WIR_AddressingModeParameter &>(
        (++(o.getParameters().begin()))->get() );

    mnemonic << m;
  } else

  if ( ( oc == ARMv6::OpCode::RFE ) || ( oc == ARMv6::OpCode::SRS ) ) {
    // Special handling of RFE/SRS opcodes.
    WIR_AddressingModeParameter &m =
      dynamic_cast<WIR_AddressingModeParameter &>(
        (o.getParameters().begin())->get() );

    mnemonic << m;
  } else

  if ( ( f == ARM_Base::OperationFormat::CASRAC8_2 ) ||
       ( f == ARM_Base::OperationFormat::CASARAC8_2 ) ||
       ( f == ARM_Base::OperationFormat::CASRC8_2 ) ||
       ( f == ARMv5T::OperationFormat::ASRAC8_2 ) ||
       ( f == ARMv5T::OperationFormat::ASARAC8_2 ) ||
       ( f == ARMv5T::OperationFormat::ASRC8_2 ) )
    mnemonic << "l";

  if ( ( f == ARM_Base::OperationFormat::CRRC8RA_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRR_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRR_4 ) ||
       ( f == ARM_Base::OperationFormat::CRRRAR_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRRC5_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRRAC60_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRRC50_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRRR_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRRR_4 ) ||
       ( f == ARM_Base::OperationFormat::CRC8RA_2 ) ||
       ( f == ARM_Base::OperationFormat::CRR_2 ) ||
       ( f == ARM_Base::OperationFormat::CRR_4 ) ||
       ( f == ARM_Base::OperationFormat::CRRAR_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRC5_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRAC60_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRC50_2 ) )
    // Dump S-Bit.
    mnemonic << "s";

  os << string( 8, ' ' ) << mnemonic.str();

  // Output parameters.
  auto params = o.getParameters();
  bool firstParam = true;
  bool omitComma = false;
  bool postIndex = false;
  bool emitExcl = false;
  bool cEmitted = false;
  bool xEmitted = false;
  bool sEmitted = false;
  bool fEmitted = false;
  unsigned int expParamCount = 0;

  for ( auto it = params.begin(); it != params.end(); ++it ) {
    auto &p = (*it).get();
    bool emitParam = p.isExplicit();

    // Skip condition or addressing mode of LDM/STM, since they're already part
    // of the mnemonic.
    if ( ( firstParam && ( p.getType() == WIR_ParameterType::cond ) ) ||
         ( ( ( oc == ARM_Base::OpCode::LDM ) ||
             ( oc == ARM_Base::OpCode::STM ) ||
             ( oc == ARMv6::OpCode::RFE ) || ( oc == ARMv6::OpCode::SRS ) ) &&
           ( p.getType() == WIR_ParameterType::addr ) ) )
      emitParam = false;

    if ( emitParam ) {
      ++expParamCount;

      bool addressOffset =
        ( ( expParamCount > 1 ) &&
          ( ( f == ARM_Base::OperationFormat::CASRAC8_1 ) ||
            ( f == ARM_Base::OperationFormat::CASRAC8_2 ) ||
            ( f == ARM_Base::OperationFormat::CASARAC8_1 ) ||
            ( f == ARM_Base::OperationFormat::CASARAC8_2 ) ||
            ( f == ARMv5T::OperationFormat::ASRAC8_1 ) ||
            ( f == ARMv5T::OperationFormat::ASRAC8_2 ) ||
            ( f == ARMv5T::OperationFormat::ASARAC8_1 ) ||
            ( f == ARMv5T::OperationFormat::ASARAC8_2 ) ) ) ||
        ( f == ARM_Base::OperationFormat::CRRAC8_1 ) ||
        ( f == ARM_Base::OperationFormat::CRRAC12_1 ) ||
        ( f == ARM_Base::OperationFormat::CRARAC8_1 ) ||
        ( f == ARM_Base::OperationFormat::CRARAC12_1 ) ||
        ( f == ARM_Base::OperationFormat::CRRAC8_2 ) ||
        ( f == ARM_Base::OperationFormat::CRRAC12_2 ) ||
        ( f == ARM_Base::OperationFormat::CRARAC8_2 ) ||
        ( f == ARM_Base::OperationFormat::CRARAC12_2 ) ||
        ( f == ARMv5TE::OperationFormat::CPRAC8_1 ) ||
        ( f == ARMv5TE::OperationFormat::CPRAC8_2 ) ||
        ( f == ARMv5TE::OperationFormat::CPARAC8_1 ) ||
        ( f == ARMv5TE::OperationFormat::CPARAC8_2 ) ||
        ( f == ARMv5TE::OperationFormat::RAC12_1 ) ;

      // Output separator between parameters.
      if ( firstParam ) {
        os << string( 16 - mnemonic.str().size(), ' ' );
        firstParam = false;

        if ( ( f == ARM_Base::OperationFormat::CAAAAC8RA_1 ) ||
             ( f == ARM_Base::OperationFormat::CAAAAR_1 ) )
          os << "CPSR_";
        if ( ( f == ARM_Base::OperationFormat::CAAAAC8RA_2 ) ||
             ( f == ARM_Base::OperationFormat::CAAAAR_2 ) )
          os << "SPSR_";
        if ( f == ARMv4T::OperationFormat::TC9_1 )
          os << "sp, ";
      } else

      if ( omitComma )
        omitComma = false;
      else
        os << ", ";

      if ( ( ( expParamCount == 2 ) &&
             ( ( f == ARM_Base::OperationFormat::CRRAC8_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRAC12_1 ) ||
               ( f == ARM_Base::OperationFormat::CRARAC8_1 ) ||
               ( f == ARM_Base::OperationFormat::CRARAC12_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRAR_3 ) ||
               ( f == ARM_Base::OperationFormat::CRARAR_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC5_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRARAC60_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC50_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRAR_4 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC5_1 ) ||
               ( f == ARM_Base::OperationFormat::CRARARAC60_1 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC50_1 ) ||
               ( f == ARM_Base::OperationFormat::CRARAR_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRAC8_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRAC12_2 ) ||
               ( f == ARM_Base::OperationFormat::CRARAC8_2 ) ||
               ( f == ARM_Base::OperationFormat::CRARAC12_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRAR_6 ) ||
               ( f == ARM_Base::OperationFormat::CRARAR_3 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC5_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRARAC60_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC50_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRAR_7 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC5_2 ) ||
               ( f == ARM_Base::OperationFormat::CRARARAC60_2 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC50_2 ) ||
               ( f == ARM_Base::OperationFormat::CRARAR_4 ) ||
               ( f == ARMv4T::OperationFormat::TRRC5_2 ) ||
               ( f == ARMv4T::OperationFormat::TRRC5_3 ) ||
               ( f == ARMv4T::OperationFormat::TRRC6_2 ) ||
               ( f == ARMv4T::OperationFormat::TRRC6_3 ) ||
               ( f == ARMv4T::OperationFormat::TRRC7_1 ) ||
               ( f == ARMv4T::OperationFormat::TRRC7_2 ) ||
               ( f == ARMv4T::OperationFormat::TRRR_2 ) ||
               ( f == ARMv4T::OperationFormat::TRRR_3 ) ||
               ( f == ARMv4T::OperationFormat::TRPCC10_2 ) ||
               ( f == ARMv4T::OperationFormat::TRSPC10_2 ) ||
               ( f == ARMv4T::OperationFormat::TRSPC10_3 ) ||
               ( f == ARMv5TE::OperationFormat::CPRAC8_1 ) ||
               ( f == ARMv5TE::OperationFormat::CPRAC8_2 ) ||
               ( f == ARMv5TE::OperationFormat::CPARAC8_1 ) ||
               ( f == ARMv5TE::OperationFormat::CPARAC8_2 ) ||
               ( f == ARMv5TE::OperationFormat::CPRAR_1 ) ||
               ( f == ARMv5TE::OperationFormat::CPRAR_2 ) ||
               ( f == ARMv5TE::OperationFormat::CPARAR_1 ) ||
               ( f == ARMv5TE::OperationFormat::CPARAR_2 ) ||
               ( f == ARMv6::OperationFormat::CRR_7 ) ) ) ||
           ( ( expParamCount == 3 ) &&
             ( ( f == ARM_Base::OperationFormat::CASRAC8_1 ) ||
               ( f == ARM_Base::OperationFormat::CASRAC8_2 ) ||
               ( f == ARM_Base::OperationFormat::CASARAC8_1 ) ||
               ( f == ARM_Base::OperationFormat::CASARAC8_2 ) ||
               ( f == ARM_Base::OperationFormat::CASRC8_1 ) ||
               ( f == ARM_Base::OperationFormat::CASRC8_2 ) ||
               ( f == ARMv5T::OperationFormat::ASRAC8_1 ) ||
               ( f == ARMv5T::OperationFormat::ASRAC8_2 ) ||
               ( f == ARMv5T::OperationFormat::ASARAC8_1 ) ||
               ( f == ARMv5T::OperationFormat::ASARAC8_2 ) ||
               ( f == ARMv5T::OperationFormat::ASRC8_1 ) ||
               ( f == ARMv5T::OperationFormat::ASRC8_2 ) ||
               ( f == ARMv6::OperationFormat::CRRR_5 ) ) ) ||
           ( ( expParamCount == 1 ) &&
             ( ( f == ARMv5TE::OperationFormat::RAC12_1 ) ||
               ( f == ARMv5TE::OperationFormat::RAR_1 ) ||
               ( f == ARMv5TE::OperationFormat::RAR_2 ) ||
               ( f == ARMv5TE::OperationFormat::RARC5_1 ) ||
               ( f == ARMv5TE::OperationFormat::RARAC60_1 ) ||
               ( f == ARMv5TE::OperationFormat::RARC50_1 ) ) ) )
        os << "[";

      // Output current parameter itself.
      switch ( p.getType() ) {

        case WIR_ParameterType::addr: {
          auto &addP = dynamic_cast<WIR_AddressingModeParameter &>( p );

          bool coprocessor =
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p0 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p1 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p2 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p3 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p4 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p5 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p6 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p7 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p8 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p9 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p10 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p11 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p12 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p13 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p14 ) ||
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::p15 );

          // After an addressing mode, the comma must be omitted (except MSR and
          // coprocessor operations).
          omitComma = true;
          if ( ( ( expParamCount == 4 ) && ( oc == ARM_Base::OpCode::MSR ) ) ||
               coprocessor ||
               ( o.getOperationFormat() == ARMv6::OperationFormat::AC5_1 ) )
            omitComma = false;

          // Check special cxsf flags of MSR operation.
          if ( addP.getAddressingMode() == ARM_Base::AddressingMode::c ) {
            if ( !cEmitted )
              cEmitted = true;
            else
              break;
          }
          if ( addP.getAddressingMode() == ARM_Base::AddressingMode::x ) {
            if ( !xEmitted )
              xEmitted = true;
            else
              break;
          }
          if ( addP.getAddressingMode() == ARM_Base::AddressingMode::s ) {
            if ( !sEmitted )
              sEmitted = true;
            else
              break;
          }
          if ( addP.getAddressingMode() == ARM_Base::AddressingMode::f ) {
            if ( !fEmitted )
              fEmitted = true;
            else
              break;
          }

          // Check whether this is post-indexed addressing.
          postIndex =
            ( addP.getAddressingMode() == ARM_Base::AddressingMode::post );

          if ( !postIndex &&
               ( addP.getAddressingMode() != ARM_Base::AddressingMode::pre ) ) {
            if ( addressOffset )
              os << "#" << p;
            else

            if ( ( ( expParamCount == 2 ) &&
                   ( ( f == ARMv5TE::OperationFormat::RAR_1 ) ||
                     ( f == ARMv5TE::OperationFormat::RAR_2 ) ||
                     ( f == ARMv5TE::OperationFormat::RARC5_1 ) ||
                     ( f == ARMv5TE::OperationFormat::RARAC60_1 ) ||
                     ( f == ARMv5TE::OperationFormat::RARC50_1 ) ) ) ||
                 ( ( expParamCount == 3 ) &&
                   ( ( f == ARM_Base::OperationFormat::CRRARAC60_1 ) ||
                     ( f == ARM_Base::OperationFormat::CRRARAC60_2 ) ) ) ||
                 ( ( expParamCount == 4 ) &&
                   ( ( f == ARM_Base::OperationFormat::CRARARAC60_1 ) ||
                     ( f == ARM_Base::OperationFormat::CRARARAC60_2 ) ) ) ||
                 ( f == ARM_Base::OperationFormat::CRRAR_3 ) ||
                 ( f == ARM_Base::OperationFormat::CRARAR_1 ) ||
                 ( f == ARM_Base::OperationFormat::CRRARC5_1 ) ||
                 ( f == ARMv5TE::OperationFormat::CPRAR_1 ) ||
                 ( f == ARMv5TE::OperationFormat::CPRAR_2 ) ||
                 ( f == ARMv5TE::OperationFormat::CPARAR_1 ) ||
                 ( f == ARMv5TE::OperationFormat::CPARAR_2 ) ||
                 ( f == ARM_Base::OperationFormat::CRRARC50_1 ) ||
                 ( f == ARM_Base::OperationFormat::CRRAR_4 ) ||
                 ( f == ARM_Base::OperationFormat::CRARARC5_1 ) ||
                 ( f == ARM_Base::OperationFormat::CRARARC50_1 ) ||
                 ( f == ARM_Base::OperationFormat::CRARAR_2 ) ||
                 ( f == ARM_Base::OperationFormat::CRRAR_6 ) ||
                 ( f == ARM_Base::OperationFormat::CRARAR_3 ) ||
                 ( f == ARM_Base::OperationFormat::CRRARC5_2 ) ||
                 ( f == ARM_Base::OperationFormat::CRRARC50_2 ) ||
                 ( f == ARM_Base::OperationFormat::CRRAR_7 ) ||
                 ( f == ARM_Base::OperationFormat::CRARARC5_2 ) ||
                 ( f == ARM_Base::OperationFormat::CRARARC50_2 ) ||
                 ( f == ARM_Base::OperationFormat::CRARAR_4 ) ||
                 ( oc == ARM_Base::OpCode::MSR ) || coprocessor ||
                 ( oc == ARMv6::OpCode::CPSID ) ||
                 ( oc == ARMv6::OpCode::CPSIE ) )
              os << p;
            else
              os << p << " ";
          }

          if ( addP.getAddressingMode() == ARM_Base::AddressingMode::pre )
            emitExcl = true;

          break;
        }

        case WIR_ParameterType::imm: {
          if ( ( expParamCount != 5 ) &&
               ( ( f == ARM_Base::OperationFormat::CAAAAC8RA_1 ) ||
                 ( f == ARM_Base::OperationFormat::CAAAAC8RA_2 ) ) )
            break;

          if ( ( f == ARM_Base::OperationFormat::CRRRC5_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRC5_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRC5_3 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC5_1 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC5_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC5_2 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC5_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRRC5_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRC5_2 ) ||
               ( f == ARMv5TE::OperationFormat::RARC5_1 ) ||
               ( ( expParamCount == 4 ) &&
                 ( ( f == ARMv6::OperationFormat::CRC5RC5_1 ) ||
                   ( f == ARMv6::OperationFormat::CRC60RC5_1 ) ) ) )
            os << "lsl ";
          else

          if ( ( f == ARM_Base::OperationFormat::CRRRC50_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRC50_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRC50_3 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC50_1 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC50_1 ) ||
               ( f == ARM_Base::OperationFormat::CRRARC50_2 ) ||
               ( f == ARM_Base::OperationFormat::CRARARC50_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRRC50_2 ) ||
               ( f == ARM_Base::OperationFormat::CRRC50_2 ) ||
               ( f == ARMv5TE::OperationFormat::RARC50_1 ) )
            os << "ror ";
          else

          if ( ( f == ARMv6::OperationFormat::CRRRC5_3 ) ||
               ( ( expParamCount == 4 ) &&
                 ( ( f == ARMv6::OperationFormat::CRC5RC60_1 ) ||
                   ( f == ARMv6::OperationFormat::CRC60RC60_1 ) ) ) )
            os << "asr ";
          else

          if ( ( f == ARM_Base::OperationFormat::CASRC8_1 ) ||
               ( f == ARM_Base::OperationFormat::CASRC8_2 ) ||
               ( f == ARMv5T::OperationFormat::ASRC8_1 ) ||
               ( f == ARMv5T::OperationFormat::ASRC8_2 ) )
            os << "{ ";

          // Any immediate operands except rotate amounts and address offsets
          // are prefixed with a '#'.
          if ( !( ( ( expParamCount == 4 ) &&
                    ( ( f == ARM_Base::OperationFormat::CRRC8RA_1 ) ||
                      ( f == ARM_Base::OperationFormat::CRRC8RA_2 ) ) ) ||
                  ( ( expParamCount == 3 ) &&
                    ( ( f == ARM_Base::OperationFormat::CRC8RA_1 ) ||
                      ( f == ARM_Base::OperationFormat::CRC8RA_2 ) ||
                      ( f == ARM_Base::OperationFormat::CRC8RA_3 ) ) ) ) &&
               !addressOffset )
            os << "#";

          // Do special handling of immediates in MSR operations to make ugly
          // arm-elf-as happy.
          if ( ( f == ARM_Base::OperationFormat::CAAAAC8RA_1 ) ||
               ( f == ARM_Base::OperationFormat::CAAAAC8RA_2 ) ) {
            if ( expParamCount == 5 ) {
              auto &cP = dynamic_cast<ARM_Const8_Unsigned &>( p );
              auto &rP =
                dynamic_cast<ARM_Const5_RotateAmount &>(
                  o.getParameters().rbegin()->get() );

              // Compute a constant value from the 8-bit immediate and the
              // rotate amount from the next parameter.
              unsigned int c = cP.getValue();
              unsigned int r = rP.getValue();

              // Rotate c right by r bit positions.
              for ( unsigned int i = 0; i < r; ++i )
                if ( c % 2 == 0 )
                  // Least-significant bit of c is 0. Enforce most-significant
                  // bit of 0 after right-shift.
                  c = ( c >> 1 ) & 0x7FFFFFFF;
                else
                  // Least-significant bit of c is 1. Enforce most-significant
                  // bit of 1 after right-shift.
                  c = ( ( c >> 1 ) & 0xFFFFFFFF ) | 0x80000000;

              os << c;
              omitComma = true;
            }

            break;
          }

          os << p;

          if ( ( f == ARM_Base::OperationFormat::CASRC8_1 ) ||
               ( f == ARM_Base::OperationFormat::CASRC8_2 ) ||
               ( f == ARMv5T::OperationFormat::ASRC8_1 ) ||
               ( f == ARMv5T::OperationFormat::ASRC8_2 ) )
            os << " } ";

          break;
        }

        case WIR_ParameterType::reg: {
          if ( ( ( expParamCount == 1 ) &&
                 ( ( oc == ARMv4T::OpCode::POP ) ||
                   ( oc == ARMv4T::OpCode::PUSH ) ) ) ||
               ( ( expParamCount == 2 ) &&
                 ( ( oc == ARM_Base::OpCode::LDM ) ||
                   ( oc == ARMv4T::OpCode::LDMIA ) ||
                   ( oc == ARM_Base::OpCode::STM ) ||
                   ( oc == ARMv4T::OpCode::STMIA ) ) ) )
            // Output opening curly braces for the register lists required by
            // LDM, LDMIA, POP and STM.
            os << "{ ";

          // Output register parameter, eventually followed by ']'  if a
          // post-indexed addressing mode is used here.
          os << p;

          if ( postIndex ||
               ( ( expParamCount == 3 ) &&
                 ( ( f == ARM_Base::OperationFormat::CASRC8_1 ) ||
                   ( f == ARM_Base::OperationFormat::CASRC8_2 ) ||
                   ( f == ARMv5T::OperationFormat::ASRC8_1 ) ||
                   ( f == ARMv5T::OperationFormat::ASRC8_2 ) ||
                   ( f == ARMv6::OperationFormat::CRRR_5 ) ) ) )
            os << "]";
          postIndex = false;

          // Output an exclamation mark '!' for LDM/STM with set W bit.
          if ( ( expParamCount == 1 ) &&
               ( ( f == ARM_Base::OperationFormat::CARR1_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR2_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR3_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR4_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR5_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR6_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR7_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR8_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR9_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR10_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR11_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR12_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR13_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR14_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR15_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR16_2 ) ||
                 ( f == ARM_Base::OperationFormat::CARR1_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR2_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR3_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR4_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR5_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR6_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR7_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR8_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR9_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR10_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR11_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR12_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR13_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR14_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR15_7 ) ||
                 ( f == ARM_Base::OperationFormat::CARR16_7 ) ||
                 ( f == ARMv4T::OperationFormat::TRR1_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR1_2 ) ||
                 ( f == ARMv4T::OperationFormat::TRR2_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR2_2 ) ||
                 ( f == ARMv4T::OperationFormat::TRR3_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR3_2 ) ||
                 ( f == ARMv4T::OperationFormat::TRR4_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR4_2 ) ||
                 ( f == ARMv4T::OperationFormat::TRR5_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR5_2 ) ||
                 ( f == ARMv4T::OperationFormat::TRR6_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR6_2 ) ||
                 ( f == ARMv4T::OperationFormat::TRR7_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR7_2 ) ||
                 ( f == ARMv4T::OperationFormat::TRR8_1 ) ||
                 ( f == ARMv4T::OperationFormat::TRR8_2 ) ||
                 ( f == ARMv6::OperationFormat::AR_2 ) ) )
            os << "!";

          break;
        }

        default: {
          // For other parameters, no special handling is required.
          os << p;

          break;
        }
      }
    }
  }

  if ( ( f == ARM_Base::OperationFormat::CRRR_3 ) ||
       ( f == ARM_Base::OperationFormat::CRR_3 ) ||
       ( f == ARM_Base::OperationFormat::CRR_6 ) ||
       ( f == ARM_Base::OperationFormat::CRRAR_4 ) ||
       ( f == ARM_Base::OperationFormat::CRARAR_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRAR_7 ) ||
       ( f == ARM_Base::OperationFormat::CRARAR_4 ) ||
       ( f == ARM_Base::OperationFormat::CRRR_4 ) ||
       ( f == ARM_Base::OperationFormat::CRR_4 ) ||
       ( f == ARMv5TE::OperationFormat::RAR_2 ) )
    os << ", rrx";

  if ( ( f == ARM_Base::OperationFormat::CASRAC8_1 ) ||
       ( f == ARM_Base::OperationFormat::CASRAC8_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRAC8_1 ) ||
       ( f == ARM_Base::OperationFormat::CRRAC12_1 ) ||
       ( f == ARM_Base::OperationFormat::CRRAR_3 ) ||
       ( f == ARM_Base::OperationFormat::CRRARC5_1 ) ||
       ( f == ARM_Base::OperationFormat::CRRARAC60_1 ) ||
       ( f == ARM_Base::OperationFormat::CRRARC50_1 ) ||
       ( f == ARM_Base::OperationFormat::CRRAR_4 ) ||
       ( f == ARM_Base::OperationFormat::CRRAC8_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRAC12_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRAR_6 ) ||
       ( f == ARM_Base::OperationFormat::CRRARC5_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRARAC60_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRARC50_2 ) ||
       ( f == ARM_Base::OperationFormat::CRRAR_7 ) ||
       ( f == ARMv4T::OperationFormat::TRRC5_2 ) ||
       ( f == ARMv4T::OperationFormat::TRRC5_3 ) ||
       ( f == ARMv4T::OperationFormat::TRRC6_2 ) ||
       ( f == ARMv4T::OperationFormat::TRRC6_3 ) ||
       ( f == ARMv4T::OperationFormat::TRRC7_1 ) ||
       ( f == ARMv4T::OperationFormat::TRRC7_2 ) ||
       ( f == ARMv4T::OperationFormat::TRRR_2 ) ||
       ( f == ARMv4T::OperationFormat::TRRR_3 ) ||
       ( f == ARMv4T::OperationFormat::TRPCC10_2 ) ||
       ( f == ARMv4T::OperationFormat::TRSPC10_2 ) ||
       ( f == ARMv4T::OperationFormat::TRSPC10_3 ) ||
       ( f == ARMv5T::OperationFormat::ASRAC8_1 ) ||
       ( f == ARMv5T::OperationFormat::ASRAC8_2 ) ||
       ( f == ARMv5TE::OperationFormat::CPRAC8_1 ) ||
       ( f == ARMv5TE::OperationFormat::CPRAC8_2 ) ||
       ( f == ARMv5TE::OperationFormat::CPRAR_1 ) ||
       ( f == ARMv5TE::OperationFormat::CPRAR_2 ) ||
       ( f == ARMv5TE::OperationFormat::RAC12_1 ) ||
       ( f == ARMv5TE::OperationFormat::RAR_1 ) ||
       ( f == ARMv5TE::OperationFormat::RAR_2 ) ||
       ( f == ARMv5TE::OperationFormat::RARC5_1 ) ||
       ( f == ARMv5TE::OperationFormat::RARAC60_1 ) ||
       ( f == ARMv5TE::OperationFormat::RARC50_1 ) ||
       ( f == ARMv6::OperationFormat::CRR_7 ) )
    os << "]";

  if ( f == ARM_Base::OperationFormat::CR_1 )
    os << ", CPSR";
  if ( f == ARM_Base::OperationFormat::CR_2 )
    os << ", SPSR";
  if ( f == ARMv6::OperationFormat::AC5_3 )
    os << "!";

  if ( ( oc == ARM_Base::OpCode::LDM ) || ( oc == ARMv4T::OpCode::LDMIA ) ||
       ( oc == ARMv4T::OpCode::POP ) || ( oc == ARMv4T::OpCode::PUSH ) ||
       ( oc == ARM_Base::OpCode::STM ) || ( oc == ARMv4T::OpCode::STMIA ) )
    // Output closing curly braces for the register lists required by LDM,
    // LDMIA, POP, PUSH, STM and STMIA.
    os << " }";

  if ( ( f == ARM_Base::OperationFormat::CARR1_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR1_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR1_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR1_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR2_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR2_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR2_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR2_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR3_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR3_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR3_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR3_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR4_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR4_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR4_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR4_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR5_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR5_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR5_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR5_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR6_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR6_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR6_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR6_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR7_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR7_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR7_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR7_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR8_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR8_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR8_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR8_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR9_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR9_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR9_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR9_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR10_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR10_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR10_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR10_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR11_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR11_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR11_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR11_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR12_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR12_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR12_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR12_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR13_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR13_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR13_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR13_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR14_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR14_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR14_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR14_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR15_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR15_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR15_5 ) ||
       ( f == ARM_Base::OperationFormat::CARR15_8 ) ||
       ( f == ARM_Base::OperationFormat::CARR16_3 ) ||
       ( f == ARM_Base::OperationFormat::CARR16_4 ) ||
       ( f == ARM_Base::OperationFormat::CARR16_8 ) )
    // Emit trailing caret '^'.
    os << "^";

  if ( emitExcl )
    os << "]!";
};


/*
  dumpARMRegisterParameter dumps a WIR register parameter to an output stream in
  an ARM-specific fashion.
*/
void dumpARMRegisterParameter( std::ostream &os,
                               const WIR_RegisterParameter &p )
{
  DSTART(
    "void dumpARMRegisterParameter(ostream&, const WIR_RegisterParameter&)" );

  WIR_BaseRegister &r = p.getRegister();

  if ( r.getType() == ARMv5TE::RegisterType::pReg ) {
    // Register pairs are dumped by only emitting the name of its first child.
    if ( r.isPhysical() ) {
      WIR_PhysicalRegister &preg = dynamic_cast<WIR_PhysicalRegister &>( r );
      os << preg.getChilds().front().get().getName();
    } else {
      WIR_VirtualRegister &vreg = dynamic_cast<WIR_VirtualRegister &>( r );
      os << vreg.getChilds().front().get().getName();
    }
  } else
    // Output the parameter's register name.
    os << r.getName();
};


/*
  dumpARMComment dumps a %WIR comment container to an output stream in an
  ARM-specific fashion.
*/
void dumpARMComment( std::ostream &os, const WIR_Comment &c )
{
  DSTART( "void dumpARMComment(ostream&, const WIR_Comment&)" );

  os << string( 8, ' ' ) << "@ " << c.getText();
};


/*
  dumpARMFileInfo dumps a %WIR fileinfo container to an output stream in an
  ARM-specific fashion.
*/
void dumpARMFileInfo( std::ostream &os, const WIR_FileInfo &f )
{
  DSTART( "void dumpARMFileInfo(ostream&, const WIR_FileInfo&)" );

  os << string( 8, ' ' ) << "@ file \"" << f.getFileName() << "\", line " << dec
     << f.getLineNumber();
};

}       // namespace WIR
