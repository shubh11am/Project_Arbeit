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
  @file tcio.cc
  @brief This file implements TriCore-specific stream I/O routines for the %WIR
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

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>


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
  tricore is an I/O manipulator that provides a TriCore assembler dump of a WIR.
*/
std::ostream &tricore( std::ostream &os )
{
  DSTART( "ostream& tricore(ostream&)" );

  os.iword( WIR_ProcessorIO() ) = TC13::getProcessorTypeID();
  return( os );
};


/*
  dumpTCBasicBlock dumps a WIR basic block to an output stream in a TriCore-
  specific fashion.
*/
void dumpTCBasicBlock( std::ostream &os, const WIR_BasicBlock &b )
{
  DSTART( "void dumpTCBasicBlock(ostream&, const WIR_BasicBlock&)" );

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
            if ( !( r.getAttributes() &
                 (unsigned long) WIR_MemoryRegionAttributes::read ) )
              throw ufFatalError(
                string( "SHF_TRICORE_NOREAD (see TriCore EABI Manual v2.3, " ) +
                  "section 4.2.2) not supported by current versions of " +
                  "tricore-as based on GNU Binutils." );

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
  dumpTCCompilationUnit dumps a WIR compilation unit to an output stream in a
  TriCore-specific fashion.
*/
void dumpTCCompilationUnit( std::ostream &os, const WIR_CompilationUnit &c )
{
  DSTART( "void dumpTCCompilationUnit(ostream&, const WIR_CompilationUnit&)" );

  TC131 p;
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

  os << string( 8, ' ' ) << "# Generated using " << WIR_MODULE << " "
     << WIR_VERSION << endl;
  os << string( 8, ' ' ) << "# for " << processorName << " ISA "
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
  dumpTCData dumps a WIR data object to an output stream in a TriCore-specific
  fashion.
*/
void dumpTCData( std::ostream &os, const WIR_Data &d )
{
  DSTART( "void dumpTCData(ostream&, const WIR_Data&)" );

  if ( d.isInserted() && d.getCompilationUnit().isInserted() ) {
    WIR_System &sys = d.getCompilationUnit().getSystem();
    WIR_Symbol &sym = sys.findSymbol( d );
    const WIR_Section &sec = sym.getSection();

    // Emit .global directive.
    if ( sym.isGlobal() )
      os << string( 8, ' ' ) << ".global" << string( 9, ' ' ) << d.getName()
         << endl;

    if ( d.isInitialized() || ( sym.getSection().getName() != ".bss" ) ) {
      os << string( 8, ' ' ) << ".balign" << string( 9, ' ' )
         << ( 1 << sec.getBlock() ) << endl;
      os << string( 8, ' ' ) << ".type" << string( 11, ' ' ) << d.getName()
         << ", @object" << endl;
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
              os << string( 8, ' ' ) << ".ascii          \""
                 << WIR_DataInit::escapeString( i.getValues().front() )
                 << "\"" << endl;
              break;
            };

            case WIR_DataInitType::iasciz: {
              os << string( 8, ' ' ) << ".asciz          \""
                 << WIR_DataInit::escapeString( i.getValues().front() )
                 << "\"" << endl;
              break;
            };

            case WIR_DataInitType::ibyte: {
              os << string( 8, ' ' ) << ".byte" << string( 11, ' ' )
                 << i.getValues().front() << endl;
              break;
            };

            case WIR_DataInitType::idouble:
            case WIR_DataInitType::idword: {
              os << string( 8, ' ' ) << ".word" << string( 11, ' ' )
                 << i.getValues().front() << endl;
              os << string( 8, ' ' ) << ".word" << string( 11, ' ' )
                 << i.getValues().back() << endl;
              break;
            };

            case WIR_DataInitType::ifloat: {
              os << string( 8, ' ' ) << ".word" << string( 11, ' ' )
                 << i.getValues().front() << endl;
              break;
            };

            case WIR_DataInitType::ihword: {
              os << string( 8, ' ' ) << ".hword" << string( 10, ' ' )
                 << i.getValues().front() << endl;
              break;
            };

            case WIR_DataInitType::ishort: {
              os << string( 8, ' ' ) << ".short" << string( 10, ' ' )
                 << i.getValues().front() << endl;
              break;
            };

            case WIR_DataInitType::ispace: {
              os << string( 8, ' ' ) << ".space" << string( 10, ' ' )
                 << i.getSpace() << endl;
              break;
            };

            case WIR_DataInitType::isymbol:
            case WIR_DataInitType::iword: {
              os << string( 8, ' ' ) << ".word" << string( 11, ' ' )
                 << i.getValues().front() << endl;
              break;
            };

          }
        }
      }
    } else {
      // Data objects from the .bss section can be initialized with .comm
      // directives which saves space in the binary executable.
      os << string( 8, ' ' ) << ".comm" << string( 11, ' ' ) << d.getName()
         << ", " << d.getSize() << ", " << ( 1 << sec.getBlock() ) << endl;
    }
  }
};


/*
  dumpTCDataSection dumps a data section to an output stream in a TriCore-
  specific fashion.
*/
void dumpTCDataSection( std::ostream &os, WIR_Section *sec,
                        const std::list<std::reference_wrapper<WIR_Data>> &l,
                        bool firstSec )
{
  DSTART(
    "void dumpTCDataSection(ostream&, WIR_Section*, const list<reference_wrapper<WIR_Data> >&, bool)" );

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
  if ( sec->getName() != ".rodata" ) {
    os << ", \"";

    // Dump region attributes.
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::allocated )
      os << "a";
    if ( !( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::read ) )
      throw ufFatalError(
        string( "SHF_TRICORE_NOREAD (see TriCore EABI Manual v2.3, section " ) +
          "4.2.2) not supported by current versions of tricore-as based on " +
          "GNU Binutils." );

    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::write )
      os << "w";
    if ( r.getAttributes() &
         (unsigned long) WIR_MemoryRegionAttributes::execute )
      os << "x";

    os << "\"";
  } else
    // Hard-coded progbits subsection for .rodata.
    os << ", \"\", @progbits";
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
  dumpTCFunction dumps a WIR function to an output stream in a TriCore-specific
  fashion.
*/
void dumpTCFunction( std::ostream &os, const WIR_Function &f )
{
  DSTART( "void dumpTCFunction(ostream&, const WIR_Function&)" );

  WIR_Symbol *symPtr = nullptr;

  // Check whether section/region directives have to be emitted.
  if ( f.isInserted() ) {
    WIR_CompilationUnit &c = f.getCompilationUnit();
    if ( c.isInserted() ) {
      WIR_System &sys = c.getSystem();
      WIR_Symbol &sym = sys.findSymbol( f );
      symPtr = &sym;
      auto &sec = sym.getSection();

      if ( f == c.getFunctions().front().get() )
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
          if ( !( r.getAttributes() &
               (unsigned long) WIR_MemoryRegionAttributes::read ) )
            throw ufFatalError(
              string( "SHF_TRICORE_NOREAD (see TriCore EABI Manual v2.3, " ) +
                "section 4.2.2) not supported by current versions of " +
                "tricore-as based on GNU Binutils." );

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

      // Emit .balign directive. This directive must appear after the .section
      // directive. Otherwise, .balign will have no effect, since the section
      // change will invalidate the location pointer.
      os << string( 8, ' ' ) << ".balign" << string( 9, ' ' )
         << ( 1 << sec.getBlock() ) << endl;
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
     << ", @function" << endl;

  // Emit .global directive.
  if ( ( symPtr != nullptr ) && symPtr->isGlobal() )
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
  dumpTCLdScript dumps a WIR system's memory layout as linker script to an
  output stream in a TriCore- and GNU-ld specific fashion.
*/
void dumpTCLdScript( std::ostream &os, const WIR_System &sys )
{
  DSTART( "void dumpTCLdScript(ostream&, const WIR_System&)" );

  auto &p = sys.getComponents<WIR_BaseProcessor>().begin()->get();

  os << "/*" << endl
     << "  Linker script for " << sys.getSystemName() << " ISA "
     << p.getISAName() << " executables." << endl
     << "  Generated using " << WIR_MODULE << " " << WIR_VERSION << endl
     << "*/" << endl << endl;

  // Write architecture-related headers.
  os << "OUTPUT_FORMAT(\"elf32-tricore\")" << endl
     << "OUTPUT_ARCH(tricore)" << endl
     << "ENTRY(_start)" << endl << endl;
  if ( sys.getSystemName() == "tc1796" )
    os << "/* __TC1796__ __TC13__ with Core TC1.3 */" << endl
       << "__TRICORE_DERIVATE_MEMORY_MAP__ = 0x1796;" << endl << endl;
  else

  if ( sys.getSystemName() == "tc1797" )
    os << "/* __TC1797__ __TC131__ with Core TC1.3.1 */" << endl
       << "__TRICORE_DERIVATE_MEMORY_MAP__ = 0x1797;" << endl << endl;

  // Write list of available memory regions.
  dumpLDScriptRegions( os, sys );

  // Write some global definitions.
  os << "/* Begin of TriCore definitions */" << endl << hex;

  auto it = sys.findComponent( "PFLASH-NC" );
  if ( it != sys.getComponents().end() ) {
    auto &r = dynamic_cast<WIR_MemoryRegion &>( it->get() );
    os << "/* Internal FLASH */" << endl
       << "__EXT_CODE_RAM_BEGIN = 0x" << r.getBaseAddress() << ";" << endl
       << "__EXT_CODE_RAM_SIZE = 0x" << r.getLength() << ";" << endl;
  }
  it = sys.findComponent( "PMI-SRAM" );
  if ( it != sys.getComponents().end() ) {
    auto &r = dynamic_cast<WIR_MemoryRegion &>( it->get() );
    os << "/* Internal RAM */" << endl
       << "__INT_CODE_RAM_BEGIN = 0x" << r.getBaseAddress() << ";" << endl
       << "__INT_CODE_RAM_SIZE = 0x" << r.getLength() << ";" << endl;
  }
  it = sys.findComponent( "DMI-LDRAM" );
  if ( it != sys.getComponents().end() ) {
    auto &r = dynamic_cast<WIR_MemoryRegion &>( it->get() );
    os << "__INT_DATA_RAM_BEGIN = 0x" << r.getBaseAddress() << ";" << endl
       << "__INT_DATA_RAM_SIZE = 0x" << r.getLength() << ";" << endl
       << "__RAM_END = __INT_DATA_RAM_BEGIN + __INT_DATA_RAM_SIZE;" << endl;
  }
  it = sys.findComponent( "PCP-TEXT" );
  if ( it != sys.getComponents().end() ) {
    auto &r = dynamic_cast<WIR_MemoryRegion &>( it->get() );
    os << "/* PCP Memory */" << endl
       << "__PCP_CODE_RAM_BEGIN = 0x" << r.getBaseAddress() << ";" << endl
       << "__PCP_CODE_RAM_SIZE = 0x" << r.getLength() << ";" << endl;
  }
  it = sys.findComponent( "PCP-DATA" );
  if ( it != sys.getComponents().end() ) {
    auto &r = dynamic_cast<WIR_MemoryRegion &>( it->get() );
    os << "__PCP_DATA_RAM_BEGIN = 0x" << r.getBaseAddress() << ";" << endl
       << "__PCP_DATA_RAM_SIZE = 0x" << r.getLength() << ";" << endl;
  }

  os << endl << "/*" << endl << "  Cache:" << endl << "  The "
     << sys.getSystemName() << " has a 2-way set associative instruction cache "
     << "with 32 sets," << endl
     << "  each way can hold 4 double-words (256 bits)" << endl
     << "  => Total cache size: 16 KByte;" << endl
     << "  Replacement strategy: Least Recently Used" << endl
     << "  Refill: critical double word first" << endl << "*/" << endl << endl;

  auto &stack = p.findSection( ".stack" )->get().getRegion();
  size_t stackSize = stack.getLength() / 1024;
  stackSize -= 2;
  os << "/* Set sizes of the user and system stacks */" << endl
     << "__HEAP_SIZE   = DEFINED (__HEAP_SIZE) ? __HEAP_SIZE : 1K ;" << endl
     << "__ISTACK_SIZE = DEFINED (__ISTACK_SIZE) ? __ISTACK_SIZE : 1K ;" << endl
     << "__USTACK_SIZE = DEFINED (__USTACK_SIZE) ? __USTACK_SIZE : " << dec
     << stackSize << "K ;" << endl;

  auto &csa = p.findSection( ".csa" )->get().getRegion();
  size_t csaSize = csa.getLength() / 1024;
  os << "__CSA_SIZE    = DEFINED (__CSA_SIZE) ? __CSA_SIZE : " << csaSize
     << "K ;" << endl;
  os << "/* End of TriCore definitions */" << endl << endl;

  // Write section mapping.
  dumpLDScriptSections( os, sys );
};


/*
  dumpTCLdScriptSection dumps a WIR section as linker script entry to an output
  stream in a TriCore- and GNU-ld specific fashion.
*/
bool dumpTCLdScriptSection( std::ostream &os, const WIR_Section &sec )
{
  DSTART( "bool dumpTCLdScriptSection(ostream&, const WIR_Section&)" );

  // Write main code section.
  if ( sec.getName() == ".text" ) {
    os << "    *(.text)" << endl
       << "    *(.text.*)" << endl
       << "    *(.tspi)" << endl
       << "    *(.tspi.*)" << endl
       << "    *(.pcp_c_ptr_init)" << endl
       << "    *(.pcp_c_ptr_init.*)" << endl
       << "    *(.gnu.linkonce.t.*)" << endl
       << "    *(.gnu.warning)" << endl;
    return( true );
  }

  // Handle the startup/init section first to ensure it is the first code in
  // flash memory.
  if ( sec.getName() == ".startup_code" ) {
    os << "    KEEP (*(.startup_code))" << endl;
    return( true );
  }

  if ( sec.getName() == ".init" ) {
    os << "    *(.init)" << endl
       << "    *(.fini)" << endl;
    return( true );
  }

  // Write .zdata section.
  if ( sec.getName() == ".zdata" ) {
    os << "    ZDATA_BASE = . ;" << endl
       << "    *(.zrodata)" << endl
       << "    *(.zrodata.*)" << endl
       << "    *(.zdata)" << endl
       << "    *(.zdata.*)" << endl
       << "    *(.gnu.linkonce.z.*)" << endl
       << "    *(.bdata)" << endl
       << "    *(.bdata.*)" << endl
       << "    . = ALIGN(8);" << endl
       << "    ZDATA_END = . ;" << endl;
    return( true );
  }

  // Code section for PCP coprocessor.
  if ( sec.getName() == ".pcptext" ) {
    os << "    PCODE_BASE = . ;" << endl
       << "    *(.pcptext)" << endl
       << "    *(.pcptext.*)" << endl
       << "    *(.pcode)" << endl
       << "    *(.pcode.*)" << endl
       << "    . = ALIGN(8) ;" << endl
       << "   PCODE_END = . ;" << endl;
    return( true );
  }

  // Data section for PCP coprocessor.
  if ( sec.getName() == ".pcpdata" ) {
    os << "    PRAM_BASE = . ;" << endl
       << "    *(.pcpdata)" << endl
       << "    *(.pcpdata.*)" << endl
       << "    *(.pdata)" << endl
       << "    *(.pdata.*)" << endl
       << "    . = ALIGN(8) ;" << endl
       << "    PRAM_END = . ;" << endl;
    return( true );
  }

  // Write dummy section for stack handling.
  if ( sec.getName() == ".stack" ) {
    os << "    /*" << endl
       << "      Define a section to store pointer to stack. We do not "
       << "allocate any memory" << endl
       << "      objects here!" << endl
       << "    */" << endl
       << "    __HEAP = . ;" << endl
       << "    __HEAP_END = __HEAP + __HEAP_SIZE ;" << endl
       << "    __ISTACK = __HEAP_END + __ISTACK_SIZE ;" << endl
       << "    __USTACK = __ISTACK + __USTACK_SIZE ;" << endl;
    return( true );
  }

  // Write dummy section for allocating memory for csa.
  if ( sec.getName() == ".csa" ) {
    os << "    /*" << endl
       << "      Define the CSA memory area as an own section. This section "
       << "will be" << endl
       << "      allocated into the internal RAM." << endl
       << "    */" << endl
       << "    __CSA_BEGIN = . ;" << endl
       << "    . +=  __CSA_SIZE ;" << endl
       << "    __CSA_END = .;" << endl;
    return( true );
  }

  return( false );
};


/*
  dumpTCOperation dumps a WIR operation to an output stream in a
  TriCore-specific fashion.

  While dumping an operation's parameters, this function omits any implicit
  parameters and only outputs explicit ones in order to produce valid TriCore
  assembly output that is accepted by a subsequent assembler.
*/
void dumpTCOperation( std::ostream &os, const WIR_Operation &o )
{
  DSTART( "void dumpTCOperation(ostream&, const WIR_Operation&)" );

  // Each operation starts with an assembly directive clearly encoding its bit
  // size.
  os << dec << string( 8, ' ' ) << ".code" << o.getBitWidth() << endl;

  // Dump comments and file information attached to the operation next.
  if ( os.iword( WIR_CommentCont() ) == 1 )
    for ( WIR_Comment &cont : o.getContainers<WIR_Comment>() )
      os << cont << endl;
  if ( os.iword( WIR_FileInfoCont() ) == 1 )
    for ( WIR_FileInfo &cont : o.getContainers<WIR_FileInfo>() )
      os << cont << endl;

  // Output the operation's opcode.
  os << string( 8, ' ' ) << o.getOpCode().getName();

  // Output parameters.
  auto params = o.getParameters();
  bool firstParam = true;
  unsigned int expParamCount = 0;
  WIR_BaseProcessor::OperationFormat f = o.getOperationFormat();

  for ( auto it = params.begin(); it != params.end(); ++it ) {
    auto &p = (*it).get();
    bool emitParam = p.isExplicit();

    // Skip PSW register parameters, since they are somehow implicit.
    if ( p.getType() == WIR_ParameterType::reg ) {
      auto &rp = dynamic_cast<WIR_RegisterParameter &>( p );
      if ( ( os.iword( WIR_ImplicitParams() ) == 0 ) &&
           ( rp.getRegister().getType() == TC131::RegisterType::pswBit ) )
        emitParam = false;
    }

    if ( emitParam ) {
      ++expParamCount;

      // Output separator between parameters.
      if ( firstParam ) {
        os << string( 16 - o.getOpCode().getName().size(), ' ' );
        firstParam = false;
      } else
        os << ", ";

      // Catch special operation formats here.
      if ( ( f == TC131::OperationFormat::AC10BOA ) ||
           ( f == TC131::OperationFormat::AC10BOAPSW ) ||
           ( ( expParamCount == 1 ) &&
             ( ( f == TC131::OperationFormat::AC10ABOA ) ||
               ( f == TC131::OperationFormat::AC10EBOA ) ||
               ( f == TC131::OperationFormat::AC10DBOA_1 ) ||
               ( f == TC131::OperationFormat::AC10DBOA_2 ) ||
               ( f == TC131::OperationFormat::AC10PBOA ) ||
               ( f == TC131::OperationFormat::AC16DBOA ) ||
               ( f == TC131::OperationFormat::SSPC10I_1 ) ||
               ( f == TC131::OperationFormat::SAC4I_1 ) ||
               ( f == TC131::OperationFormat::SAC4I_2 ) ||
               ( f == TC131::OperationFormat::SIC4A ) ||
               ( f == TC131::OperationFormat::SIC4D ) ||
               ( f == TC131::OperationFormat::SSPC10I_2 ) ) ) ||
           ( ( expParamCount == 2 ) &&
             ( ( f == TC131::OperationFormat::AAC10BOA ) ||
               ( f == TC131::OperationFormat::AAC16BOA ) ||
               ( f == TC131::OperationFormat::DAC10BOA ) ||
               ( f == TC131::OperationFormat::DAC16BOA ) ||
               ( f == TC131::OperationFormat::EAC10BOA ) ||
               ( f == TC131::OperationFormat::PAC10BOA ) ||
               ( f == TC131::OperationFormat::SAIC4 ) ||
               ( f == TC131::OperationFormat::SDIC4_1 ) ||
               ( f == TC131::OperationFormat::SIAC4_1 ) ||
               ( f == TC131::OperationFormat::SIAC4_2 ) ||
               ( f == TC131::OperationFormat::SISPC10_1 ) ||
               ( f == TC131::OperationFormat::SISPC10_2 ) ) ) ) {
        // Base + Offset addressing.
        os << "[" << dynamic_cast<WIR_RegisterParameter &>( (*it).get() )
           << "]";
        do {
          ++it;
        } while ( (*it).get().isImplicit() );
        os << dynamic_cast<WIR_BaseImmediateParameter &>( (*it).get() );
      } else

      if ( ( f == TC131::OperationFormat::PBRA ) ||
           ( ( expParamCount == 1 ) &&
             ( ( f == TC131::OperationFormat::PABRA ) ||
               ( f == TC131::OperationFormat::PDBRA_1 ) ||
               ( f == TC131::OperationFormat::PDBRA_2 ) ||
               ( f == TC131::OperationFormat::PEBRA ) ||
               ( f == TC131::OperationFormat::PPBRA_2 ) ) ) ||
           ( ( expParamCount == 2 ) &&
             ( ( f == TC131::OperationFormat::APBRA ) ||
               ( f == TC131::OperationFormat::DPBRA ) ||
               ( f == TC131::OperationFormat::EPBRA ) ||
               ( f == TC131::OperationFormat::PPBRA_1 ) ) ) ) {
        // Bit-reverse addressing.
        os << "[" << dynamic_cast<WIR_RegisterParameter &>( (*it).get() )
           << "+r]";
      } else

      if ( ( f == TC131::OperationFormat::PC10CA ) ||
           ( ( expParamCount == 1 ) &&
             ( ( f == TC131::OperationFormat::PC10ACA ) ||
               ( f == TC131::OperationFormat::PC10DCA_1 ) ||
               ( f == TC131::OperationFormat::PC10DCA_2 ) ||
               ( f == TC131::OperationFormat::PC10ECA ) ||
               ( f == TC131::OperationFormat::PC10PCA ) ) ) ||
           ( ( expParamCount == 2 ) &&
             ( ( f == TC131::OperationFormat::APC10CA ) ||
               ( f == TC131::OperationFormat::DPC10CA ) ||
               ( f == TC131::OperationFormat::EPC10CA ) ||
               ( f == TC131::OperationFormat::PPC10CA ) ) ) ) {
        // Circular addressing.
        os << "[" << dynamic_cast<WIR_RegisterParameter &>( (*it).get() )
           << "+c]";
        do {
          ++it;
        } while ( (*it).get().isImplicit() );
        os << dynamic_cast<WIR_BaseImmediateParameter &>( (*it).get() );
      } else

      if ( ( ( expParamCount == 1 ) &&
             ( ( f == TC131::OperationFormat::SAA_4 ) ||
               ( f == TC131::OperationFormat::SAD_2 ) ) ) ||
           ( ( expParamCount == 2 ) &&
             ( ( f == TC131::OperationFormat::SAA_2 ) ||
               ( f == TC131::OperationFormat::SDA_2 ) ) ) ) {
        // Register indirect addressing.
        os << "[" << dynamic_cast<WIR_RegisterParameter &>( (*it).get() )
           << "]";
      } else

      if ( ( ( expParamCount == 1 ) &&
             ( ( f == TC131::OperationFormat::SAA_6 ) ||
               ( f == TC131::OperationFormat::SAD_3 ) ) ) ||
           ( ( expParamCount == 2 ) &&
             ( ( f == TC131::OperationFormat::SAA_3 ) ||
               ( f == TC131::OperationFormat::SDA_3 ) ) ) ) {
        // Short post-increment addressing.
        os << "[" << dynamic_cast<WIR_RegisterParameter &>( (*it).get() )
           << "+]";
      } else

      if ( ( expParamCount == 2 ) && ( f == TC131::OperationFormat::AL_1 ) ) {
        // Access upper 16 address bits of a label.
        os << "HI:" << dynamic_cast<WIR_LabelParameter &>( (*it).get() );
      } else

      if ( ( ( expParamCount == 1 ) &&
             ( f == TC131::OperationFormat::ALC16DBOA ) ) ||
           ( ( expParamCount == 2 ) &&
             ( ( f == TC131::OperationFormat::AALC16BOA ) ||
               ( f == TC131::OperationFormat::DALC16BOA ) ) ) ) {
        // Access lower 16 address bits of a label.
        os << "[" << dynamic_cast<WIR_RegisterParameter &>( (*it).get() )
           << "]";
        do {
          ++it;
        } while ( (*it).get().isImplicit() );
        os << "LO:" << dynamic_cast<WIR_LabelParameter &>( (*it).get() );
        do {
          ++it;
        } while ( (*it).get().isImplicit() );
        auto &offset = dynamic_cast<TC_Const16_Signed &>( (*it).get() );
        if ( offset.getSignedValue() > 0 )
          os << "+" << offset;
        else

        if ( offset.getSignedValue() < 0 )
          os << offset;
      } else

      if ( ( ( expParamCount == 2 ) &&
             ( f == TC131::OperationFormat::DDDC1_8 ) ) ||
           ( ( expParamCount == 3 ) &&
             ( ( f == TC131::OperationFormat::DDDC1_2 ) ||
               ( f == TC131::OperationFormat::DDDC1_8 ) ||
               ( f == TC131::OperationFormat::DDDDC1_8 ) ||
               ( f == TC131::OperationFormat::EDDC1_2 ) ||
               ( f == TC131::OperationFormat::EEDDC1_8 ) ) ) ||
           ( ( expParamCount == 4 ) &&
             ( ( f == TC131::OperationFormat::DDDDC1_2 ) ||
               ( f == TC131::OperationFormat::DDDDC1_8 ) ||
               ( f == TC131::OperationFormat::EEDDC1_2 ) ||
               ( f == TC131::OperationFormat::EEDDC1_8 ) ) ) ) {
        // L-type multiplication.
        os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() ) << "L";
      } else

      if ( ( ( expParamCount == 3 ) &&
             ( ( f == TC131::OperationFormat::DDDC1_3 ) ||
               ( f == TC131::OperationFormat::EDDC1_3 ) ) ) ||
           ( ( expParamCount == 4 ) &&
             ( ( f == TC131::OperationFormat::DDDDC1_3 ) ||
               ( f == TC131::OperationFormat::EEDDC1_3 ) ) ) ) {
        // LL-type multiplication.
        os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() ) << "LL";
      } else

      if ( ( ( expParamCount == 3 ) &&
             ( ( f == TC131::OperationFormat::DDDC1_4 ) ||
               ( f == TC131::OperationFormat::EDDC1_4 ) ) ) ||
           ( ( expParamCount == 4 ) &&
             ( ( f == TC131::OperationFormat::DDDDC1_4 ) ||
               ( f == TC131::OperationFormat::EEDDC1_4 ) ) ) ) {
        // LU-type multiplication.
        os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() ) << "LU";
      } else

      if ( ( ( expParamCount == 2 ) &&
             ( f == TC131::OperationFormat::DDDC1_9 ) ) ||
           ( ( expParamCount == 3 ) &&
             ( ( f == TC131::OperationFormat::DDDC1_5 ) ||
               ( f == TC131::OperationFormat::DDDC1_9 ) ||
               ( f == TC131::OperationFormat::DDDDC1_9 ) ||
               ( f == TC131::OperationFormat::EDDC1_5 ) ||
               ( f == TC131::OperationFormat::EEDDC1_9 ) ) ) ||
           ( ( expParamCount == 4 ) &&
             ( ( f == TC131::OperationFormat::DDDDC1_5 ) ||
               ( f == TC131::OperationFormat::DDDDC1_9 ) ||
               ( f == TC131::OperationFormat::EEDDC1_5 ) ||
               ( f == TC131::OperationFormat::EEDDC1_9 ) ) ) ) {
        // U-type multiplication.
        os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() ) << "U";
      } else

      if ( ( ( expParamCount == 3 ) &&
             ( ( f == TC131::OperationFormat::DDDC1_6 ) ||
               ( f == TC131::OperationFormat::EDDC1_6 ) ) ) ||
           ( ( expParamCount == 4 ) &&
             ( ( f == TC131::OperationFormat::DDDDC1_6 ) ||
               ( f == TC131::OperationFormat::DEDDC1 ) ||
               ( f == TC131::OperationFormat::EEDDC1_6 ) ) ) ) {
        // UL-type multiplication.
        os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() ) << "UL";
      } else

      if ( ( ( expParamCount == 3 ) &&
             ( ( f == TC131::OperationFormat::DDDC1_7 ) ||
               ( f == TC131::OperationFormat::EDDC1_7 ) ) ) ||
           ( ( expParamCount == 4 ) &&
             ( ( f == TC131::OperationFormat::DDDDC1_7 ) ||
               ( f == TC131::OperationFormat::EEDDC1_7 ) ) ) ) {
        // UU-type multiplication.
        os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() ) << "UU";
      } else {

        // Output current parameter itself.
        switch ( p.getType() ) {

          case WIR_ParameterType::addr: {
            auto &addP = dynamic_cast<WIR_AddressingModeParameter &>( p );

            os << "[";

            if ( addP.getAddressingMode() == TC131::AddressingMode::post ) {
              do {
                ++it;
              } while ( (*it).get().isImplicit() );

              os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() );

              os << "+";
            } else

            if ( addP.getAddressingMode() == TC131::AddressingMode::pre ) {
              os << "+";

              do {
                ++it;
              } while ( (*it).get().isImplicit() );

              os << dynamic_cast<WIR_RegisterParameter &>( (*it).get() );
            }

            os << "]";

            do {
              ++it;
            } while ( (*it).get().isImplicit() );

            os << dynamic_cast<WIR_BaseImmediateParameter &>( (*it).get() );

            break;
          }

          default: {
            // For immediate, register and label parameters, no special handling
            // is required.
            os << p;

            break;
          }
        }
      }
    }
  }
};


/*
  dumpTCRegisterParameter dumps a WIR register parameter to an output stream in
  a TriCore-specific fashion.
*/
void dumpTCRegisterParameter( std::ostream &os,
                              const WIR_RegisterParameter &p )
{
  DSTART(
    "void dumpTCRegisterParameter(ostream&, const WIR_RegisterParameter&)" );

  WIR_BaseRegister &r = p.getRegister();

  // Skip PSW register parameters, since they are somehow implicit.
  if ( ( os.iword( WIR_ImplicitParams() ) == 0 ) &&
       ( r.getType() == TC131::RegisterType::pswBit ) )
    return;

  // We emit the prefix '%' first for good-old tricore-as.
  os << "%";

  if ( r.getType() == TC131::RegisterType::pReg ) {
    // Due to the nasty behavior of tricore-as, extended address registers are
    // dumped by only emitting the name of its first child.
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

  // Output pre-colorings for a virtual register for debugging purposes.
  if ( r.isVirtual() ) {
    auto &vreg = dynamic_cast<WIR_VirtualRegister &>( r );

    if ( vreg.isPrecolored() ) {
      os << "/" << vreg.getPrecolor().getName();
    }
  }
};


/*
  dumpTCComment dumps a WIR comment container to an output stream in a
  TriCore-specific fashion.
*/
void dumpTCComment( std::ostream &os, const WIR_Comment &c )
{
  DSTART( "void dumpTCComment(ostream&, const WIR_Comment&)" );

  os << string( 8, ' ' ) << "# " << c.getText();
};


/*
  dumpTCFileInfo dumps a WIR fileinfo container to an output stream in a
  TriCore-specific fashion.
*/
void dumpTCFileInfo( std::ostream &os, const WIR_FileInfo &f )
{
  DSTART( "void dumpTCFileInfo(ostream&, const WIR_FileInfo&)" );

  os << string( 8, ' ' ) << "# file \"" << f.getFileName() << "\", line " << dec
     << f.getLineNumber();
};

}       // namespace WIR
