/*

   This source file belongs to the

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
  @file rvio.cc
  @brief This file implements RISC-V-specific stream I/O routines for the %WIR
         library.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/exceptions.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>


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
  riscv is an I/O manipulator that provides a RISC-V assembler dump of a WIR.
*/
std::ostream &riscv( std::ostream &os )
{
  DSTART( "ostream& riscv(ostream&)" );

  os.iword( WIR_ProcessorIO() ) = RV32I::getProcessorTypeID();
  return( os );
};


/*
  dumpRVBasicBlock dumps a WIR basic block to an output stream in a RISC-V-
  specific fashion.
*/
void dumpRVBasicBlock( std::ostream &os, const WIR_BasicBlock &b )
{
  DSTART( "void dumpRVBasicBlock(ostream&, const WIR_BasicBlock&)" );

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
  dumpRVCompilationUnit dumps a WIR compilation unit to an output stream in a
  RISC-V-specific fashion.
*/
void dumpRVCompilationUnit( std::ostream &os, const WIR_CompilationUnit &c )
{
  DSTART( "void dumpRVCompilationUnit(ostream&, const WIR_CompilationUnit&)" );

  RV32IMC p;
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
  dumpRVData dumps a WIR data object to an output stream in a RISC-V-specific
  fashion.
*/
void dumpRVData( std::ostream &os, const WIR_Data &d )
{
  DSTART( "void dumpRVData(ostream&, const WIR_Data&)" );

  if ( d.isInserted() && d.getCompilationUnit().isInserted() ) {
    WIR_System &sys = d.getCompilationUnit().getSystem();
    WIR_Symbol &sym = sys.findSymbol( d );
    const WIR_Section &sec = sym.getSection();

    // Emit .globl directive.
    if ( sym.isGlobal() )
      os << string( 8, ' ' ) << ".globl" << string( 9, ' ' ) << d.getName()
         << endl;

    // if ( d.isInitialized() || ( sym.getSection().getName() != ".bss" ) ) {
    os << string( 8, ' ' ) << ".align" << string( 9, ' ' )
        << ( 1 << sec.getBlock() ) << endl;
    os << string( 8, ' ' ) << ".type" << string( 11, ' ' ) << d.getName()
        << ", @object" << endl;
    os << string( 8, ' ' ) << ".size" << string( 11, ' ' ) << d.getName()
        << ", " << d.getSize() << endl;
    os << d.getName() << ":" << endl;

    if ( !d.isInitialized() )
      os << string( 8, ' ' ) << ".zero" << string( 10, ' ' ) << d.getSize()
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
  }
};


/*
  dumpRVDataSection dumps a data section to an output stream in a RISC-V-
  specific fashion.
*/
void dumpRVDataSection( std::ostream &os, WIR_Section *sec,
                        const std::list<std::reference_wrapper<WIR_Data>> &l,
                        bool firstSec )
{
  DSTART(
    "void dumpRVDataSection(ostream&, WIR_Section*, const list<reference_wrapper<WIR_Data> >&, bool)" );

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
  dumpRVFunction dumps a WIR function to an output stream in a RISC-V-specific
  fashion.
*/
void dumpRVFunction( std::ostream &os, const WIR_Function &f )
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
          if ( r.getAttributes() &
               (unsigned long) WIR_MemoryRegionAttributes::read )
            os << "r";
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


void dumpRVOperation( std::ostream &os, const WIR_Operation &o )
{
  DSTART( "void dumpRVOperation(ostream&, const WIR_Operation&)" );

  // Dump comments and file information attached to the operation first.
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

    if ( emitParam ) {
      ++expParamCount;

      // Output separator between parameters.
      if ( firstParam ) {
        os << string( 16 - o.getOpCode().getName().size(), ' ' );
        firstParam = false;
      } else

      // Skip comma separator between parameters for special operation formats.
      if ( ( expParamCount != 3 ) ||
           ( ( f != RV32I::OperationFormat::RC12R_1 ) &&
             ( f != RV32I::OperationFormat::RC12R_2 ) &&
             ( f != RV32I::OperationFormat::RLR_1 ) &&
             ( f != RV32I::OperationFormat::RLR_2 ) &&
             ( f != RV32IC::OperationFormat::SRC5R_1 ) &&
             ( f != RV32IC::OperationFormat::SRC5R_2 ) &&
             ( f != RV32IC::OperationFormat::SRC6R_1 ) &&
             ( f != RV32IC::OperationFormat::SRC6R_2 ) ) )
        os << ", ";

      // Catch special operation formats here.
      if ( ( f == RV32I::OperationFormat::RL_2 ) && ( expParamCount == 2 ) )
        // HI-part of a label.
        os << "%hi(" << dynamic_cast<WIR_LabelParameter &>( it->get() ) << ")";
      else

      if ( ( ( f == RV32I::OperationFormat::RC12R_1 ) ||
             ( f == RV32I::OperationFormat::RC12R_2 ) ||
             ( f == RV32IC::OperationFormat::SRC5R_1 ) ||
             ( f == RV32IC::OperationFormat::SRC5R_2 ) ||
             ( f == RV32IC::OperationFormat::SRC6R_1 ) ||
             ( f == RV32IC::OperationFormat::SRC6R_2 ) ) &&
           ( expParamCount == 3 ) )
        // Base + Offset addressing.
        os << "(" << dynamic_cast<WIR_RegisterParameter &>( it->get() ) << ")";
      else

      if ( ( f == RV32I::OperationFormat::RRL_2 ) && ( expParamCount == 3 ) )
        // LO-part of a label.
        os << "%lo(" << dynamic_cast<WIR_LabelParameter &>( it->get() ) << ")";
      else

      if ( ( ( f == RV32I::OperationFormat::RLR_1 ) ||
             ( f == RV32I::OperationFormat::RLR_2 ) ) &&
           ( expParamCount == 2 ) ) {
        // LO-part of a label.
        os << "%lo(" << dynamic_cast<WIR_LabelParameter &>( it->get() ) << ")";

        // Next parameter.
        do
          ++it;
        while ( ( it != params.end() ) && it->get().isImplicit() );

        os << "(" << dynamic_cast<WIR_RegisterParameter &>( it->get() ) << ")";
      } else

      if ( ( o.getOpCode() == RV32I::OpCode::J ) && ( expParamCount == 1 ) )
        // In the RISC-V RV32I ISA, unconditional jumps boil down to JALs with
        // x0 as return address register. Since this x0 register parameter is
        // not part of the WIR J operation, we have to handle it here
        // explicitly.
        os << "x0, " << p;
      else
        // For immediate, register and label parameters, no special handling
        // is required.
        os << p;
    }
  }

  if ( ( o.getOpCode() == RV32I::OpCode::MOV ) )
    // The move operation is a pseudo-operation de facto realized by an ADDI.
    // The first parameter of the move (and the ADDI) is the target register
    // while the second one is the source register. For a proper realization,
    // the ADDI needs a 0 as immediate value, which is added here.
    os << ", 0";
};


/*
  dumpRVRegisterParameter dumps a WIR register parameter to an output stream in
  a RISCV-V-specific fashion.
*/
void dumpRVRegisterParameter( std::ostream &os, const WIR_RegisterParameter &p )
{
  DSTART(
    "void dumpRVRegisterParameter(ostream&, const WIR_RegisterParameter&)" );

  WIR_BaseRegister &r = p.getRegister();

    // Output the parameter's register name.
    os << r.getName();

  // Output pre-colorings for a virtual register for debugging purposes.
  if ( r.isVirtual() ) {
    auto &vreg = dynamic_cast<WIR_VirtualRegister &>( r );

    if ( vreg.isPrecolored() )
      os << "/" << vreg.getPrecolor().getName();
  }
};


/*
  dumpRVComment dumps a WIR comment container to an output stream in a
  RISC-V-specific fashion.
*/
void dumpRVComment( std::ostream &os, const WIR_Comment &c )
{
  DSTART( "void dumpRVComment(ostream&, const WIR_Comment&)" );

  os << string( 8, ' ' ) << "# " << c.getText();
};


/*
  dumpRVFileInfo dumps a WIR fileinfo container to an output stream in a
  RISC-V-specific fashion.
*/
void dumpRVFileInfo( std::ostream &os, const WIR_FileInfo &f )
{
  DSTART( "void dumpRVFileInfo(ostream&, const WIR_FileInfo&)" );

  os << string( 8, ' ' ) << "# file \"" << f.getFileName() << "\", line " << dec
     << f.getLineNumber();
};

}       // namespace WIR
