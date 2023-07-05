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


//
// Include section
//

// Include standard headers
#include <set>

// Include libuseful headers
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/generic/mips.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_TaskManager t;
  WIR_System sys( "genericmips.sys", t );
  MIPS &p = sys.getComponents<MIPS>().begin()->get();
  auto &r = sys.getComponents<WIR_MemoryRegion>().begin()->get();
  auto &bss = p.getBssSection();
  auto &data = p.getDataSection();
  auto &rodata = p.getRODataSection();
  auto &text = p.getTextSection();

  WIR_CompilationUnit c1;
  WIR_Function f( "foo" );
  MIPS_RegV &x =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &y =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &z =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &t1 =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );
  MIPS_RegV &t2 =
    static_cast<MIPS_RegV &>( f.pushBackVirtualRegister( MIPS_RegV() ) );

  WIR_BasicBlock &bb1 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb3 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb2 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb5 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb4 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb6 = f.pushBackBasicBlock( {} );
  WIR_BasicBlock &bb7 = f.pushBackBasicBlock( {} );

  // Create a CFG.
  bb1.pushBackInstruction(
    { {  MIPS::OpCode::SLTI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( t1, WIR_Usage::def ),
        WIR_RegisterParameter( z, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 2 ) } } );
  bb1.pushBackInstruction(
    { { MIPS::OpCode::BEQ, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( t1, WIR_Usage::use ),
        WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
        WIR_LabelParameter( bb2 ) } } );

  bb3.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( x, WIR_Usage::def ),
        WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
        MIPS_Immediate16_Signed( 2 ) } } );
  bb3.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( bb5 ) } } );

  bb2.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( x, WIR_Usage::def ),
        WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
        MIPS_Immediate16_Signed( 1 ) } } );
  bb2.pushBackInstruction(
    { { MIPS::OpCode::SLTI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( t2, WIR_Usage::def ),
        WIR_RegisterParameter( z, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 3 ) } } );
  bb2.pushBackInstruction(
    { { MIPS::OpCode::BEQ, MIPS::OperationFormat::RRL,
        WIR_RegisterParameter( t2, WIR_Usage::use ),
        WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
        WIR_LabelParameter( bb4 ) } } );

  bb5.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( z, WIR_Usage::def ),
        WIR_RegisterParameter( x, WIR_Usage::use ),
        MIPS_Immediate16_Signed( -3 ) } } );
  bb5.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( x, WIR_Usage::def ),
        WIR_RegisterParameter( p.r0(), WIR_Usage::use ),
        MIPS_Immediate16_Signed( 4 ) } } );
  bb5.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( bb6 ) } } );

  bb4.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( y, WIR_Usage::def ),
        WIR_RegisterParameter( x, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 1 ) } } );
  bb4.pushBackInstruction(
    { { MIPS::OpCode::J, MIPS::OperationFormat::L,
        WIR_LabelParameter( bb7 ) } } );

  bb6.pushBackInstruction(
    { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
        WIR_RegisterParameter( z, WIR_Usage::def ),
        WIR_RegisterParameter( x, WIR_Usage::use ),
        MIPS_Immediate16_Signed( 7 ) } } );

  bb7.pushBackInstruction(
    { { MIPS::OpCode::JR, MIPS::OperationFormat::R_2,
        WIR_RegisterParameter( p.r31(), WIR_Usage::use ) } } );

  c1.pushBackFunction( move( f ) );
  c1.pushBackFunction( WIR_Function( "bar" ) );

  WIR_CompilationUnit &c = sys.pushBackCompilationUnit( c1 );
  WIR_Function &f1 = c.getFunctions().front().get();
  auto it = f1.getBasicBlocks().begin();
  WIR_BasicBlock &b1 = (it++)->get();
  WIR_BasicBlock &b2 = (it++)->get();
  WIR_BasicBlock &b3 = (it++)->get();
  WIR_BasicBlock &b4 = (it++)->get();
  WIR_BasicBlock &b5 = (it++)->get();
  WIR_BasicBlock &b6 = (it++)->get();
  WIR_BasicBlock &b7 = (it++)->get();
  WIR_Function &f2 = c.getFunctions().back().get();
  f2.pushBackBasicBlock(
    { { { MIPS::OpCode::ADDI, MIPS::OperationFormat::RRI,
          WIR_RegisterParameter( p.r15(), WIR_Usage::def ),
          WIR_RegisterParameter( p.r7(), WIR_Usage::use ),
          MIPS_Immediate16_Signed( 42 ) } } } );

  auto &d3 = c.pushBackData( WIR_Data( "d3" ) );
  auto &d2 = c.pushFrontData( WIR_Data( "d2" ) );
  auto &d1 = c.pushFrontData( WIR_Data( "d1" ) );
  auto &d4 = c.pushBackData( WIR_Data( "d4" ) );

  d1.setSize( 5 );
  d2.setSize( 13 );
  d3.setSize( 42 );
  d4.setSize( 1000 );

  // The data objects are assembled into section .bss at the very beginning of
  // region 'RAM'. Start addresses of the data objects are aligned by 3 bits.
  ufAssert( sys.findSymbol( d1 ).getBaseAddress() == r.getBaseAddress() );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 0 ) == sys.findSymbol( d1 ) );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 4 ) == sys.findSymbol( d1 ) );
  ufAssert( sys.findSymbol( d2 ).getBaseAddress() == r.getBaseAddress() + 8 );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 8 ) == sys.findSymbol( d2 ) );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 20 ) == sys.findSymbol( d2 ) );
  ufAssert( sys.findSymbol( d3 ).getBaseAddress() == r.getBaseAddress() + 24 );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 24 ) == sys.findSymbol( d3 ) );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 65 ) == sys.findSymbol( d3 ) );
  ufAssert( sys.findSymbol( d4 ).getBaseAddress() == r.getBaseAddress() + 72 );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 72 ) == sys.findSymbol( d4 ) );
  ufAssert(
    sys.findSymbol( r.getBaseAddress() + 1071 ) == sys.findSymbol( d4 ) );

  ufAssert( bss.getLength() == 1072 );

  // Functions f1 and f2 are assembled into section .text after section .bss in
  // the same region 'RAM'. .text and .bss are separated by 10 bits of
  // alignment so that the symbols within section .text should start at physical
  // address 0x800.
  auto sectionOffset = r.getBaseAddress() + 0x800;
  ufAssert( sys.findSymbol( f1 ).getBaseAddress() == sectionOffset );
  ufAssert( sys.findSymbol( b1 ).getBaseAddress() == sectionOffset );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b1 ) );
  ufAssert( sys.findSymbol( sectionOffset + 7 ) == sys.findSymbol( b1 ) );
  ufAssert(
    sys.findSymbol( b2 ).getBaseAddress() ==
      ( sectionOffset += b1.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b2 ) );
  ufAssert( sys.findSymbol( sectionOffset + 7 ) == sys.findSymbol( b2 ) );
  ufAssert(
    sys.findSymbol( b3 ).getBaseAddress() ==
      ( sectionOffset += b2.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b3 ) );
  ufAssert( sys.findSymbol( sectionOffset + 11 ) == sys.findSymbol( b3 ) );
  ufAssert(
    sys.findSymbol( b4 ).getBaseAddress() ==
      ( sectionOffset += b3.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b4 ) );
  ufAssert( sys.findSymbol( sectionOffset + 11 ) == sys.findSymbol( b4 ) );
  ufAssert(
    sys.findSymbol( b5 ).getBaseAddress() ==
      ( sectionOffset += b4.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b5 ) );
  ufAssert( sys.findSymbol( sectionOffset + 7 ) == sys.findSymbol( b5 ) );
  ufAssert(
    sys.findSymbol( b6 ).getBaseAddress() ==
      ( sectionOffset += b5.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b6 ) );
  ufAssert( sys.findSymbol( sectionOffset + 3 ) == sys.findSymbol( b6 ) );
  ufAssert(
    sys.findSymbol( b7 ).getBaseAddress() ==
      ( sectionOffset += b6.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b7 ) );
  ufAssert( sys.findSymbol( sectionOffset + 3 ) == sys.findSymbol( b7 ) );
  sectionOffset += b7.getSize();

  // Function f2 is assembled after f1, with a size of 8 bytes.
  ufAssert( sectionOffset == 0x838 );
  ufAssert( sys.findSymbol( f2 ).getBaseAddress() == sectionOffset );

  // Check section and region sizes.
  ufAssert( bss.getLength() == 0x430 );
  ufAssert( text.getLength() == 0x3C );
  ufAssert(
    r.getFreeSpace() == r.getLength() - bss.getLength() - text.getLength() );

  // Move some items to different sections.
  sys.findSymbol( d2 ).setConst();                  // -> .rodata
  d4.pushBackInitData( WIR_DataInit( 42 ) );        // -> .data
  sys.findSymbol( b3 ).setSection( *( p.findSection( ".ctors" ) ) );

  // Data objects d1 and d3 are assembled into section .bss at the very
  // beginning of region 'RAM'.
  ufAssert( sys.findSymbol( d1 ).getBaseAddress() == r.getBaseAddress() );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 0 ) == sys.findSymbol( d1 ) );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 4 ) == sys.findSymbol( d1 ) );
  ufAssert( sys.findSymbol( d3 ).getBaseAddress() == 0x8 );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 8 ) == sys.findSymbol( d3 ) );
  ufAssert( sys.findSymbol( r.getBaseAddress() + 49 ) == sys.findSymbol( d3 ) );

  // With an alignment of 10 bits, section .ctors with basic block b3 follows.
  ufAssert( sys.findSymbol( b3 ).getBaseAddress() == 0x400 );
  ufAssert(
    sys.findSymbol( WIR_MemoryAddress( 0x400 ) ) == sys.findSymbol( b3 ) );
  ufAssert(
    sys.findSymbol( WIR_MemoryAddress( 0x40B ) ) == sys.findSymbol( b3 ) );

  // With an alignment of 10 bits, section .data with d4 follows.
  ufAssert( sys.findSymbol( d4 ).getBaseAddress() == 0x800 );
  ufAssert(
    sys.findSymbol( WIR_MemoryAddress( 0x800 ) ) == sys.findSymbol( d4 ) );
  ufAssert(
    sys.findSymbol( WIR_MemoryAddress( 0xBE7 ) ) == sys.findSymbol( d4 ) );

  // With an alignment of 10 bits, section .rodata with d2 follows.
  ufAssert( sys.findSymbol( d2 ).getBaseAddress() == 0xC00 );
  ufAssert(
    sys.findSymbol( WIR_MemoryAddress( 0xC00 ) ) == sys.findSymbol( d2 ) );
  ufAssert(
    sys.findSymbol( WIR_MemoryAddress( 0xC0C ) ) == sys.findSymbol( d2 ) );

  // With an alignment of 10 bits, section .text follows.
  sectionOffset = 0x1000;
  ufAssert( sys.findSymbol( f1 ).getBaseAddress() == sectionOffset );
  ufAssert( sys.findSymbol( b1 ).getBaseAddress() == sectionOffset );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b1 ) );
  ufAssert( sys.findSymbol( sectionOffset + 7 ) == sys.findSymbol( b1 ) );
  ufAssert(
    sys.findSymbol( b2 ).getBaseAddress() ==
      ( sectionOffset += b1.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b2 ) );
  ufAssert( sys.findSymbol( sectionOffset + 7 ) == sys.findSymbol( b2 ) );
  ufAssert(
    sys.findSymbol( b4 ).getBaseAddress() ==
      ( sectionOffset += b2.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b4 ) );
  ufAssert( sys.findSymbol( sectionOffset + 11 ) == sys.findSymbol( b4 ) );
  ufAssert(
    sys.findSymbol( b5 ).getBaseAddress() ==
      ( sectionOffset += b4.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b5 ) );
  ufAssert( sys.findSymbol( sectionOffset + 7 ) == sys.findSymbol( b5 ) );
  ufAssert(
    sys.findSymbol( b6 ).getBaseAddress() ==
      ( sectionOffset += b5.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b6 ) );
  ufAssert( sys.findSymbol( sectionOffset + 3 ) == sys.findSymbol( b6 ) );
  ufAssert(
    sys.findSymbol( b7 ).getBaseAddress() ==
      ( sectionOffset += b6.getSize() ) );
  ufAssert( sys.findSymbol( sectionOffset ) == sys.findSymbol( b7 ) );
  ufAssert( sys.findSymbol( sectionOffset + 3 ) == sys.findSymbol( b7 ) );
  sectionOffset += b7.getSize();

  // Function f2 is assembled after f1, with a size of 8 bytes.
  ufAssert( sectionOffset == 0x102C );
  ufAssert( sys.findSymbol( f2 ).getBaseAddress() == sectionOffset + 4 );

  // Check section and region sizes.
  ufAssert( bss.getLength() == 0x32 );
  ufAssert( p.findSection( ".ctors" )->get().getLength() == 0xC );
  ufAssert( data.getLength() == 0x3E8 );
  ufAssert( rodata.getLength() == 0xD );
  ufAssert( text.getLength() == 0x34 );

  ufAssert(
    r.getFreeSpace() ==
      r.getLength() - bss.getLength() -
      p.findSection( ".ctors" )->get().getLength() - data.getLength() -
      rodata.getLength() - text.getLength() );

  return( 0 );
}
