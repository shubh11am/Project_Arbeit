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
  @file rv32i.cc
  @brief This file implements the specific interface of the RISC-V RV32I Base
         Integer instruction set, version 2.0.

  @author Jonas Oltmanns <Jonas.Oltmanns@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32i.h>
#include <arch/riscv/rv32ic.h>
#include <arch/riscv/rv32im.h>
#include <arch/riscv/rv32imc.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for RV32I processor architectures.
*/
RV32I::RV32I( void ) :
  WIR_Processor<RV32I> {}
{
  DSTART( "RV32I::RV32I()" );

  // Specify the processor architecture modeled by this class.
  setProcessorName( "RISC-V" );
  setISAName( "RV32I V2.0" );

  // Create physical registers for all 32 general purpose registers.
  // For the meaning of the individual registers, please refer to file
  // doc/riscv-spec-v2.2.pdf, page 109.
  addPhReg<RV_RegP>( "0" );
  addPhReg<RV_RegP>( "1" );
  addPhReg<RV_RegP>( "2", true );
  addPhReg<RV_RegP>( "3" );
  addPhReg<RV_RegP>( "4" );
  addPhReg<RV_RegP>( "5" );
  addPhReg<RV_RegP>( "6" );
  addPhReg<RV_RegP>( "7" );
  addPhReg<RV_RegP>( "8" );
  addPhReg<RV_RegP>( "9" );
  addPhReg<RV_RegP>( "10" );
  addPhReg<RV_RegP>( "11" );
  addPhReg<RV_RegP>( "12" );
  addPhReg<RV_RegP>( "13" );
  addPhReg<RV_RegP>( "14" );
  addPhReg<RV_RegP>( "15" );
  addPhReg<RV_RegP>( "16" );
  addPhReg<RV_RegP>( "17" );
  addPhReg<RV_RegP>( "18" );
  addPhReg<RV_RegP>( "19" );
  addPhReg<RV_RegP>( "20" );
  addPhReg<RV_RegP>( "21" );
  addPhReg<RV_RegP>( "22" );
  addPhReg<RV_RegP>( "23" );
  addPhReg<RV_RegP>( "24" );
  addPhReg<RV_RegP>( "25" );
  addPhReg<RV_RegP>( "26" );
  addPhReg<RV_RegP>( "27" );
  addPhReg<RV_RegP>( "28" );
  addPhReg<RV_RegP>( "29" );
  addPhReg<RV_RegP>( "30" );
  addPhReg<RV_RegP>( "31" );
};


/*
  Copy constructor.
*/
RV32I::RV32I( const RV32I &__o ) :
  WIR_Processor<RV32I> { __o }
{
  DSTART( "RV32I::RV32I(const RV32I&)"  );
};


/*
  Move constructor.
*/
RV32I::RV32I( RV32I &&__o ) :
  WIR_Processor<RV32I> { move( __o ) }
{
  DSTART( "RV32I::RV32I(RV32I&&)" );
};


/*
  Destructor.
*/
RV32I::~RV32I( void )
{
  DSTART( "RV32I::~RV32I()" );
};


/*
  Copy-assignment operator.
*/
RV32I & RV32I::operator = ( const RV32I &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Processor<RV32I>::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
RV32I & RV32I::operator = ( RV32I &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Processor<RV32I>::operator = ( move( __o ) );

  return( *this );
};


/*
  init performs some global initialization tasks for RV32I processor
  architectures.

  This includes setting up the assignment of valid operation formats to RV32I
  opcodes.

  init shall be called globally by WIR_Init(). It shall only perform tasks that
  cannot be expressed as initializations of static class members (since the
  order of static initialization is unspecified in C++) and that thus require
  execution by active code.
*/
void RV32I::init( void )
{
  DSTART( "static void RV32I::init()" );

  //
  // Register RV32I I/O functions.
  //

  WIR_Registry::registerBasicBlockDumper(
    getProcessorTypeID(), dumpRVBasicBlock );
  WIR_Registry::registerCompilationUnitDumper(
    getProcessorTypeID(), dumpRVCompilationUnit );
  WIR_Registry::registerDataDumper( getProcessorTypeID(), dumpRVData );
  WIR_Registry::registerDataSectionDumper(
    getProcessorTypeID(), dumpRVDataSection );
  WIR_Registry::registerFunctionDumper( getProcessorTypeID(), dumpRVFunction );
  WIR_Registry::registerOperationDumper(
    getProcessorTypeID(), dumpRVOperation );
  WIR_Registry::registerRegisterParameterDumper(
    getProcessorTypeID(), dumpRVRegisterParameter );
  WIR_Registry::registerCommentDumper( getProcessorTypeID(), dumpRVComment );
  WIR_Registry::registerFileInfoDumper( getProcessorTypeID(), dumpRVFileInfo );


  //
  // RV32I operation formats.
  //

  WIR_BasicBlock b;
  RV_RegV *regV = new RV_RegV;

  WIR_Registry::registerOperationFormat(
    OperationFormat::L_1,
    { new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::NULL_1, {} );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RC12R_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new RV_Const12_Signed( 0 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RC12R_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new RV_Const12_Signed( 0 ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RC20_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new RV_Const20_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RL_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RL_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RLR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RLR_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_LabelParameter( b ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RRC5_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new RV_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RRC12_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new RV_Const12_Signed( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RRL_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RRL_2,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_LabelParameter( b ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RRR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RSC5_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new RV_Const5_Unsigned( 0 ) } );

  WIR_Registry::registerOperationFormat(
    OperationFormat::RSR_1,
    { new WIR_RegisterParameter( *regV, WIR_Usage::def ),
      new WIR_StringParameter( "frm" ),
      new WIR_RegisterParameter( *regV, WIR_Usage::use ) } );


  //
  // RV32I opcode to operation format mapping.
  //

  WIR_Registry::registerOpCode( OpCode::ADD, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::ADDI, OperationFormat::RRC12_1 );
  WIR_Registry::registerOpCode( OpCode::ADDI, OperationFormat::RRL_2 );
  WIR_Registry::registerOpCode( OpCode::AND, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::ANDI, OperationFormat::RRC12_1 );
  WIR_Registry::registerOpCode( OpCode::ANDI, OperationFormat::RRL_2 );
  WIR_Registry::registerOpCode( OpCode::AUIPC, OperationFormat::RC20_1 );
  WIR_Registry::registerOpCode( OpCode::AUIPC, OperationFormat::RL_2 );
  WIR_Registry::registerOpCode( OpCode::BEQ, OperationFormat::RRL_1 );
  WIR_Registry::registerOpCode( OpCode::BGE, OperationFormat::RRL_1 );
  WIR_Registry::registerOpCode( OpCode::BGEU, OperationFormat::RRL_1 );
  WIR_Registry::registerOpCode( OpCode::BLT, OperationFormat::RRL_1 );
  WIR_Registry::registerOpCode( OpCode::BLTU, OperationFormat::RRL_1 );
  WIR_Registry::registerOpCode( OpCode::BNE, OperationFormat::RRL_1 );
  WIR_Registry::registerOpCode( OpCode::CSRRC, OperationFormat::RSR_1 );
  WIR_Registry::registerOpCode( OpCode::CSRRCI, OperationFormat::RSC5_1 );
  WIR_Registry::registerOpCode( OpCode::CSRRS, OperationFormat::RSR_1 );
  WIR_Registry::registerOpCode( OpCode::CSRRSI, OperationFormat::RSC5_1 );
  WIR_Registry::registerOpCode( OpCode::CSRRW, OperationFormat::RSR_1 );
  WIR_Registry::registerOpCode( OpCode::CSRRWI, OperationFormat::RSC5_1 );
  WIR_Registry::registerOpCode( OpCode::EBREAK, OperationFormat::NULL_1 );
  WIR_Registry::registerOpCode( OpCode::ECALL, OperationFormat::NULL_1 );
  WIR_Registry::registerOpCode( OpCode::J, OperationFormat::L_1 );
  WIR_Registry::registerOpCode( OpCode::JAL, OperationFormat::RL_1 );
  WIR_Registry::registerOpCode( OpCode::JALR, OperationFormat::RC12R_1 );
  WIR_Registry::registerOpCode( OpCode::JALR, OperationFormat::RLR_1 );
  WIR_Registry::registerOpCode( OpCode::JALR, OperationFormat::RRC12_1 );
  WIR_Registry::registerOpCode( OpCode::JALR, OperationFormat::RRL_2 );
  WIR_Registry::registerOpCode( OpCode::LB, OperationFormat::RC12R_1 );
  WIR_Registry::registerOpCode( OpCode::LB, OperationFormat::RLR_1 );
  WIR_Registry::registerOpCode( OpCode::LBU, OperationFormat::RC12R_1 );
  WIR_Registry::registerOpCode( OpCode::LBU, OperationFormat::RLR_1 );
  WIR_Registry::registerOpCode( OpCode::LH, OperationFormat::RC12R_1 );
  WIR_Registry::registerOpCode( OpCode::LH, OperationFormat::RLR_1 );
  WIR_Registry::registerOpCode( OpCode::LHU, OperationFormat::RC12R_1 );
  WIR_Registry::registerOpCode( OpCode::LHU, OperationFormat::RLR_1 );
  WIR_Registry::registerOpCode( OpCode::LUI, OperationFormat::RC20_1 );
  WIR_Registry::registerOpCode( OpCode::LUI, OperationFormat::RL_2 );
  WIR_Registry::registerOpCode( OpCode::LW, OperationFormat::RC12R_1 );
  WIR_Registry::registerOpCode( OpCode::LW, OperationFormat::RLR_1 );
  WIR_Registry::registerOpCode( OpCode::MOV, OperationFormat::RR_1 );
  WIR_Registry::registerOpCode( OpCode::OR, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::ORI, OperationFormat::RRC12_1 );
  WIR_Registry::registerOpCode( OpCode::ORI, OperationFormat::RRL_2 );
  WIR_Registry::registerOpCode( OpCode::SB, OperationFormat::RC12R_2 );
  WIR_Registry::registerOpCode( OpCode::SB, OperationFormat::RLR_2 );
  WIR_Registry::registerOpCode( OpCode::SH, OperationFormat::RC12R_2 );
  WIR_Registry::registerOpCode( OpCode::SH, OperationFormat::RLR_2 );
  WIR_Registry::registerOpCode( OpCode::SLL, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::SLLI, OperationFormat::RRC5_1 );
  WIR_Registry::registerOpCode( OpCode::SLT, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::SLTI, OperationFormat::RRC12_1 );
  WIR_Registry::registerOpCode( OpCode::SLTI, OperationFormat::RRL_2 );
  WIR_Registry::registerOpCode( OpCode::SLTIU, OperationFormat::RRC12_1 );
  WIR_Registry::registerOpCode( OpCode::SLTIU, OperationFormat::RRL_2 );
  WIR_Registry::registerOpCode( OpCode::SLTU, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::SRA, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::SRAI, OperationFormat::RRC5_1 );
  WIR_Registry::registerOpCode( OpCode::SRL, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::SRLI, OperationFormat::RRC5_1 );
  WIR_Registry::registerOpCode( OpCode::SUB, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::SW, OperationFormat::RC12R_2 );
  WIR_Registry::registerOpCode( OpCode::SW, OperationFormat::RLR_2 );
  WIR_Registry::registerOpCode( OpCode::XOR, OperationFormat::RRR_1 );
  WIR_Registry::registerOpCode( OpCode::XORI, OperationFormat::RRC12_1 );
  WIR_Registry::registerOpCode( OpCode::XORI, OperationFormat::RRL_2 );


  //
  // Register this current processor model.
  //

  WIR_Registry::registerProcessor( RV32I() );


  //
  // Finally, initialize derived processor models.
  //

  RV32IM::init();
  RV32IC::init();
  RV32IMC::init();
};


/*
  Access to physical general-purpose register x0.
*/
const RV_RegP &RV32I::x0( void ) const
{
  DSTART( "const RV_RegP& RV32I::x0() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 0 ).get() ) );
};


/*
  Access to physical general-purpose register x1.
*/
const RV_RegP &RV32I::x1( void ) const
{
  DSTART( "const RV_RegP& RV32I::x1() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 1 ).get() ) );
};


/*
  Access to physical general-purpose register x2.
*/
const RV_RegP &RV32I::x2( void ) const
{
  DSTART( "const RV_RegP& RV32I::x2() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 2 ).get() ) );
};


/*
  Access to physical general-purpose register x3.
*/
const RV_RegP &RV32I::x3( void ) const
{
  DSTART( "const RV_RegP& RV32I::x3() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 3 ).get() ) );
};


/*
  Access to physical general-purpose register x4.
*/
const RV_RegP &RV32I::x4( void ) const
{
  DSTART( "const RV_RegP& RV32I::x4() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 4 ).get() ) );
};


/*
  Access to physical general-purpose register x5.
*/
const RV_RegP &RV32I::x5( void ) const
{
  DSTART( "const RV_RegP& RV32I::x5() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 5 ).get() ) );
};


/*
  Access to physical general-purpose register x6.
*/
const RV_RegP &RV32I::x6( void ) const
{
  DSTART( "const RV_RegP& RV32I::x6() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 6 ).get() ) );
};


/*
  Access to physical general-purpose register x7.
*/
const RV_RegP &RV32I::x7( void ) const
{
  DSTART( "const RV_RegP& RV32I::x7() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 7 ).get() ) );
};


/*
  Access to physical general-purpose register x8.
*/
const RV_RegP &RV32I::x8( void ) const
{
  DSTART( "const RV_RegP& RV32I::x8() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 8 ).get() ) );
};


/*
  Access to physical general-purpose register x9.
*/
const RV_RegP &RV32I::x9( void ) const
{
  DSTART( "const RV_RegP& RV32I::x9() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 9 ).get() ) );
};


/*
  Access to physical general-purpose register x10.
*/
const RV_RegP &RV32I::x10( void ) const
{
  DSTART( "const RV_RegP& RV32I::x10() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 10 ).get() ) );
};


/*
  Access to physical general-purpose register x11.
*/
const RV_RegP &RV32I::x11( void ) const
{
  DSTART( "const RV_RegP& RV32I::x11() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 11 ).get() ) );
};


/*
  Access to physical general-purpose register x12.
*/
const RV_RegP &RV32I::x12( void ) const
{
  DSTART( "const RV_RegP& RV32I::x12() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 12 ).get() ) );
};


/*
  Access to physical general-purpose register x13.
*/
const RV_RegP &RV32I::x13( void ) const
{
  DSTART( "const RV_RegP& RV32I::x13() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 13 ).get() ) );
};


/*
  Access to physical general-purpose register x14.
*/
const RV_RegP &RV32I::x14( void ) const
{
  DSTART( "const RV_RegP& RV32I::x14() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 14 ).get() ) );
};


/*
  Access to physical general-purpose register x15.
*/
const RV_RegP &RV32I::x15( void ) const
{
  DSTART( "const RV_RegP& RV32I::x15() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 15 ).get() ) );
};


/*
  Access to physical general-purpose register x16.
*/
const RV_RegP &RV32I::x16( void ) const
{
  DSTART( "const RV_RegP& RV32I::x16() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 16 ).get() ) );
};


/*
  Access to physical general-purpose register x17.
*/
const RV_RegP &RV32I::x17( void ) const
{
  DSTART( "const RV_RegP& RV32I::x17() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 17 ).get() ) );
};


/*
  Access to physical general-purpose register x18.
*/
const RV_RegP &RV32I::x18( void ) const
{
  DSTART( "const RV_RegP& RV32I::x18() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 18 ).get() ) );
};


/*
  Access to physical general-purpose register x19.
*/
const RV_RegP &RV32I::x19( void ) const
{
  DSTART( "const RV_RegP& RV32I::x19() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 19 ).get() ) );
};


/*
  Access to physical general-purpose register x20.
*/
const RV_RegP &RV32I::x20( void ) const
{
  DSTART( "const RV_RegP& RV32I::x20() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 20 ).get() ) );
};

/*
  Access to physical general-purpose register x21.
*/
const RV_RegP &RV32I::x21( void ) const
{
  DSTART( "const RV_RegP& RV32I::x21() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 21 ).get() ) );
};


/*
  Access to physical general-purpose register x22.
*/
const RV_RegP &RV32I::x22( void ) const
{
  DSTART( "const RV_RegP& RV32I::x22() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 22 ).get() ) );
};


/*
  Access to physical general-purpose register x23.
*/
const RV_RegP &RV32I::x23( void ) const
{
  DSTART( "const RV_RegP& RV32I::x23() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 23 ).get() ) );
};


/*
  Access to physical general-purpose register x24.
*/
const RV_RegP &RV32I::x24( void ) const
{
  DSTART( "const RV_RegP& RV32I::x24() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 24 ).get() ) );
};


/*
  Access to physical general-purpose register x25.
*/
const RV_RegP &RV32I::x25( void ) const
{
  DSTART( "const RV_RegP& RV32I::x25() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 25 ).get() ) );
};


/*
  Access to physical general-purpose register x26.
*/
const RV_RegP &RV32I::x26( void ) const
{
  DSTART( "const RV_RegP& RV32I::x26() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 26 ).get() ) );
};


/*
  Access to physical general-purpose register x27.
*/
const RV_RegP &RV32I::x27( void ) const
{
  DSTART( "const RV_RegP& RV32I::x27() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 27 ).get() ) );
};


/*
  Access to physical general-purpose register x28.
*/
const RV_RegP &RV32I::x28( void ) const
{
  DSTART( "const RV_RegP& RV32I::x28() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 28 ).get() ) );
};


/*
  Access to physical general-purpose register x29.
*/
const RV_RegP &RV32I::x29( void ) const
{
  DSTART( "const RV_RegP& RV32I::x29() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 29 ).get() ) );
};


/*
  Access to physical general-purpose register x30.
*/
const RV_RegP &RV32I::x30( void ) const
{
  DSTART( "const RV_RegP& RV32I::x30() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 30 ).get() ) );
};


/*
  Access to physical general-purpose register x31.
*/
const RV_RegP &RV32I::x31( void ) const
{
  DSTART( "const RV_RegP& RV32I::x31() const" );

  return( dynamic_cast<RV_RegP &>( mPhRegReferences.at( 31 ).get() ) );
};


/*
  Access to return address pointer.
*/
const RV_RegP &RV32I::RA( void ) const
{
  DSTART( "const RV_RegP& RV32I::RA() const" );

  return( x1() );
};


/*
  Access to stack pointer.
*/
const RV_RegP &RV32I::SP( void ) const
{
  DSTART( "const RV_RegP& RV32I::SP() const" );

  return( x2() );
};


/*
  isX0 checks whether the specified register is the RISC-V's x0 register which
  is always zero.
*/
bool RV32I::isX0( const WIR_BaseRegister &r )
{
  DSTART( "static bool RV32I::isX0(const WIR_BaseRegister&)" );

  if ( r.isVirtual() ) {
    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

    if ( vr.isPrecolored() && ( vr.getPrecolor().getName() == "x0" ) )
      return( true );
  } else
    return( r.getName() == "x0" );

  return( false );
};


/*
  isSP checks whether the specified register is the RISC-V's stack pointer.
*/
bool RV32I::isSP( const WIR_BaseRegister &r )
{
  DSTART( "static bool RV32I::isSP(const WIR_BaseRegister&)" );

  if ( r.isVirtual() ) {
    auto &vr = dynamic_cast<const WIR_VirtualRegister &>( r );

    if ( vr.isPrecolored() )
      return( vr.getPrecolor().isStackPointer() );
  } else {
    auto &pr = dynamic_cast<const WIR_PhysicalRegister &>( r );

    return( pr.isStackPointer() );
  }

  return( false );
};


/*
  adjustStack allocates additional space in the specified function's stack
  frame and adjusts all stack-related memory accesses accordingly.

  According to the RISC-V ABI (section 2.1, Integer Calling Convention), the
  stack grows downwards (towards lower addresses) and the stack pointer shall be
  aligned to a 128-bit boundary upon procedure entry. The first argument passed
  on the stack is located at offset zero of the stack pointer on function entry;
  following arguments are stored at correspondingly higher addresses.

  In the standard ABI, the stack pointer must remain aligned throughout
  procedure execution. [...]

  Procedures must not rely upon the persistence of stack-allocated data whose
  addresses lie below the stack pointer.

  (Stack
    growing
    direction)
        |
        |   +-------------------------+      (high address)
        |   | Local Variables Func 1  |
        |   +-------------------------+
        |   | Argument Area for func- |
        |   | tions called by Func 1  |      (first argument passed on stack)
        |   +-------------------------+
        |   | Local Variables Func 2  |
        |   +-------------------------+
        |   | Argument Area for func- |
        |   | tions called by Func 2  |
        |   +-------------------------+ <--- Stack Pointer (SP) at entry
        V   | Local Variables Func 3  |      (CALL) to Function 3
            +-------------------------+
            | Argument Area for func- |
            | tions called by Func 3  |
            +-------------------------+ <--- Stack Pointer (SP) after stack
            |                         |      allocation of Function 3
            |           ...           |
            +-------------------------+      (low address)
*/
void RV32I::adjustStack( WIR_Function &f, int size,
                        const std::list<std::reference_wrapper<WIR_Instruction>> &insertedSpillCode )
{
  DSTART(
    "static void RV32I::adjustStack(WIR_Function&, int, const "
    "list<reference_wrapper<WIR_Instruction> >&)" );

  DOUT(
    "Adjusting stack of function '" << f.getName() << "' by " << size <<
    " bytes." << endl );

  if ( size == 0 )
    return;

  // Align stack pointer to 16 bytes.
  int remainder = size % 16;
  if ( remainder != 0 ) {
    size += 16 - remainder;
    DOUT( "Aligning to next 16-byte boundary of " << size << "." << endl );
  }

  // Determine the involved processor core.
  WIR_System &sys = f.getCompilationUnit().getSystem();
  const WIR_Section &sec = sys.findSymbol( f ).getSection();
  RV32I &rv = dynamic_cast<RV32I &>( sec.getProcessor() );

  auto &stackPointer = rv.SP();

  // Convert list insertedSpillCode into a set of IDs for convenience.
  set<WIR_id_t> spillCode;
  for ( WIR_Instruction &i : insertedSpillCode )
    spillCode.insert( i.getID() );

  // Determine the first non-empty instruction and operation of f.
  WIR_Instruction *iid = nullptr;

  for ( WIR_BasicBlock &b : f ) {
    for ( WIR_Instruction &i : b )
      if ( !i.getOperations().empty() ) {
        iid = &i;
        break;
      }

    if ( iid != nullptr )
      break;
  }

  if ( iid == nullptr )
    return;

  set<WIR_id_t> sacIDs;
  auto &firstIns = *iid;
  WIR_Operation &oldSAC = firstIns.getOperations().front();

  // Test if f already has some stack-allocating code (SAC) and adjust it.
  if ( isStackPointerADDI( oldSAC ) ) {
    DOUT( "Adjusting ADDI stack-allocating code." << endl );

    auto pos = oldSAC.getParameters().begin();
    ++pos;
    //TODO CADDI16SP if( oldSAC.getOpCode() == RV32I::OpCode::ADDI ){
      ++pos;

      int oldStackFrameSize =
        dynamic_cast<RV_Const12_Signed &>( pos->get() ).getValue();

      oldSAC.replaceParameter(
        pos, RV_Const12_Signed( oldStackFrameSize - size ) );
    //TODO CADDI16SP
    // } else if( oldSAC.getOpCode() == RV32IC::OpCode::CADDI16SP ){
    //   // CADDI16SP has the offset as it's second parameter
    //   // and multiplies the given value by 16

    //   int oldStackFrameSize =
    //     dynamic_cast<RV_Const6_Signed &>( pos->get() ).getValue();

    //   oldSAC.replaceParameter(
    //     pos, RV_Const6_Signed( oldStackFrameSize - (size / 16) ) );
    // } else {
    //   ufAssertT( false,
    //     "Unsupported stack pointer modification operation." );
    // }

    sacIDs.insert( oldSAC.getID() );
  } else {
    // No SAC.
    DOUT( "Inserting LEA stack-allocating code entry." <<  (-size)/16 << endl );
    auto &leai1 =
      f.getBasicBlocks().front().get().insertInstruction(
        f.getBasicBlocks().front().get().begin(), {} )->get();
    //TODO CADDI16SP
    // leai1.pushFrontOperation(
    //   { RV32IC::OpCode::CADDI16SP, RV32IC::OperationFormat::SRC6_4,
    //     WIR_RegisterParameter( stackPointer, WIR_Usage::defuse ),
    //     RV_Const6_Signed( (int)(-size) ) } );
    leai1.pushBackOperation(
      { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
        WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
        WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
        RV_Const12_Signed( (int)(-size) ) } );

    sacIDs.insert( leai1.getOperations().front().get().getID() );
    DOUT( "Inserting LEA stack-allocating code exit." <<  leai1 << endl );
  }

  // Adjust all old accesses to the stack.
  for ( WIR_BasicBlock &b : f )
    for ( auto it = b.begin(); it != b.end(); ++it ) {
      WIR_Instruction &i = it->get();

      // Adjust stack access only if it's not a regular spill instruction.
      // TODO: Here, things become tricky. According to the RISC-V ABI, the
      //       lowest addresses of a function's stack frame are reserved for
      //       argument passing. The additional stack space required for
      //       spilling thus must, in general, not lie at stack pointer offset
      //       0. In the TriCore reference implementation of the register
      //       allocator, we however assumed that the spilling area is reserved
      //       at the bottom of a stack frame, which no longer works for RISC-V.
      //       If instead, we allocate the spilling area at the top of a
      //       function's stack frame, then SP offsets 0 still refer to the
      //       argument area, directly followed by the native stack frame of the
      //       function, finally followed by the spilling area. Thus, all the
      //       original stack accesses using the argument area of the native
      //       stack frame need not to be adjusted at all! The RISC-V register
      //       allocator should thus strive to use large offsets during spill
      //       code generation, which then has significant impact on the code
      //       below (can be removed to large parts?) stemming from TriCore.
      // TODO: Carefully rethink all this, both for TriCore and for RISC-V! The
      //       TriCore EABI also says that outgoing function arguments (i.e.,
      //       the so-called overflow area) must be at the bottom of the stack
      //       frame at offset 0. Any this layout also seems to apply to RISC-V.

      // Check whether the current instruction contains parameters that are
      // marked as don't touch since they access the overflow area.
      bool isOverflowAccess = false;
      for ( WIR_Operation &o : i )
        for ( WIR_Parameter &p : o )
          if ( p.getDontOptimize() ) {
            isOverflowAccess = true;
            p.setDontOptimize( false );
          }

      // Adjust stack access only if it's not an access to the overflow region
      // or a regular spill instruction.
      if ( !isOverflowAccess && !spillCode.count( i.getID() ) &&
           !i.getOperations().empty() ) {
        WIR_Operation &o = i.getOperations().front().get();

        // Checks for the instruction that initializes the new frame pointer
        bool fpAdj = ( o.getOpCode() == RV32I::OpCode::ADDI ) &&
                     ( o.getOperationFormat() == RV32IC::OperationFormat::RRC12_1 ) &&
                     (dynamic_cast<WIR_RegisterParameter &>( o.getExplicitParameter( 1 ) ).getRegister().getName() == "x8" ) &&
                     isSP( dynamic_cast<WIR_RegisterParameter &>( o.getExplicitParameter( 2 ) ).getRegister() ) ;

        if ( !( o.isMemoryAccess() || o.isMemoryLoad() || o.isMemoryStore() || fpAdj ) )
          continue;

        // Don't adjust the original operation for stack frame allocation again!
        if ( sacIDs.count( o.getID() ) )
          continue;

        auto &p = o.getExplicitParameters();

        // This lambda is used to obtain an unsigned constant argument value.
        auto getReg = [&]( unsigned int i ) -> WIR_BaseRegister & {
          return(
            dynamic_cast<WIR_RegisterParameter &>(
              o.getExplicitParameter( i ) ).getRegister() );
        };
        // The operation formats of load and store store instructions and the instruction to set the new fp need to be checked.
        bool rc12r_1 = ( o.getOperationFormat() == RV32I::OperationFormat::RC12R_1 ) && isSP( getReg( 3 ) );
        bool rc12r_2 = ( o.getOperationFormat() == RV32I::OperationFormat::RC12R_2 ) && isSP( getReg( 3 ) );
        bool src5r_1 = ( o.getOperationFormat() == RV32IC::OperationFormat::SRC5R_1 ) && isSP( getReg( 3 ) );
        bool src5r_2 = ( o.getOperationFormat() == RV32IC::OperationFormat::SRC5R_2 ) && isSP( getReg( 3 ) );
        bool src6r_1 = ( o.getOperationFormat() == RV32IC::OperationFormat::SRC6R_1 ) && isSP( getReg( 3 ) );
        bool src6r_2 = ( o.getOperationFormat() == RV32IC::OperationFormat::SRC6R_2 ) && isSP( getReg( 3 ) );
        bool rlr_1 = ( o.getOperationFormat() == RV32IC::OperationFormat::RLR_1 ) && isSP( getReg( 3 ) );
        bool rlr_2 = ( o.getOperationFormat() == RV32IC::OperationFormat::RLR_2 ) && isSP( getReg( 3 ) );

        if ( rc12r_1 || rc12r_2 || src5r_1 || src5r_2 || src6r_1 || src6r_2 || rlr_1 || rlr_2 || fpAdj) {
          DOUT( "Adjusting stack access in " << o << endl );

          // Determine the immediate parameter and current stack offset.
          auto pos = p.end();
          for ( auto pit = p.begin(); pit != p.end(); ++pit )
            if ( pit->get().getType() == WIR_ParameterType::imm ) {
              pos = pit;
              break;
            }
          auto &ip = dynamic_cast<WIR_BaseImmediateParameter &>( pos->get() );
          long long currentOffset =
            ip.isSigned() ? ip.getSignedValue() : ip.getUnsignedValue();
          // TODO: Add proper handling of C.LW, C.LWSP, C.SW, C.SWSP?
          if ( currentOffset >= 0 ) {
            long long newOffset = currentOffset + size;

            if ( ( o.getOpCode() == RV32I::OpCode::LW )  &&
                 ( newOffset > RV_Const12_Signed::getMinValue( 12 ) ) &&
                 ( newOffset <= RV_Const12_Signed::getMaxValue( 12 ) ) ) {
              DOUT( "Replacing LW." << endl );

              // Replace LW offset parameter
              i.replaceOperation( i.getOperations().begin(),
                { RV32I::OpCode::LW, RV32I::OperationFormat::RC12R_1,
                  WIR_RegisterParameter(
                    dynamic_cast<WIR_RegisterParameter &>( p.front().get() ) ),
                  RV_Const12_Signed( newOffset ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ) } );
            } else if ( newOffset > RV_Const12_Signed::getMaxValue( 12 ) ) {
            // The new offset does not fit into a 12 bits immediate.
            DOUT(
              "Generating two ADDIs before/after the current operation." <<
              endl );

            // TODO: The following manipulation of SP violates the RISC-V ABI
            //       which states that "Procedures must not rely upon the
            //       persistence of stack-allocated data whose addresses lie
            //       below the stack pointer." (RISC-V ABI, Section 2.1, Page
            //       6). In the future, some different register needs to be
            //       identified and used here.
            //
            //       RV_GraphColoring::isAdjustedLoadOrStoreInstruction needs
            //       to be rewritten in accordance with changes here since it
            //       checks for exactly this case.

            // TODO: What has to happen below if size itself does not fit into
            //       the 12 bits immediates for the two ADDIs?
            b.insertInstruction( it,
              { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
                  WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  RV_Const12_Signed( newOffset ) } } );
            b.insertInstruction( std::next( it ),
              { { RV32I::OpCode::ADDI, RV32I::OperationFormat::RRC12_1,
                  WIR_RegisterParameter( stackPointer, WIR_Usage::def ),
                  WIR_RegisterParameter( stackPointer, WIR_Usage::use ),
                  RV_Const12_Signed( -newOffset ) } } );
            } else {
              DOUT( "Replacing stack offset parameter. " << newOffset << endl );

              // The adjusted SP-relative offset is small enough to still fit
              // into the current operation.
              dynamic_cast<RV_Const12_Signed &>( ip ).setValue( newOffset );
            }
          }
        }
      }
    }
};


/*
  isStackPointerADDI checks wether the given operation is an ADDI operation that
  modifies the stack pointer.
*/
bool RV32I::isStackPointerADDI( const WIR_Operation &o )
{
  DSTART( BOOST_CURRENT_FUNCTION );
  return( (
    ( ( o.getOpCode() == RV32I::OpCode::ADDI ) &&
    ( o.getOperationFormat() == RV32I::OperationFormat::RRC12_1 ) )
    //TODO CADDI16SP
    // || ( ( o.getOpCode() == RV32IC::OpCode::CADDI16SP ) &&
    // ( o.getOperationFormat() == RV32IC::OperationFormat::SRC6_4 ) )
    ) && isSP(
      dynamic_cast<WIR_RegisterParameter &>(
        o.getExplicitParameter( 1 ) ).getRegister() ) );
};


//
// Private class methods
//

/*
  clone creates a copy of an RV32I processor.
*/
WIR_BaseProcessor *RV32I::clone( void ) const
{
  DSTART( "virtual WIR_BaseProcessor* RV32I::clone()" );

  return( new RV32I( *this ) );
};

}       // namespace WIR
