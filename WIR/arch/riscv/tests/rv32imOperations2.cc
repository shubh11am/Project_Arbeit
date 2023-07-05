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


//
// Include section
//

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32im.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV32IM p;
  const RV_RegP &x1 = p.x1(), &x2 = p.x2(), &x3 = p.x3();

  WIR_Function f( "main" );

  cout.iword( WIR_Indentation() ) = 8;
  cout << riscv << comment;

  // This lambda serves for generating a RISC-V operation, adding a comment to
  // it and finally dumping it.
  auto rvop = []( WIR_Operation &&o ) {
    o.insertContainer(
      WIR_Comment( "Operation Format: " + o.getOperationFormat().getName() ) );
    cout << o << endl;
  };

  // The following operations must be accepted according to the RISC-V RV32IM
  // ISA.
  rvop(
    { RV32IM::OpCode::DIV, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IM::OpCode::DIVU, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IM::OpCode::MUL, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IM::OpCode::MULH, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IM::OpCode::MULHSU, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IM::OpCode::MULHU, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IM::OpCode::REM, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  rvop(
    { RV32IM::OpCode::REMU, RV32IM::OperationFormat::RRR_1,
      new WIR_RegisterParameter( x1, WIR_Usage::def ),
      new WIR_RegisterParameter( x2, WIR_Usage::use ),
      new WIR_RegisterParameter( x3, WIR_Usage::use ) } );

  return( 0 );
}
