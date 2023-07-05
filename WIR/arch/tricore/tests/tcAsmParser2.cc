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
#include <sstream>
#include <string>

// Include WIR headers
#include <wir/wir.h>
#include <arch/tricore/tc131.h>
#include <arch/tricore/asmparser/tcasmargument.h>
#include <arch/tricore/asmparser/tcasmparser.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  WIR_TaskManager t;
  WIR_System sys( "tc1797.sys", t );
  auto &p = sys.getComponents<TC131>().begin()->get();

  WIR_CompilationUnit &c = sys.pushBackCompilationUnit( {} );
  WIR_Function &f = c.pushBackFunction( WIR_Function( "main" ) );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );

  stringstream sstr;
  sstr.iword( WIR_Indentation() ) = 8;
  sstr << tricore;

  // This lambda serves for inserting a TriCore operation and adding a comment
  // to it.
  auto tcop = [&]( WIR_Operation &&o ) {
    auto &op = b1.pushBackInstruction( WIR_Instruction( WIR_Operation( o ) ) );
    op.insertContainer(
      WIR_Comment( "Operation Format: " + o.getOperationFormat().getName() ) );
  };

  // The following physical data registers must be accepted by the parser.
  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D0(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D1(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D2(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D3(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D4(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D5(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D6(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D7(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D8(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D9(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D10(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D11(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D12(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D13(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::ABS, TC131::OperationFormat::DD,
      new WIR_RegisterParameter( p.D14(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ) } );

  // The following physical address registers must be accepted by the parser.
  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A0(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A1(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A2(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A3(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A4(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A5(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A6(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A7(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A8(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A9(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A10(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A11(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A12(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A13(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::MOV_AA, TC131::OperationFormat::AA,
      new WIR_RegisterParameter( p.A14(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.A15(), WIR_Usage::use ) } );

  // The following physical extended data registers must be accepted by the
  // parser.
  tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( p.E0(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.E2(), WIR_Usage::use ),
      new WIR_RegisterParameter( p.D15(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( p.E4(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.E6(), WIR_Usage::use ),
      new WIR_RegisterParameter( p.D14(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( p.E8(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.E10(), WIR_Usage::use ),
      new WIR_RegisterParameter( p.D13(), WIR_Usage::use ) } );

  tcop(
    { TC131::OpCode::DVADJ, TC131::OperationFormat::EED,
      new WIR_RegisterParameter( p.E12(), WIR_Usage::def ),
      new WIR_RegisterParameter( p.E14(), WIR_Usage::use ),
      new WIR_RegisterParameter( p.D12(), WIR_Usage::use ) } );

  // The following physical extended address registers must be accepted by the
  // parser.
  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P0(), WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P2(), WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P4(), WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P6(), WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P8(), WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P10(), WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P12(), WIR_Usage::defuse ) } );

  tcop(
    { TC131::OpCode::CACHEA_I, TC131::OperationFormat::PBRA,
      new WIR_RegisterParameter( p.P14(), WIR_Usage::defuse ) } );

  // Create string to be parsed, avoid .section directive here.
  sstr << b1;
  sstr.str( string() );
  sstr << b1;

  // Parse the TriCore assembly string.
  vector<unique_ptr<TC_AsmArgument>> dummy;
  TC_AsmParser parser;
  parser.run( sstr.str(), dummy, b1, "tcAsmParser2.cc" );

  // bIt1 points to the original basic block to be parsed.
  auto bIt1 = f.begin();

  // iIt1 points to an original TriCore instruction to be parsed.
  auto iIt1 = bIt1->get().begin();

  // bIt2 points to the first basic block created by the parser.
  auto bIt2 = bIt1;
  ++bIt2;

  // Iterate over all basic blocks created by the parser.
  for ( ; bIt2 != f.end(); ++bIt2 )
    // Iterate over all TriCore instructions in lockstep, both original one and
    // the one created by the parser.
    for ( auto iIt2 = bIt2->get().begin(); iIt2 != bIt2->get().end();
          ++iIt2, ++iIt1 ) {
      // Perform pairwise comparison of the current two instructions.
      ufAssert(
        iIt2->get().getOperations().size() ==
          iIt1->get().getOperations().size() );

      auto oIt1 = iIt1->get().begin();
      auto oIt2 = iIt2->get().begin();

      // Iterate over all TriCore operations in lockstep.
      for ( ; oIt2 != iIt2->get().end(); ++oIt2, ++oIt1 ) {
        // Perform pairwise comparison of the current two operations.
        ufAssert( oIt2->get().getOpCode() == oIt1->get().getOpCode() );
        ufAssert(
          oIt2->get().getOperationFormat() ==
            oIt1->get().getOperationFormat() );
        ufAssert(
          oIt2->get().getParameters().size() ==
            oIt1->get().getParameters().size() );

        auto pIt1 = oIt1->get().begin();
        auto pIt2 = oIt2->get().begin();

        // Iterate over all TriCore parameters in lockstep.
        for ( ; pIt2 != oIt2->get().end(); ++pIt2, ++pIt1 ) {
          WIR_Parameter &p1 = pIt1->get();
          WIR_Parameter &p2 = pIt2->get();

          ufAssert( p2.getType() == p1.getType() );

          switch( p2.getType() ) {

            case WIR_ParameterType::addr: {
              ufAssert(
                dynamic_cast<WIR_AddressingModeParameter &>(
                  p2 ).getAddressingMode() ==
                dynamic_cast<WIR_AddressingModeParameter &>(
                  p1 ).getAddressingMode() );
              break;
            }

            case WIR_ParameterType::imm: {
              auto &ip1 = dynamic_cast<WIR_BaseImmediateParameter &>( p1 );
              auto &ip2 = dynamic_cast<WIR_BaseImmediateParameter &>( p2 );

              ufAssert( ip2.isSigned() == ip1.isSigned() );
              if ( ip2.isSigned() )
                ufAssert( ip2.getSignedValue() == ip1.getSignedValue() );
              else
                ufAssert( ip2.getUnsignedValue() == ip1.getUnsignedValue() );
              break;
            }

            case WIR_ParameterType::label: {
              auto &lp1 = dynamic_cast<WIR_LabelParameter &>( p1 );
              auto &lp2 = dynamic_cast<WIR_LabelParameter &>( p2 );

              ufAssert( lp2.getLabelType() == lp1.getLabelType() );

              switch ( lp2.getLabelType() ) {
                case WIR_SymbolType::block: {
                  // Block labels need not be equal after parsing so that we
                  // simply don't do any checks here.
                  break;
                }

                case WIR_SymbolType::data: {
                  ufAssert( lp2.getData() == lp1.getData() );
                  break;
                }

                case WIR_SymbolType::function: {
                  ufAssert( lp2.getFunction() == lp1.getFunction() );
                  break;
                }
              }

              break;
            }

            case WIR_ParameterType::reg: {
              ufAssert(
                dynamic_cast<WIR_RegisterParameter &>( p2 ).getRegister() ==
                dynamic_cast<WIR_RegisterParameter &>( p1 ).getRegister() );
              break;
            }

            default: {
              ufAssertT(
                false,
                "Parameter type detected that should not occur for TriCore." );
              break;
            }
          }
        }
      }
    }

  return( 0 );
}
