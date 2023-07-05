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

  WIR_CompilationUnit &c = sys.pushBackCompilationUnit( {} );
  WIR_Function &f = c.pushBackFunction( WIR_Function( "main" ) );
  WIR_BasicBlock &b1 = f.pushBackBasicBlock( {} );

  // String to be rejected by the parser.
  vector<unique_ptr<TC_AsmArgument>> dummy;
  TC_AsmParser parser;
  parser.run( "foobar %a0, %d4", dummy, b1, "tcAsmParser4.cc" );

  return( 0 );
}
