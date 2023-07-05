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

  WIR_Function f( "foo" );
  WIR_BasicBlock b;
  WIR_CompilationUnit c1;
  WIR_TaskManager t;
  WIR_System sys( "genericmips.sys", t );
  auto &c = sys.getComponents<WIR_BaseProcessor>().begin()->get();

  ufAssert( sys.getSymbols().empty() );

  f.pushBackBasicBlock( b );
  f.pushBackBasicBlock( WIR_BasicBlock() );
  c1.pushBackFunction( f );
  auto &cu = sys.pushBackCompilationUnit( c1 );
  WIR_Data data3( "d3" );
  data3.pushBackInitData( WIR_DataInit( WIR_DataInitType::ibyte, "42" ) );

  auto d1 = cu.pushFrontData( WIR_Data( "d1" ) ).getID();
  auto d2 = cu.pushFrontData( WIR_Data( "d2" ) ).getID();
  auto d3 = cu.pushBackData( data3 ).getID();

  auto &symd1 = sys.findSymbol( d1 );
  auto &symd2 = sys.findSymbol( d2 );
  auto &symd3 = sys.findSymbol( d3 );

  symd2.setConst();

  auto &fun =
    sys.getCompilationUnits().front().get().getFunctions().front().get();
  auto &bb1 = fun.getBasicBlocks().front().get();
  auto &bb2 = fun.getBasicBlocks().back().get();

  auto &symf = sys.findSymbol( fun );
  auto &symbb1 = sys.findSymbol( bb1 );
  auto &symbb2 = sys.findSymbol( bb2 );

  // Check default section assignment.
  ufAssert( symd1.getSection() == c.getBssSection() );
  ufAssert( symd2.getSection() == c.getRODataSection() );
  ufAssert( symd3.getSection() == c.getDataSection() );
  ufAssert( symf.getSection() == c.getTextSection() );
  ufAssert( symbb1.getSection() == c.getTextSection() );
  ufAssert( symbb2.getSection() == c.getTextSection() );

  // Test section assignment of data objects.
  symd1.setSection( *(c.findSection( ".init" )) );
  ufAssert( symd1.getSection() != c.getDataSection() );
  ufAssert( symd1.getSection() == *(c.findSection( ".init" )) );
  symd1.unsetSection();
  ufAssert( symd1.getSection() == c.getBssSection() );

  // Test section assignment of functions.
  symf.setSection( *(c.findSection( ".ctors" )) );
  ufAssert( symf.getSection() != c.getTextSection() );
  ufAssert( symf.getSection() == *(c.findSection( ".ctors" )) );
  ufAssert( symbb1.getSection() == *(c.findSection( ".ctors" )) );
  ufAssert( symbb2.getSection() == *(c.findSection( ".ctors" )) );
  symf.unsetSection();
  ufAssert( symf.getSection() == c.getTextSection() );
  ufAssert( symbb1.getSection() == c.getTextSection() );
  ufAssert( symbb1.getSection() == c.getTextSection() );

  // Test section assignment of basic blocks.
  symbb1.setSection( *(c.findSection( ".inttab" )) );
  symbb2.setSection( *(c.findSection( ".dtors" )) );
  ufAssert( symf.getSection() == *(c.findSection( ".inttab" )) );
  ufAssert( symbb1.getSection() != c.getTextSection() );
  ufAssert( symbb1.getSection() != c.getTextSection() );
  ufAssert( symbb1.getSection() == *(c.findSection( ".inttab" )) );
  ufAssert( symbb2.getSection() == *(c.findSection( ".dtors" )) );
  symbb1.unsetSection();
  ufAssert( symf.getSection() == c.getTextSection() );
  ufAssert( symbb1.getSection() == c.getTextSection() );
  ufAssert( symbb2.getSection() == *(c.findSection( ".dtors" )) );
  symbb2.unsetSection();
  ufAssert( symf.getSection() == c.getTextSection() );
  ufAssert( symbb1.getSection() == c.getTextSection() );
  ufAssert( symbb2.getSection() == c.getTextSection() );

  // Test reset of function-level assignment during basic block assignment.
  symf.setSection( *(c.findSection( ".ctors" )) );
  symbb2.setSection( *(c.findSection( ".dtors" )) );
  ufAssert( symf.getSection() == c.getTextSection() );
  ufAssert( symbb2.getSection() == *(c.findSection( ".dtors" )) );

  // Test rest of block-level assignment during function assignment.
  symf.setSection( *(c.findSection( ".inttab" )) );
  ufAssert( symf.getSection() == *(c.findSection( ".inttab" )) );
  ufAssert( symbb1.getSection() == *(c.findSection( ".inttab" )) );
  ufAssert( symbb2.getSection() == *(c.findSection( ".inttab" )) );

  return( 0 );
}
