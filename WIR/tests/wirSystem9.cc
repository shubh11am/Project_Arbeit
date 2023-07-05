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
  WIR_CompilationUnit &c = sys.pushBackCompilationUnit( {} );

  WIR_Data d( "d1" );
  d.setSize( 42 );
  d.pushBackInitData( WIR_DataInit( WIR_DataInitType::ibyte, "b1" ) );
  d.pushBackInitData( WIR_DataInit( WIR_DataInitType::ihword, "h1" ) );
  d.pushBackInitData( WIR_DataInit( 10 ) );
  d.pushBackInitData( WIR_DataInit( 100 ) );
  d.pushBackInitData( WIR_DataInit( WIR_DataInitType::iword, "w1" ) );

  auto &d1 = c.pushBackData( d );
  auto &d2 = c.pushBackData( WIR_Data( "d2" ) );

  ufAssert( c.getData().front().get() == d1 );
  ufAssert( c.getData().back().get() == d2 );
  ufAssert( d1.getName() == "d1" );
  ufAssert( d1.getSize() == 42 );
  ufAssert( d1.isInitialized() );
  ufAssert( d1.getInitData().size() == 5 );

  auto it = d1.begin();
  ufAssert( it->get().getType() == WIR_DataInitType::ibyte );
  ufAssert( it->get().getValues().front() == "b1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ihword );
  ufAssert( it->get().getValues().front() == "h1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 10 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 100 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::iword );
  ufAssert( it->get().getValues().front() == "w1" );

  ufAssert( d2.getName() == "d2" );
  ufAssert( d2.getSize() == 0 );
  ufAssert( !d2.isInitialized() );
  ufAssert( d2.getInitData().size() == 0 );

  // Test the copy constructors.
  WIR_System sys1( sys );
  auto &c1 = sys1.getCompilationUnits().front().get();

  ufAssert( c1.getData().front().get() != d1 );
  ufAssert( c1.getData().back().get() != d2 );
  auto &d3 = c1.getData().front().get();
  auto &d4 = c1.getData().back().get();
  ufAssert( d3.getName() == "d1" );
  ufAssert( d3.getSize() == 42 );
  ufAssert( d3.isInitialized() );
  ufAssert( d3.getInitData().size() == 5 );

  it = d3.begin();
  ufAssert( it->get().getType() == WIR_DataInitType::ibyte );
  ufAssert( it->get().getValues().front() == "b1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ihword );
  ufAssert( it->get().getValues().front() == "h1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 10 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 100 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::iword );
  ufAssert( it->get().getValues().front() == "w1" );

  ufAssert( d4.getName() == "d2" );
  ufAssert( d4.getSize() == 0 );
  ufAssert( !d4.isInitialized() );
  ufAssert( d4.getInitData().size() == 0 );

  // Test the move constructors.
  WIR_System sys2( move( sys ) );
  auto &c2 = sys2.getCompilationUnits().front().get();

  auto &d5 = c2.getData().front().get();
  auto &d6 = c2.getData().back().get();

  ufAssert( d5.getCompilationUnit() == c2 );
  ufAssert( d6.getCompilationUnit() == c2 );
  ufAssert( d5.getName() == "d1" );
  ufAssert( d5.getSize() == 42 );
  ufAssert( d5.isInitialized() );
  ufAssert( d5.getInitData().size() == 5 );

  it = d5.begin();
  ufAssert( it->get().getType() == WIR_DataInitType::ibyte );
  ufAssert( it->get().getValues().front() == "b1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ihword );
  ufAssert( it->get().getValues().front() == "h1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 10 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 100 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::iword );
  ufAssert( it->get().getValues().front() == "w1" );

  ufAssert( d6.getName() == "d2" );
  ufAssert( d6.getSize() == 0 );
  ufAssert( !d6.isInitialized() );
  ufAssert( d6.getInitData().size() == 0 );

  // Test the copy assignment operator.
  WIR_System sys3( "genericmips.sys", t );
  sys3 = sys1;
  auto &c3 = sys3.getCompilationUnits().front().get();

  auto &d7 = c3.getData().front().get();
  auto &d8 = c3.getData().back().get();
  ufAssert( d7.getName() == "d1" );
  ufAssert( d7.getSize() == 42 );
  ufAssert( d7.isInitialized() );
  ufAssert( d7.getInitData().size() == 5 );

  it = d7.begin();
  ufAssert( it->get().getType() == WIR_DataInitType::ibyte );
  ufAssert( it->get().getValues().front() == "b1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ihword );
  ufAssert( it->get().getValues().front() == "h1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 10 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 100 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::iword );
  ufAssert( it->get().getValues().front() == "w1" );

  ufAssert( d8.getName() == "d2" );
  ufAssert( d8.getSize() == 0 );
  ufAssert( !d8.isInitialized() );
  ufAssert( d8.getInitData().size() == 0 );

  // Test the move assignment operator.
  WIR_System sys4( "genericmips.sys", t );
  sys4 = move( sys1 );
  auto &c4 = sys4.getCompilationUnits().front().get();

  auto &d9 = c4.getData().front().get();
  auto &d10 = c4.getData().back().get();

  ufAssert( d9.getCompilationUnit() == c4 );
  ufAssert( d10.getCompilationUnit() == c4 );
  ufAssert( d9.getName() == "d1" );
  ufAssert( d9.getSize() == 42 );
  ufAssert( d9.isInitialized() );
  ufAssert( d9.getInitData().size() == 5 );

  it = d9.begin();
  ufAssert( it->get().getType() == WIR_DataInitType::ibyte );
  ufAssert( it->get().getValues().front() == "b1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ihword );
  ufAssert( it->get().getValues().front() == "h1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 10 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 100 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::iword );
  ufAssert( it->get().getValues().front() == "w1" );

  ufAssert( d10.getName() == "d2" );
  ufAssert( d10.getSize() == 0 );
  ufAssert( !d10.isInitialized() );
  ufAssert( d10.getInitData().size() == 0 );

  d9.foldSpaces();
  ufAssert( d9.isInitialized() );
  ufAssert( d9.getInitData().size() == 4 );

  it = d9.begin();
  ufAssert( it->get().getType() == WIR_DataInitType::ibyte );
  ufAssert( it->get().getValues().front() == "b1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ihword );
  ufAssert( it->get().getValues().front() == "h1" );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::ispace );
  ufAssert( it->get().getSpace() == 110 );
  ++it;
  ufAssert( it->get().getType() == WIR_DataInitType::iword );
  ufAssert( it->get().getValues().front() == "w1" );

  d9.clearInitData();
  ufAssert( !d9.isInitialized() );
  ufAssert( d9.getInitData().size() == 0 );

  return( 0 );
}
