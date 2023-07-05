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

  WIR_CompilationUnit c;

  WIR_Data d( "d1" );
  d.setSize( 42 );
  d.pushBackInitData( WIR_DataInit( WIR_DataInitType::ibyte, "b1" ) );
  d.pushBackInitData( WIR_DataInit( WIR_DataInitType::ihword, "h1" ) );
  d.pushBackInitData( WIR_DataInit( 10 ) );
  d.pushBackInitData( WIR_DataInit( 100 ) );
  d.pushBackInitData( WIR_DataInit( WIR_DataInitType::iword, "w1" ) );

  auto &d1 = c.pushBackData( d );
  c.pushBackData( WIR_Data( "d2" ) );

  d1.setDontOptimize();
  d1.clearInitData();

  return( 0 );
}
