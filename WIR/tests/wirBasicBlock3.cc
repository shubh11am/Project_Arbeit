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
#include <string>

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

  // Check names of basic blocks.

  WIR_Function f( "bar" );
  WIR_BasicBlock b1, b2;

  string n1 = b1.getName(), n2 = b2.getName();
  ufAssert( n1 != f.getName() );
  ufAssert( n2 != f.getName() );

  WIR_BasicBlock &b3 = f.pushBackBasicBlock( move( b1 ) );
  WIR_BasicBlock &b4 = f.pushBackBasicBlock( move( b2 ) );

  ufAssert( b3.getName() == f.getName() );
  ufAssert( b3.getName() != n1 );
  ufAssert( b4.getName() != f.getName() );
  ufAssert( b4.getName() == n2 );

  f.setName( "foobar" );

  ufAssert( b3.getName() == f.getName() );
  ufAssert( b3.getName() != n1 );
  ufAssert( b3.getName() != "main" );
  ufAssert( b4.getName() != f.getName() );
  ufAssert( b4.getName() == n2 );

  return( 0 );
}
