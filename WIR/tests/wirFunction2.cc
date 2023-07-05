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

  // Check precolors.

  MIPS m;
  WIR_Function f( "main" );
  auto & tr1 = f.pushBackVirtualRegister( MIPS_RegV() );
  auto & tr2 = f.pushBackVirtualRegister( MIPS_RegV() );

  MIPS_RegV &r1 = static_cast<MIPS_RegV &>( tr1 );
  MIPS_RegV &r2 = static_cast<MIPS_RegV &>( tr2 );
  MIPS_RegV r3;

  f.insertPrecolor( r2, m.r0() );

  ufAssert( !f.containsPrecolor( r1 ) );
  ufAssert( f.containsPrecolor( r2 ) );
  ufAssert( !f.containsPrecolor( r3 ) );
  ufAssert( f.findPrecolor( r2 ) == m.r0() );

  f.insertPrecolor( r1, m.r1() );
  f.insertPrecolor( r2, m.r2() );

  ufAssert( f.containsPrecolor( r1 ) );
  ufAssert( f.containsPrecolor( r2 ) );
  ufAssert( !f.containsPrecolor( r3 ) );
  ufAssert( f.findPrecolor( r1 ) == m.r1() );
  ufAssert( f.findPrecolor( r2 ) == m.r2() );

  f.erasePrecolor( r1 );

  ufAssert( !f.containsPrecolor( r1 ) );
  ufAssert( f.containsPrecolor( r2 ) );
  ufAssert( !f.containsPrecolor( r3 ) );
  ufAssert( f.findPrecolor( r2 ) == m.r2() );

  f.erasePrecolor( r1 );
  f.erasePrecolor( r3 );

  ufAssert( !f.containsPrecolor( r1 ) );
  ufAssert( f.containsPrecolor( r2 ) );
  ufAssert( !f.containsPrecolor( r3 ) );
  ufAssert( f.findPrecolor( r2 ) == m.r2() );

  f.insertPrecolor( r1, m.r3() );

  ufAssert( f.containsPrecolor( r1 ) );
  ufAssert( f.containsPrecolor( r2 ) );
  ufAssert( !f.containsPrecolor( r3 ) );
  ufAssert( f.findPrecolor( r1 ) == m.r3() );
  ufAssert( f.findPrecolor( r2 ) == m.r2() );

  f.clearPrecolors();

  ufAssert( !f.containsPrecolor( r1 ) );
  ufAssert( !f.containsPrecolor( r2 ) );
  ufAssert( !f.containsPrecolor( r3 ) );

  // Test precolors and copy constructor.
  f.insertPrecolor( r1, m.r4() );
  f.insertPrecolor( r2, m.r5() );
  WIR_Function f1( f );

  MIPS_RegV &r4 =
    static_cast<MIPS_RegV &>( f1.getVirtualRegisters().front().get() );
  MIPS_RegV &r5 =
    static_cast<MIPS_RegV &>( f1.getVirtualRegisters().back().get() );
  ufAssert( r1 != r4 );
  ufAssert( r2 != r5 );
  ufAssert( f.findPrecolor( r1 ) == f1.findPrecolor( r4 ) );
  ufAssert( f.findPrecolor( r2 ) == f1.findPrecolor( r5 ) );

  // Test precolors and assignment operator.
  WIR_Function f2( "foo" );
  f2 = f;

  MIPS_RegV &r6 =
    static_cast<MIPS_RegV &>( f2.getVirtualRegisters().front().get() );
  MIPS_RegV &r7 =
    static_cast<MIPS_RegV &>( f2.getVirtualRegisters().back().get() );
  ufAssert( r1 != r6 );
  ufAssert( r2 != r7 );
  ufAssert( f.findPrecolor( r1 ) == f2.findPrecolor( r6 ) );
  ufAssert( f.findPrecolor( r2 ) == f2.findPrecolor( r7 ) );

  return( 0 );
}
