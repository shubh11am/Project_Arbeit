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

  ufAssert( sys.getComponents<WIR_BaseProcessor>().size() == 1 );
  ufAssert( sys.getComponents<WIR_MemoryRegion>().size() == 1 );

  auto &p = sys.getComponents<MIPS>().begin()->get();
  ufAssert( p.getName() == "CORE0" );
  ufAssert( p.getISAName() == "WIR generic" );
  ufAssert( p.getClockFrequency() == 150000000 );
  ufAssert( p.getVoltage() == 3.3f );
  ufAssert( p.isInserted() );
  ufAssert( p.getSystem() == sys );

  // Test physical registers of a system's core.
  ufAssert( p.r0() == p.getPhRegs().at( 0 ) );
  ufAssert( p.r0().isInserted() );
  ufAssert( p.r0().getProcessor() == p );

  ufAssert( p.r1() == p.getPhRegs().at( 1 ) );
  ufAssert( p.r1().isInserted() );
  ufAssert( p.r1().getProcessor() == p );

  ufAssert( p.r2() == p.getPhRegs().at( 2 ) );
  ufAssert( p.r2().isInserted() );
  ufAssert( p.r2().getProcessor() == p );

  ufAssert( p.r3() == p.getPhRegs().at( 3 ) );
  ufAssert( p.r3().isInserted() );
  ufAssert( p.r3().getProcessor() == p );

  ufAssert( p.r4() == p.getPhRegs().at( 4 ) );
  ufAssert( p.r4().isInserted() );
  ufAssert( p.r4().getProcessor() == p );

  ufAssert( p.r5() == p.getPhRegs().at( 5 ) );
  ufAssert( p.r5().isInserted() );
  ufAssert( p.r5().getProcessor() == p );

  ufAssert( p.r6() == p.getPhRegs().at( 6 ) );
  ufAssert( p.r6().isInserted() );
  ufAssert( p.r6().getProcessor() == p );

  ufAssert( p.r7() == p.getPhRegs().at( 7 ) );
  ufAssert( p.r7().isInserted() );
  ufAssert( p.r7().getProcessor() == p );

  ufAssert( p.r8() == p.getPhRegs().at( 8 ) );
  ufAssert( p.r8().isInserted() );
  ufAssert( p.r8().getProcessor() == p );

  ufAssert( p.r9() == p.getPhRegs().at( 9 ) );
  ufAssert( p.r9().isInserted() );
  ufAssert( p.r9().getProcessor() == p );

  ufAssert( p.r10() == p.getPhRegs().at( 10 ) );
  ufAssert( p.r10().isInserted() );
  ufAssert( p.r10().getProcessor() == p );

  ufAssert( p.r11() == p.getPhRegs().at( 11 ) );
  ufAssert( p.r11().isInserted() );
  ufAssert( p.r11().getProcessor() == p );

  ufAssert( p.r12() == p.getPhRegs().at( 12 ) );
  ufAssert( p.r12().isInserted() );
  ufAssert( p.r12().getProcessor() == p );

  ufAssert( p.r13() == p.getPhRegs().at( 13 ) );
  ufAssert( p.r13().isInserted() );
  ufAssert( p.r13().getProcessor() == p );

  ufAssert( p.r14() == p.getPhRegs().at( 14 ) );
  ufAssert( p.r14().isInserted() );
  ufAssert( p.r14().getProcessor() == p );

  ufAssert( p.r15() == p.getPhRegs().at( 15 ) );
  ufAssert( p.r15().isInserted() );
  ufAssert( p.r15().getProcessor() == p );

  ufAssert( p.r16() == p.getPhRegs().at( 16 ) );
  ufAssert( p.r16().isInserted() );
  ufAssert( p.r16().getProcessor() == p );

  ufAssert( p.r17() == p.getPhRegs().at( 17 ) );
  ufAssert( p.r17().isInserted() );
  ufAssert( p.r17().getProcessor() == p );

  ufAssert( p.r18() == p.getPhRegs().at( 18 ) );
  ufAssert( p.r18().isInserted() );
  ufAssert( p.r18().getProcessor() == p );

  ufAssert( p.r19() == p.getPhRegs().at( 19 ) );
  ufAssert( p.r19().isInserted() );
  ufAssert( p.r19().getProcessor() == p );

  ufAssert( p.r20() == p.getPhRegs().at( 20 ) );
  ufAssert( p.r20().isInserted() );
  ufAssert( p.r20().getProcessor() == p );

  ufAssert( p.r21() == p.getPhRegs().at( 21 ) );
  ufAssert( p.r21().isInserted() );
  ufAssert( p.r21().getProcessor() == p );

  ufAssert( p.r22() == p.getPhRegs().at( 22 ) );
  ufAssert( p.r22().isInserted() );
  ufAssert( p.r22().getProcessor() == p );

  ufAssert( p.r23() == p.getPhRegs().at( 23 ) );
  ufAssert( p.r23().isInserted() );
  ufAssert( p.r23().getProcessor() == p );

  ufAssert( p.r24() == p.getPhRegs().at( 24 ) );
  ufAssert( p.r24().isInserted() );
  ufAssert( p.r24().getProcessor() == p );

  ufAssert( p.r25() == p.getPhRegs().at( 25 ) );
  ufAssert( p.r25().isInserted() );
  ufAssert( p.r25().getProcessor() == p );

  ufAssert( p.r26() == p.getPhRegs().at( 26 ) );
  ufAssert( p.r26().isInserted() );
  ufAssert( p.r26().getProcessor() == p );

  ufAssert( p.r27() == p.getPhRegs().at( 27 ) );
  ufAssert( p.r27().isInserted() );
  ufAssert( p.r27().getProcessor() == p );

  ufAssert( p.r28() == p.getPhRegs().at( 28 ) );
  ufAssert( p.r28().isInserted() );
  ufAssert( p.r28().getProcessor() == p );

  ufAssert( p.r29() == p.getPhRegs().at( 29 ) );
  ufAssert( p.r29().isInserted() );
  ufAssert( p.r29().getProcessor() == p );

  ufAssert( p.r30() == p.getPhRegs().at( 30 ) );
  ufAssert( p.r30().isInserted() );
  ufAssert( p.r30().getProcessor() == p );

  ufAssert( p.r31() == p.getPhRegs().at( 31 ) );
  ufAssert( p.r31().isInserted() );
  ufAssert( p.r31().getProcessor() == p );

  auto &m = sys.getComponents<WIR_MemoryRegion>().begin()->get();
  ufAssert( m.getBaseAddress() == 0 );
  ufAssert( m.getLength() == 0x100000 );
  ufAssert( m.getAttributes() == 15 );
  ufAssert( m.getMinDelay() == 1 );
  ufAssert( m.getMaxDelay() == 1 );
  ufAssert( m.getHierarchy().size() == 1 );
  ufAssert( m.getHierarchy().begin()->size() == 1 );
  ufAssert( m.getHierarchy().begin()->front().get() == p );
  ufAssert( m.isInserted() );
  ufAssert( m.getSystem() == sys );
  ufAssert( m.getAddressRanges().getIntervalCount() == 0 );

  return( 0 );
}
