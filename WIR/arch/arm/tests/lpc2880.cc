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
#include <arch/arm/armv4t.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();
  WIR_TaskManager t;
  WIR_System sys( "lpc2880.sys", t );

  ufAssert( sys.getComponents<WIR_BaseProcessor>().size() == 1 );
  ufAssert( sys.getComponents<WIR_MemoryRegion>().size() == 7 );

  auto &p = sys.getComponents<ARMv4T>().begin()->get();
  ufAssert( p.getName() == "CORE0" );
  ufAssert( p.getISAName() == "ARMv4T" );
  ufAssert( p.getClockFrequency() == 150000000 );
  ufAssert( p.getVoltage() == 3.3f );
  ufAssert( p.isInserted() );
  ufAssert( p.getSystem() == sys );

  // Test physical registers of a system's core.
  ufAssert( p.R0() == p.getPhRegs().at( 0 ) );
  ufAssert( p.R0().isInserted() );
  ufAssert( p.R0().getProcessor() == p );

  ufAssert( p.R1() == p.getPhRegs().at( 1 ) );
  ufAssert( p.R1().isInserted() );
  ufAssert( p.R1().getProcessor() == p );

  ufAssert( p.R2() == p.getPhRegs().at( 2 ) );
  ufAssert( p.R2().isInserted() );
  ufAssert( p.R2().getProcessor() == p );

  ufAssert( p.R3() == p.getPhRegs().at( 3 ) );
  ufAssert( p.R3().isInserted() );
  ufAssert( p.R3().getProcessor() == p );

  ufAssert( p.R4() == p.getPhRegs().at( 4 ) );
  ufAssert( p.R4().isInserted() );
  ufAssert( p.R4().getProcessor() == p );

  ufAssert( p.R5() == p.getPhRegs().at( 5 ) );
  ufAssert( p.R5().isInserted() );
  ufAssert( p.R5().getProcessor() == p );

  ufAssert( p.R6() == p.getPhRegs().at( 6 ) );
  ufAssert( p.R6().isInserted() );
  ufAssert( p.R6().getProcessor() == p );

  ufAssert( p.R7() == p.getPhRegs().at( 7 ) );
  ufAssert( p.R7().isInserted() );
  ufAssert( p.R7().getProcessor() == p );

  ufAssert( p.R8() == p.getPhRegs().at( 8 ) );
  ufAssert( p.R8().isInserted() );
  ufAssert( p.R8().getProcessor() == p );

  ufAssert( p.R9() == p.getPhRegs().at( 9 ) );
  ufAssert( p.R9().isInserted() );
  ufAssert( p.R9().getProcessor() == p );

  ufAssert( p.R10() == p.getPhRegs().at( 10 ) );
  ufAssert( p.R10().isInserted() );
  ufAssert( p.R10().getProcessor() == p );

  ufAssert( p.R11() == p.getPhRegs().at( 11 ) );
  ufAssert( p.R11().isInserted() );
  ufAssert( p.R11().getProcessor() == p );

  ufAssert( p.R12() == p.getPhRegs().at( 12 ) );
  ufAssert( p.R12().isInserted() );
  ufAssert( p.R12().getProcessor() == p );

  ufAssert( p.R13() == p.getPhRegs().at( 13 ) );
  ufAssert( p.R13().isInserted() );
  ufAssert( p.R13().getProcessor() == p );

  ufAssert( p.R14() == p.getPhRegs().at( 14 ) );
  ufAssert( p.R14().isInserted() );
  ufAssert( p.R14().getProcessor() == p );

  ufAssert( p.R15() == p.getPhRegs().at( 15 ) );
  ufAssert( p.R15().isInserted() );
  ufAssert( p.R15().getProcessor() == p );

  auto &c = dynamic_cast<WIR_Cache &>( sys.findComponent( "L1-I" )->get() );
  ufAssert( c.getCacheType() == WIR_Cache::CacheType::I );
  ufAssert( c.getLevel() == WIR_Cache::CacheLevel::L1 );
  ufAssert( c.isEnabled() == false );
  ufAssert( c.isShared() == false );
  ufAssert( c.isWriteThrough() == true );
  ufAssert( c.isWriteAllocate() == true );
  ufAssert( c.getBusWidth() == 4 );
  ufAssert( c.isInserted() );
  ufAssert( c.getSystem() == sys );

  ufAssert( sys.findComponent( "CORE0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "BOOT" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "SRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "CODE-SRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "CSI-SP" ) != sys.getComponents().end() );

  auto &m =
    dynamic_cast<WIR_MemoryRegion &>( sys.findComponent( "FLASH-C" )->get() );
  ufAssert( m.getBaseAddress() == 0x500000 );
  ufAssert( m.getLength() == 0x100000 );
  ufAssert( m.getAttributes() == 0x2d );
  ufAssert( m.getBusWidth() == 32 );
  ufAssert( m.getMinDelay() == 6 );
  ufAssert( m.getMaxDelay() == 6 );
  ufAssert( m.getHierarchy().size() == 1 );
  ufAssert( m.getHierarchy().begin()->size() == 2 );
  ufAssert( m.getHierarchy().begin()->front().get() == p );
  ufAssert( m.getHierarchy().begin()->back().get() == c );
  ufAssert( m.isInserted() );
  ufAssert( m.getSystem() == sys );
  ufAssert( m.getAddressRanges().getIntervalCount() == 0 );

  ufAssert( sys.findComponent( "FLASH-NC" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DFLASH-NC" ) != sys.getComponents().end() );

  return( 0 );
}
