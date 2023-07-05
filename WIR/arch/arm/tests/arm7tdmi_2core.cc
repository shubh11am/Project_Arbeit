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
  WIR_System sys( "arm7tdmi_2core.sys", t );

  ufAssert( sys.getComponents<WIR_BaseProcessor>().size() == 2 );
  ufAssert( sys.getComponents<WIR_MemoryRegion>().size() == 19 );

  auto &p1 = sys.getComponents<ARMv4T>().begin()->get();
  ufAssert( p1.getName() == "CORE0" );
  ufAssert( p1.getISAName() == "ARMv4T" );
  ufAssert( p1.getClockFrequency() == 200000000 );
  ufAssert( p1.getVoltage() == 3.3f );
  ufAssert( p1.isInserted() );
  ufAssert( p1.getSystem() == sys );

  // Test physical registers of a system's core.
  ufAssert( p1.R0() == p1.getPhRegs().at( 0 ) );
  ufAssert( p1.R0().isInserted() );
  ufAssert( p1.R0().getProcessor() == p1 );

  ufAssert( p1.R1() == p1.getPhRegs().at( 1 ) );
  ufAssert( p1.R1().isInserted() );
  ufAssert( p1.R1().getProcessor() == p1 );

  ufAssert( p1.R2() == p1.getPhRegs().at( 2 ) );
  ufAssert( p1.R2().isInserted() );
  ufAssert( p1.R2().getProcessor() == p1 );

  ufAssert( p1.R3() == p1.getPhRegs().at( 3 ) );
  ufAssert( p1.R3().isInserted() );
  ufAssert( p1.R3().getProcessor() == p1 );

  ufAssert( p1.R4() == p1.getPhRegs().at( 4 ) );
  ufAssert( p1.R4().isInserted() );
  ufAssert( p1.R4().getProcessor() == p1 );

  ufAssert( p1.R5() == p1.getPhRegs().at( 5 ) );
  ufAssert( p1.R5().isInserted() );
  ufAssert( p1.R5().getProcessor() == p1 );

  ufAssert( p1.R6() == p1.getPhRegs().at( 6 ) );
  ufAssert( p1.R6().isInserted() );
  ufAssert( p1.R6().getProcessor() == p1 );

  ufAssert( p1.R7() == p1.getPhRegs().at( 7 ) );
  ufAssert( p1.R7().isInserted() );
  ufAssert( p1.R7().getProcessor() == p1 );

  ufAssert( p1.R8() == p1.getPhRegs().at( 8 ) );
  ufAssert( p1.R8().isInserted() );
  ufAssert( p1.R8().getProcessor() == p1 );

  ufAssert( p1.R9() == p1.getPhRegs().at( 9 ) );
  ufAssert( p1.R9().isInserted() );
  ufAssert( p1.R9().getProcessor() == p1 );

  ufAssert( p1.R10() == p1.getPhRegs().at( 10 ) );
  ufAssert( p1.R10().isInserted() );
  ufAssert( p1.R10().getProcessor() == p1 );

  ufAssert( p1.R11() == p1.getPhRegs().at( 11 ) );
  ufAssert( p1.R11().isInserted() );
  ufAssert( p1.R11().getProcessor() == p1 );

  ufAssert( p1.R12() == p1.getPhRegs().at( 12 ) );
  ufAssert( p1.R12().isInserted() );
  ufAssert( p1.R12().getProcessor() == p1 );

  ufAssert( p1.R13() == p1.getPhRegs().at( 13 ) );
  ufAssert( p1.R13().isInserted() );
  ufAssert( p1.R13().getProcessor() == p1 );

  ufAssert( p1.R14() == p1.getPhRegs().at( 14 ) );
  ufAssert( p1.R14().isInserted() );
  ufAssert( p1.R14().getProcessor() == p1 );

  ufAssert( p1.R15() == p1.getPhRegs().at( 15 ) );
  ufAssert( p1.R15().isInserted() );
  ufAssert( p1.R15().getProcessor() == p1 );

  auto &p2 = sys.getComponents<ARMv4T>().rbegin()->get();
  ufAssert( p1 != p2 );
  ufAssert( p2.getName() == "CORE1" );
  ufAssert( p2.getISAName() == "ARMv4T" );
  ufAssert( p2.getClockFrequency() == 200000000 );
  ufAssert( p2.getVoltage() == 3.3f );
  ufAssert( p2.isInserted() );
  ufAssert( p2.getSystem() == sys );

  // Test physical registers of a system's core.
  ufAssert( p2.R0() == p2.getPhRegs().at( 0 ) );
  ufAssert( p2.R0().isInserted() );
  ufAssert( p2.R0().getProcessor() == p2 );

  ufAssert( p2.R1() == p2.getPhRegs().at( 1 ) );
  ufAssert( p2.R1().isInserted() );
  ufAssert( p2.R1().getProcessor() == p2 );

  ufAssert( p2.R2() == p2.getPhRegs().at( 2 ) );
  ufAssert( p2.R2().isInserted() );
  ufAssert( p2.R2().getProcessor() == p2 );

  ufAssert( p2.R3() == p2.getPhRegs().at( 3 ) );
  ufAssert( p2.R3().isInserted() );
  ufAssert( p2.R3().getProcessor() == p2 );

  ufAssert( p2.R4() == p2.getPhRegs().at( 4 ) );
  ufAssert( p2.R4().isInserted() );
  ufAssert( p2.R4().getProcessor() == p2 );

  ufAssert( p2.R5() == p2.getPhRegs().at( 5 ) );
  ufAssert( p2.R5().isInserted() );
  ufAssert( p2.R5().getProcessor() == p2 );

  ufAssert( p2.R6() == p2.getPhRegs().at( 6 ) );
  ufAssert( p2.R6().isInserted() );
  ufAssert( p2.R6().getProcessor() == p2 );

  ufAssert( p2.R7() == p2.getPhRegs().at( 7 ) );
  ufAssert( p2.R7().isInserted() );
  ufAssert( p2.R7().getProcessor() == p2 );

  ufAssert( p2.R8() == p2.getPhRegs().at( 8 ) );
  ufAssert( p2.R8().isInserted() );
  ufAssert( p2.R8().getProcessor() == p2 );

  ufAssert( p2.R9() == p2.getPhRegs().at( 9 ) );
  ufAssert( p2.R9().isInserted() );
  ufAssert( p2.R9().getProcessor() == p2 );

  ufAssert( p2.R10() == p2.getPhRegs().at( 10 ) );
  ufAssert( p2.R10().isInserted() );
  ufAssert( p2.R10().getProcessor() == p2 );

  ufAssert( p2.R11() == p2.getPhRegs().at( 11 ) );
  ufAssert( p2.R11().isInserted() );
  ufAssert( p2.R11().getProcessor() == p2 );

  ufAssert( p2.R12() == p2.getPhRegs().at( 12 ) );
  ufAssert( p2.R12().isInserted() );
  ufAssert( p2.R12().getProcessor() == p2 );

  ufAssert( p2.R13() == p2.getPhRegs().at( 13 ) );
  ufAssert( p2.R13().isInserted() );
  ufAssert( p2.R13().getProcessor() == p2 );

  ufAssert( p2.R14() == p2.getPhRegs().at( 14 ) );
  ufAssert( p2.R14().isInserted() );
  ufAssert( p2.R14().getProcessor() == p2 );

  ufAssert( p2.R15() == p2.getPhRegs().at( 15 ) );
  ufAssert( p2.R15().isInserted() );
  ufAssert( p2.R15().getProcessor() == p2 );

  auto &c1 = dynamic_cast<WIR_Cache &>( sys.findComponent( "L1-I0" )->get() );
  ufAssert( c1.getCacheType() == WIR_Cache::CacheType::I );
  ufAssert( c1.isShared() == false );
  ufAssert( c1.getLevel() == WIR_Cache::CacheLevel::L1 );
  ufAssert( c1.isInserted() );
  ufAssert( c1.getSystem() == sys );

  auto &c2 = dynamic_cast<WIR_Cache &>( sys.findComponent( "L1-I1" )->get() );
  ufAssert( c2.getCacheType() == WIR_Cache::CacheType::I );
  ufAssert( c2.isShared() == false );
  ufAssert( c2.getLevel() == WIR_Cache::CacheLevel::L1 );
  ufAssert( c2.isInserted() );
  ufAssert( c2.getSystem() == sys );

  auto &c3 = dynamic_cast<WIR_Cache &>( sys.findComponent( "L1-D0" )->get() );
  ufAssert( c3.getCacheType() == WIR_Cache::CacheType::D );
  ufAssert( c3.isShared() == false );
  ufAssert( c3.getLevel() == WIR_Cache::CacheLevel::L1 );
  ufAssert( c3.isInserted() );
  ufAssert( c3.getSystem() == sys );

  auto &c4 = dynamic_cast<WIR_Cache &>( sys.findComponent( "L1-D1" )->get() );
  ufAssert( c4.getCacheType() == WIR_Cache::CacheType::D );
  ufAssert( c4.isShared() == false );
  ufAssert( c4.getLevel() == WIR_Cache::CacheLevel::L1 );
  ufAssert( c4.isInserted() );
  ufAssert( c4.getSystem() == sys );

  auto &c5 = dynamic_cast<WIR_Cache &>( sys.findComponent( "L2-I" )->get() );
  ufAssert( c5.getCacheType() == WIR_Cache::CacheType::I );
  ufAssert( c5.isShared() == true );
  ufAssert( c5.getLevel() == WIR_Cache::CacheLevel::L2 );
  ufAssert( c5.isInserted() );
  ufAssert( c5.getSystem() == sys );

  auto &c6 = dynamic_cast<WIR_Cache &>( sys.findComponent( "L2-D" )->get() );
  ufAssert( c6.getCacheType() == WIR_Cache::CacheType::D );
  ufAssert( c6.isShared() == true );
  ufAssert( c6.getLevel() == WIR_Cache::CacheLevel::L2 );
  ufAssert( c6.isInserted() );
  ufAssert( c6.getSystem() == sys );

  ufAssert( sys.findComponent( "BOOT0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "BOOT1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "I-SRAM-NC0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "I-SRAM-NC1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "I-SRAM-C0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "I-SRAM-C1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "I-SP0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "I-SP1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SRAM-NC0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SRAM-NC1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SRAM-C0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SRAM-C1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SP0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SP1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SP-STACK0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "D-SP-STACK1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "FLASH-NC0" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "FLASH-NC1" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "GLOBAL_DATA" ) != sys.getComponents().end() );

  return( 0 );
}
