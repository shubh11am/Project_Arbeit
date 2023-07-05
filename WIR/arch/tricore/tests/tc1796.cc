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
#include <arch/tricore/tc13.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();
  WIR_TaskManager t;
  WIR_System sys( "tc1796.sys", t );

  ufAssert( sys.getComponents<WIR_BaseProcessor>().size() == 1 );
  ufAssert( sys.getComponents<WIR_MemoryRegion>().size() == 14 );

  auto &p = sys.getComponents<TC13>().begin()->get();
  ufAssert( p.getName() == "CORE0" );
  ufAssert( p.getISAName() == "TC1.3" );
  ufAssert( p.getClockFrequency() == 150000000 );
  ufAssert( p.getVoltage() == 1.5f );
  ufAssert( p.isInserted() );
  ufAssert( p.getSystem() == sys );

  // Test physical registers of a system's core.
  ufAssert( p.A0() == p.getPhRegs().at( 0 ) );
  ufAssert( p.A0().isInserted() );
  ufAssert( p.A0().getProcessor() == p );

  ufAssert( p.A1() == p.getPhRegs().at( 1 ) );
  ufAssert( p.A1().isInserted() );
  ufAssert( p.A1().getProcessor() == p );

  ufAssert( p.A2() == p.getPhRegs().at( 2 ) );
  ufAssert( p.A2().isInserted() );
  ufAssert( p.A2().getProcessor() == p );

  ufAssert( p.A3() == p.getPhRegs().at( 3 ) );
  ufAssert( p.A3().isInserted() );
  ufAssert( p.A3().getProcessor() == p );

  ufAssert( p.A4() == p.getPhRegs().at( 4 ) );
  ufAssert( p.A4().isInserted() );
  ufAssert( p.A4().getProcessor() == p );

  ufAssert( p.A5() == p.getPhRegs().at( 5 ) );
  ufAssert( p.A5().isInserted() );
  ufAssert( p.A5().getProcessor() == p );

  ufAssert( p.A6() == p.getPhRegs().at( 6 ) );
  ufAssert( p.A6().isInserted() );
  ufAssert( p.A6().getProcessor() == p );

  ufAssert( p.A7() == p.getPhRegs().at( 7 ) );
  ufAssert( p.A7().isInserted() );
  ufAssert( p.A7().getProcessor() == p );

  ufAssert( p.A8() == p.getPhRegs().at( 8 ) );
  ufAssert( p.A8().isInserted() );
  ufAssert( p.A8().getProcessor() == p );

  ufAssert( p.A9() == p.getPhRegs().at( 9 ) );
  ufAssert( p.A9().isInserted() );
  ufAssert( p.A9().getProcessor() == p );

  ufAssert( p.A10() == p.getPhRegs().at( 10 ) );
  ufAssert( p.A10().isInserted() );
  ufAssert( p.A10().getProcessor() == p );

  ufAssert( p.A11() == p.getPhRegs().at( 11 ) );
  ufAssert( p.A11().isInserted() );
  ufAssert( p.A11().getProcessor() == p );

  ufAssert( p.A12() == p.getPhRegs().at( 12 ) );
  ufAssert( p.A12().isInserted() );
  ufAssert( p.A12().getProcessor() == p );

  ufAssert( p.A13() == p.getPhRegs().at( 13 ) );
  ufAssert( p.A13().isInserted() );
  ufAssert( p.A13().getProcessor() == p );

  ufAssert( p.A14() == p.getPhRegs().at( 14 ) );
  ufAssert( p.A14().isInserted() );
  ufAssert( p.A14().getProcessor() == p );

  ufAssert( p.A15() == p.getPhRegs().at( 15 ) );
  ufAssert( p.A15().isInserted() );
  ufAssert( p.A15().getProcessor() == p );

  ufAssert( p.D0() == p.getPhRegs().at( 16 ) );
  ufAssert( p.D0().isInserted() );
  ufAssert( p.D0().getProcessor() == p );

  ufAssert( p.D1() == p.getPhRegs().at( 17 ) );
  ufAssert( p.D1().isInserted() );
  ufAssert( p.D1().getProcessor() == p );

  ufAssert( p.D2() == p.getPhRegs().at( 18 ) );
  ufAssert( p.D2().isInserted() );
  ufAssert( p.D2().getProcessor() == p );

  ufAssert( p.D3() == p.getPhRegs().at( 19 ) );
  ufAssert( p.D3().isInserted() );
  ufAssert( p.D3().getProcessor() == p );

  ufAssert( p.D4() == p.getPhRegs().at( 20 ) );
  ufAssert( p.D4().isInserted() );
  ufAssert( p.D4().getProcessor() == p );

  ufAssert( p.D5() == p.getPhRegs().at( 21 ) );
  ufAssert( p.D5().isInserted() );
  ufAssert( p.D5().getProcessor() == p );

  ufAssert( p.D6() == p.getPhRegs().at( 22 ) );
  ufAssert( p.D6().isInserted() );
  ufAssert( p.D6().getProcessor() == p );

  ufAssert( p.D7() == p.getPhRegs().at( 23 ) );
  ufAssert( p.D7().isInserted() );
  ufAssert( p.D7().getProcessor() == p );

  ufAssert( p.D8() == p.getPhRegs().at( 24 ) );
  ufAssert( p.D8().isInserted() );
  ufAssert( p.D8().getProcessor() == p );

  ufAssert( p.D9() == p.getPhRegs().at( 25 ) );
  ufAssert( p.D9().isInserted() );
  ufAssert( p.D9().getProcessor() == p );

  ufAssert( p.D10() == p.getPhRegs().at( 26 ) );
  ufAssert( p.D10().isInserted() );
  ufAssert( p.D10().getProcessor() == p );

  ufAssert( p.D11() == p.getPhRegs().at( 27 ) );
  ufAssert( p.D11().isInserted() );
  ufAssert( p.D11().getProcessor() == p );

  ufAssert( p.D12() == p.getPhRegs().at( 28 ) );
  ufAssert( p.D12().isInserted() );
  ufAssert( p.D12().getProcessor() == p );

  ufAssert( p.D13() == p.getPhRegs().at( 29 ) );
  ufAssert( p.D13().isInserted() );
  ufAssert( p.D13().getProcessor() == p );

  ufAssert( p.D14() == p.getPhRegs().at( 30 ) );
  ufAssert( p.D14().isInserted() );
  ufAssert( p.D14().getProcessor() == p );

  ufAssert( p.D15() == p.getPhRegs().at( 31 ) );
  ufAssert( p.D15().isInserted() );
  ufAssert( p.D15().getProcessor() == p );

  ufAssert( p.E0() == p.getPhRegs().at( 32 ) );
  ufAssert( p.E0().isInserted() );
  ufAssert( p.E0().getProcessor() == p );

  ufAssert( p.E2() == p.getPhRegs().at( 33 ) );
  ufAssert( p.E2().isInserted() );
  ufAssert( p.E2().getProcessor() == p );

  ufAssert( p.E4() == p.getPhRegs().at( 34 ) );
  ufAssert( p.E4().isInserted() );
  ufAssert( p.E4().getProcessor() == p );

  ufAssert( p.E6() == p.getPhRegs().at( 35 ) );
  ufAssert( p.E6().isInserted() );
  ufAssert( p.E6().getProcessor() == p );

  ufAssert( p.E8() == p.getPhRegs().at( 36 ) );
  ufAssert( p.E8().isInserted() );
  ufAssert( p.E8().getProcessor() == p );

  ufAssert( p.E10() == p.getPhRegs().at( 37 ) );
  ufAssert( p.E10().isInserted() );
  ufAssert( p.E10().getProcessor() == p );

  ufAssert( p.E12() == p.getPhRegs().at( 38 ) );
  ufAssert( p.E12().isInserted() );
  ufAssert( p.E12().getProcessor() == p );

  ufAssert( p.E14() == p.getPhRegs().at( 39 ) );
  ufAssert( p.E14().isInserted() );
  ufAssert( p.E14().getProcessor() == p );

  ufAssert( p.P0() == p.getPhRegs().at( 40 ) );
  ufAssert( p.P0().isInserted() );
  ufAssert( p.P0().getProcessor() == p );

  ufAssert( p.P2() == p.getPhRegs().at( 41 ) );
  ufAssert( p.P2().isInserted() );
  ufAssert( p.P2().getProcessor() == p );

  ufAssert( p.P4() == p.getPhRegs().at( 42 ) );
  ufAssert( p.P4().isInserted() );
  ufAssert( p.P4().getProcessor() == p );

  ufAssert( p.P6() == p.getPhRegs().at( 43 ) );
  ufAssert( p.P6().isInserted() );
  ufAssert( p.P6().getProcessor() == p );

  ufAssert( p.P8() == p.getPhRegs().at( 44 ) );
  ufAssert( p.P8().isInserted() );
  ufAssert( p.P8().getProcessor() == p );

  ufAssert( p.P10() == p.getPhRegs().at( 45 ) );
  ufAssert( p.P10().isInserted() );
  ufAssert( p.P10().getProcessor() == p );

  ufAssert( p.P12() == p.getPhRegs().at( 46 ) );
  ufAssert( p.P12().isInserted() );
  ufAssert( p.P12().getProcessor() == p );

  ufAssert( p.P14() == p.getPhRegs().at( 47 ) );
  ufAssert( p.P14().isInserted() );
  ufAssert( p.P14().getProcessor() == p );

  auto &c = dynamic_cast<WIR_Cache &>( sys.findComponent( "L1-I" )->get() );
  ufAssert( c.getCacheType() == WIR_Cache::CacheType::I );
  ufAssert( c.getLevel() == WIR_Cache::CacheLevel::L1 );
  ufAssert( c.getSize() == 16384 );
  ufAssert( c.getAssociativity() == 2 );
  ufAssert( c.getLineSize() == 32 );
  ufAssert( c.getNumberOfSets() == 256 );
  ufAssert( c.getOffsetBits() == 5 );
  ufAssert( c.getIndexBits() == 8 );
  ufAssert( c.getTagBits() == 19 );
  ufAssert( c.isEnabled() == true );
  ufAssert( c.isShared() == false );
  ufAssert( c.isInserted() );
  ufAssert( c.getSystem() == sys );
  ufAssert(
    c.isActiveInRange( WIR_AddressRange { 0x80100000, 0x801fffff } ) );
  ufAssert( c.getAddressRanges().getIntervalCount() == 1 );
  ufAssert(
    c.getAddressRanges().getIntervals().front() ==
      WIR_AddressRange(
        WIR_MemoryAddress( 0x80100000 ), WIR_MemoryAddress( 0x801fffff ) ) );

  ufAssert( sys.findComponent( "CORE0" ) != sys.getComponents().end() );

  auto &m =
    dynamic_cast<WIR_MemoryRegion &>( sys.findComponent( "PFLASH-C" )->get() );
  ufAssert( m.getBaseAddress() == 0x80100000 );
  ufAssert( m.getLength() == 0x100000 );
  ufAssert( m.getAttributes() == 0x2d );
  ufAssert( m.getMinDelay() == 6 );
  ufAssert( m.getMaxDelay() == 6 );
  ufAssert( m.getHierarchy().size() == 1 );
  ufAssert( m.getHierarchy().begin()->size() == 2 );
  ufAssert( m.getHierarchy().begin()->front().get() == p );
  ufAssert( m.getHierarchy().begin()->back().get() == c );
  ufAssert( m.isInserted() );
  ufAssert( m.getSystem() == sys );
  ufAssert( m.getAddressRanges().isEmpty() );

  ufAssert( sys.findComponent( "BOOT" ) != sys.getComponents().end() );
  ufAssert(
    sys.findComponent( "PFLASH-STARTUP" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PFLASH-NC" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DFLASH" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DMU-SRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DMU-SBRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DMI-SP" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DMI-LDRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DMI-DPRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PMI-SRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PMI-SYS" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PCP-DATA" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PCP-TEXT" ) != sys.getComponents().end() );

  return( 0 );
}
