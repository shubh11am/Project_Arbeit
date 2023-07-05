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
#include <arch/tricore/tc131.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();
  WIR_TaskManager t;
  WIR_System sys( "tc1797.sys", t );

  ufAssert( sys.getComponents<WIR_BaseProcessor>().size() == 1 );
  ufAssert( sys.getComponents<WIR_MemoryRegion>().size() == 12 );

  auto &p = sys.getComponents<TC131>().begin()->get();
  ufAssert( p.getName() == "CORE0" );
  ufAssert( p.getISAName() == "TC1.3.1" );
  ufAssert( p.getClockFrequency() == 150000000 );
  ufAssert( p.getVoltage() == 1.5f );
  ufAssert( p.isInserted() );
  ufAssert( p.getSystem() == sys );

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
  ufAssert( m.getMinBurstDelay() == 1 );
  ufAssert( m.getMaxBurstDelay() == 1 );
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
  ufAssert( sys.findComponent( "DMI-CSA" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DMI-SP" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "DMI-LDRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PMI-SRAM" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PMI-SYS" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PCP-DATA" ) != sys.getComponents().end() );
  ufAssert( sys.findComponent( "PCP-TEXT" ) != sys.getComponents().end() );

  return( 0 );
}
