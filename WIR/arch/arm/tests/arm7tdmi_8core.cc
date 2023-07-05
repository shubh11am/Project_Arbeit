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
  WIR_System sys( "arm7tdmi_8core.sys", t );

  ufAssert( sys.getComponents<WIR_BaseProcessor>().size() == 8 );

  auto &p1 = sys.getComponents<ARMv4T>().begin()->get();
  ufAssert( p1.getName() == "CORE0" );
  ufAssert( p1.getISAName() == "ARMv4T" );
  ufAssert( p1.getClockFrequency() == 200000000 );
  ufAssert( p1.getVoltage() == 3.3f );
  ufAssert( p1.isInserted() );
  ufAssert( p1.getSystem() == sys );

  auto &p2 = sys.getComponents<ARMv4T>().rbegin()->get();
  ufAssert( p2.getName() == "CORE7" );
  ufAssert( p2.getISAName() == "ARMv4T" );
  ufAssert( p2.getClockFrequency() == 200000000 );
  ufAssert( p2.getVoltage() == 3.3f );
  ufAssert( p2.isInserted() );
  ufAssert( p2.getSystem() == sys );

  return( 0 );
}
