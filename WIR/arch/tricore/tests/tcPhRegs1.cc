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
#include <arch/tricore/tc131.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  TC131 p1;

  // Check names of all physical registers.
  ufAssert( p1.A0().getName() == "a0" );
  ufAssert( p1.A1().getName() == "a1" );
  ufAssert( p1.A2().getName() == "a2" );
  ufAssert( p1.A3().getName() == "a3" );
  ufAssert( p1.A4().getName() == "a4" );
  ufAssert( p1.A5().getName() == "a5" );
  ufAssert( p1.A6().getName() == "a6" );
  ufAssert( p1.A7().getName() == "a7" );
  ufAssert( p1.A8().getName() == "a8" );
  ufAssert( p1.A9().getName() == "a9" );
  ufAssert( p1.A10().getName() == "a10" );
  ufAssert( p1.A11().getName() == "a11" );
  ufAssert( p1.A12().getName() == "a12" );
  ufAssert( p1.A13().getName() == "a13" );
  ufAssert( p1.A14().getName() == "a14" );
  ufAssert( p1.A15().getName() == "a15" );

  ufAssert( p1.SP().getName() == "a10" );
  ufAssert( p1.RA().getName() == "a11" );

  ufAssert( p1.D0().getName() == "d0" );
  ufAssert( p1.D1().getName() == "d1" );
  ufAssert( p1.D2().getName() == "d2" );
  ufAssert( p1.D3().getName() == "d3" );
  ufAssert( p1.D4().getName() == "d4" );
  ufAssert( p1.D5().getName() == "d5" );
  ufAssert( p1.D6().getName() == "d6" );
  ufAssert( p1.D7().getName() == "d7" );
  ufAssert( p1.D8().getName() == "d8" );
  ufAssert( p1.D9().getName() == "d9" );
  ufAssert( p1.D10().getName() == "d10" );
  ufAssert( p1.D11().getName() == "d11" );
  ufAssert( p1.D12().getName() == "d12" );
  ufAssert( p1.D13().getName() == "d13" );
  ufAssert( p1.D14().getName() == "d14" );
  ufAssert( p1.D15().getName() == "d15" );

  ufAssert( p1.E0().getName() == "e0" );
  ufAssert( p1.E2().getName() == "e2" );
  ufAssert( p1.E4().getName() == "e4" );
  ufAssert( p1.E6().getName() == "e6" );
  ufAssert( p1.E8().getName() == "e8" );
  ufAssert( p1.E10().getName() == "e10" );
  ufAssert( p1.E12().getName() == "e12" );
  ufAssert( p1.E14().getName() == "e14" );

  ufAssert( p1.P0().getName() == "p0" );
  ufAssert( p1.P2().getName() == "p2" );
  ufAssert( p1.P4().getName() == "p4" );
  ufAssert( p1.P6().getName() == "p6" );
  ufAssert( p1.P8().getName() == "p8" );
  ufAssert( p1.P10().getName() == "p10" );
  ufAssert( p1.P12().getName() == "p12" );
  ufAssert( p1.P14().getName() == "p14" );

  ufAssert( p1.PSW_C().getName() == "PSW.C" );

  // Check virtual/physical property.
  ufAssert( p1.A0().isPhysical() );
  ufAssert( p1.A1().isPhysical() );
  ufAssert( p1.A2().isPhysical() );
  ufAssert( p1.A3().isPhysical() );
  ufAssert( p1.A4().isPhysical() );
  ufAssert( p1.A5().isPhysical() );
  ufAssert( p1.A6().isPhysical() );
  ufAssert( p1.A7().isPhysical() );
  ufAssert( p1.A8().isPhysical() );
  ufAssert( p1.A9().isPhysical() );
  ufAssert( p1.A10().isPhysical() );
  ufAssert( p1.A11().isPhysical() );
  ufAssert( p1.A12().isPhysical() );
  ufAssert( p1.A13().isPhysical() );
  ufAssert( p1.A14().isPhysical() );
  ufAssert( p1.A15().isPhysical() );
  ufAssert( p1.D0().isPhysical() );
  ufAssert( p1.D1().isPhysical() );
  ufAssert( p1.D2().isPhysical() );
  ufAssert( p1.D3().isPhysical() );
  ufAssert( p1.D4().isPhysical() );
  ufAssert( p1.D5().isPhysical() );
  ufAssert( p1.D6().isPhysical() );
  ufAssert( p1.D7().isPhysical() );
  ufAssert( p1.D8().isPhysical() );
  ufAssert( p1.D9().isPhysical() );
  ufAssert( p1.D10().isPhysical() );
  ufAssert( p1.D11().isPhysical() );
  ufAssert( p1.D12().isPhysical() );
  ufAssert( p1.D13().isPhysical() );
  ufAssert( p1.D14().isPhysical() );
  ufAssert( p1.D15().isPhysical() );
  ufAssert( p1.E0().isPhysical() );
  ufAssert( p1.E2().isPhysical() );
  ufAssert( p1.E4().isPhysical() );
  ufAssert( p1.E6().isPhysical() );
  ufAssert( p1.E8().isPhysical() );
  ufAssert( p1.E10().isPhysical() );
  ufAssert( p1.E12().isPhysical() );
  ufAssert( p1.E14().isPhysical() );
  ufAssert( p1.P0().isPhysical() );
  ufAssert( p1.P2().isPhysical() );
  ufAssert( p1.P4().isPhysical() );
  ufAssert( p1.P6().isPhysical() );
  ufAssert( p1.P8().isPhysical() );
  ufAssert( p1.P10().isPhysical() );
  ufAssert( p1.P12().isPhysical() );
  ufAssert( p1.P14().isPhysical() );
  ufAssert( p1.PSW_C().isPhysical() );

  ufAssert( !p1.A0().isVirtual() );
  ufAssert( !p1.A1().isVirtual() );
  ufAssert( !p1.A2().isVirtual() );
  ufAssert( !p1.A3().isVirtual() );
  ufAssert( !p1.A4().isVirtual() );
  ufAssert( !p1.A5().isVirtual() );
  ufAssert( !p1.A6().isVirtual() );
  ufAssert( !p1.A7().isVirtual() );
  ufAssert( !p1.A8().isVirtual() );
  ufAssert( !p1.A9().isVirtual() );
  ufAssert( !p1.A10().isVirtual() );
  ufAssert( !p1.A11().isVirtual() );
  ufAssert( !p1.A12().isVirtual() );
  ufAssert( !p1.A13().isVirtual() );
  ufAssert( !p1.A14().isVirtual() );
  ufAssert( !p1.A15().isVirtual() );
  ufAssert( !p1.D0().isVirtual() );
  ufAssert( !p1.D1().isVirtual() );
  ufAssert( !p1.D2().isVirtual() );
  ufAssert( !p1.D3().isVirtual() );
  ufAssert( !p1.D4().isVirtual() );
  ufAssert( !p1.D5().isVirtual() );
  ufAssert( !p1.D6().isVirtual() );
  ufAssert( !p1.D7().isVirtual() );
  ufAssert( !p1.D8().isVirtual() );
  ufAssert( !p1.D9().isVirtual() );
  ufAssert( !p1.D10().isVirtual() );
  ufAssert( !p1.D11().isVirtual() );
  ufAssert( !p1.D12().isVirtual() );
  ufAssert( !p1.D13().isVirtual() );
  ufAssert( !p1.D14().isVirtual() );
  ufAssert( !p1.D15().isVirtual() );
  ufAssert( !p1.E0().isVirtual() );
  ufAssert( !p1.E2().isVirtual() );
  ufAssert( !p1.E4().isVirtual() );
  ufAssert( !p1.E6().isVirtual() );
  ufAssert( !p1.E8().isVirtual() );
  ufAssert( !p1.E10().isVirtual() );
  ufAssert( !p1.E12().isVirtual() );
  ufAssert( !p1.E14().isVirtual() );
  ufAssert( !p1.P0().isVirtual() );
  ufAssert( !p1.P2().isVirtual() );
  ufAssert( !p1.P4().isVirtual() );
  ufAssert( !p1.P6().isVirtual() );
  ufAssert( !p1.P8().isVirtual() );
  ufAssert( !p1.P10().isVirtual() );
  ufAssert( !p1.P12().isVirtual() );
  ufAssert( !p1.P14().isVirtual() );
  ufAssert( !p1.PSW_C().isVirtual() );

  // Check register type.
  ufAssert( p1.A0().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A1().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A2().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A3().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A4().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A5().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A6().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A7().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A8().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A9().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A10().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A11().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A12().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A13().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A14().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.A15().getType() == TC131::RegisterType::aReg );
  ufAssert( p1.D0().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D1().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D2().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D3().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D4().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D5().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D6().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D7().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D8().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D9().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D10().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D11().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D12().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D13().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D14().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.D15().getType() == TC131::RegisterType::dReg );
  ufAssert( p1.E0().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.E2().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.E4().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.E6().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.E8().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.E10().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.E12().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.E14().getType() == TC131::RegisterType::eReg );
  ufAssert( p1.P0().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.P2().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.P4().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.P6().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.P8().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.P10().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.P12().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.P14().getType() == TC131::RegisterType::pReg );
  ufAssert( p1.PSW_C().getType() == TC131::RegisterType::pswBit );

  // Check insertion.
  ufAssert( p1.A0().isInserted() );
  ufAssert( p1.A1().isInserted() );
  ufAssert( p1.A2().isInserted() );
  ufAssert( p1.A3().isInserted() );
  ufAssert( p1.A4().isInserted() );
  ufAssert( p1.A5().isInserted() );
  ufAssert( p1.A6().isInserted() );
  ufAssert( p1.A7().isInserted() );
  ufAssert( p1.A8().isInserted() );
  ufAssert( p1.A9().isInserted() );
  ufAssert( p1.A10().isInserted() );
  ufAssert( p1.A11().isInserted() );
  ufAssert( p1.A12().isInserted() );
  ufAssert( p1.A13().isInserted() );
  ufAssert( p1.A14().isInserted() );
  ufAssert( p1.A15().isInserted() );
  ufAssert( p1.D0().isInserted() );
  ufAssert( p1.D1().isInserted() );
  ufAssert( p1.D2().isInserted() );
  ufAssert( p1.D3().isInserted() );
  ufAssert( p1.D4().isInserted() );
  ufAssert( p1.D5().isInserted() );
  ufAssert( p1.D6().isInserted() );
  ufAssert( p1.D7().isInserted() );
  ufAssert( p1.D8().isInserted() );
  ufAssert( p1.D9().isInserted() );
  ufAssert( p1.D10().isInserted() );
  ufAssert( p1.D11().isInserted() );
  ufAssert( p1.D12().isInserted() );
  ufAssert( p1.D13().isInserted() );
  ufAssert( p1.D14().isInserted() );
  ufAssert( p1.D15().isInserted() );
  ufAssert( p1.E0().isInserted() );
  ufAssert( p1.E2().isInserted() );
  ufAssert( p1.E4().isInserted() );
  ufAssert( p1.E6().isInserted() );
  ufAssert( p1.E8().isInserted() );
  ufAssert( p1.E10().isInserted() );
  ufAssert( p1.E12().isInserted() );
  ufAssert( p1.E14().isInserted() );
  ufAssert( p1.P0().isInserted() );
  ufAssert( p1.P2().isInserted() );
  ufAssert( p1.P4().isInserted() );
  ufAssert( p1.P6().isInserted() );
  ufAssert( p1.P8().isInserted() );
  ufAssert( p1.P10().isInserted() );
  ufAssert( p1.P12().isInserted() );
  ufAssert( p1.P14().isInserted() );
  ufAssert( p1.PSW_C().isInserted() );

  // Check bit widths.
  ufAssert( TC131::RegisterType::aReg.getBitWidth() == 32 );
  ufAssert( TC131::RegisterType::dReg.getBitWidth() == 32 );
  ufAssert( TC131::RegisterType::eReg.getBitWidth() == 64 );
  ufAssert( TC131::RegisterType::pReg.getBitWidth() == 64 );
  ufAssert( TC131::RegisterType::pswBit.getBitWidth() == 1 );

  ufAssert( p1.A0().getBitWidth() == 32 );
  ufAssert( p1.A1().getBitWidth() == 32 );
  ufAssert( p1.A2().getBitWidth() == 32 );
  ufAssert( p1.A3().getBitWidth() == 32 );
  ufAssert( p1.A4().getBitWidth() == 32 );
  ufAssert( p1.A5().getBitWidth() == 32 );
  ufAssert( p1.A6().getBitWidth() == 32 );
  ufAssert( p1.A7().getBitWidth() == 32 );
  ufAssert( p1.A8().getBitWidth() == 32 );
  ufAssert( p1.A9().getBitWidth() == 32 );
  ufAssert( p1.A10().getBitWidth() == 32 );
  ufAssert( p1.A11().getBitWidth() == 32 );
  ufAssert( p1.A12().getBitWidth() == 32 );
  ufAssert( p1.A13().getBitWidth() == 32 );
  ufAssert( p1.A14().getBitWidth() == 32 );
  ufAssert( p1.A15().getBitWidth() == 32 );
  ufAssert( p1.D0().getBitWidth() == 32 );
  ufAssert( p1.D1().getBitWidth() == 32 );
  ufAssert( p1.D2().getBitWidth() == 32 );
  ufAssert( p1.D3().getBitWidth() == 32 );
  ufAssert( p1.D4().getBitWidth() == 32 );
  ufAssert( p1.D5().getBitWidth() == 32 );
  ufAssert( p1.D6().getBitWidth() == 32 );
  ufAssert( p1.D7().getBitWidth() == 32 );
  ufAssert( p1.D8().getBitWidth() == 32 );
  ufAssert( p1.D9().getBitWidth() == 32 );
  ufAssert( p1.D10().getBitWidth() == 32 );
  ufAssert( p1.D11().getBitWidth() == 32 );
  ufAssert( p1.D12().getBitWidth() == 32 );
  ufAssert( p1.D13().getBitWidth() == 32 );
  ufAssert( p1.D14().getBitWidth() == 32 );
  ufAssert( p1.D15().getBitWidth() == 32 );
  ufAssert( p1.E0().getBitWidth() == 64 );
  ufAssert( p1.E2().getBitWidth() == 64 );
  ufAssert( p1.E4().getBitWidth() == 64 );
  ufAssert( p1.E6().getBitWidth() == 64 );
  ufAssert( p1.E8().getBitWidth() == 64 );
  ufAssert( p1.E10().getBitWidth() == 64 );
  ufAssert( p1.E12().getBitWidth() == 64 );
  ufAssert( p1.E14().getBitWidth() == 64 );
  ufAssert( p1.P0().getBitWidth() == 64 );
  ufAssert( p1.P2().getBitWidth() == 64 );
  ufAssert( p1.P4().getBitWidth() == 64 );
  ufAssert( p1.P6().getBitWidth() == 64 );
  ufAssert( p1.P8().getBitWidth() == 64 );
  ufAssert( p1.P10().getBitWidth() == 64 );
  ufAssert( p1.P12().getBitWidth() == 64 );
  ufAssert( p1.P14().getBitWidth() == 64 );
  ufAssert( p1.PSW_C().getBitWidth() == 1 );

  // Check stack pointer property.
  ufAssert( !p1.A0().isStackPointer() );
  ufAssert( !p1.A1().isStackPointer() );
  ufAssert( !p1.A2().isStackPointer() );
  ufAssert( !p1.A3().isStackPointer() );
  ufAssert( !p1.A4().isStackPointer() );
  ufAssert( !p1.A5().isStackPointer() );
  ufAssert( !p1.A6().isStackPointer() );
  ufAssert( !p1.A7().isStackPointer() );
  ufAssert( !p1.A8().isStackPointer() );
  ufAssert( !p1.A9().isStackPointer() );
  ufAssert( p1.A10().isStackPointer() );
  ufAssert( !p1.A11().isStackPointer() );
  ufAssert( !p1.A12().isStackPointer() );
  ufAssert( !p1.A13().isStackPointer() );
  ufAssert( !p1.A14().isStackPointer() );
  ufAssert( !p1.A15().isStackPointer() );
  ufAssert( !p1.D0().isStackPointer() );
  ufAssert( !p1.D1().isStackPointer() );
  ufAssert( !p1.D2().isStackPointer() );
  ufAssert( !p1.D3().isStackPointer() );
  ufAssert( !p1.D4().isStackPointer() );
  ufAssert( !p1.D5().isStackPointer() );
  ufAssert( !p1.D6().isStackPointer() );
  ufAssert( !p1.D7().isStackPointer() );
  ufAssert( !p1.D8().isStackPointer() );
  ufAssert( !p1.D9().isStackPointer() );
  ufAssert( !p1.D10().isStackPointer() );
  ufAssert( !p1.D11().isStackPointer() );
  ufAssert( !p1.D12().isStackPointer() );
  ufAssert( !p1.D13().isStackPointer() );
  ufAssert( !p1.D14().isStackPointer() );
  ufAssert( !p1.D15().isStackPointer() );
  ufAssert( !p1.E0().isStackPointer() );
  ufAssert( !p1.E2().isStackPointer() );
  ufAssert( !p1.E4().isStackPointer() );
  ufAssert( !p1.E6().isStackPointer() );
  ufAssert( !p1.E8().isStackPointer() );
  ufAssert( !p1.E10().isStackPointer() );
  ufAssert( !p1.E12().isStackPointer() );
  ufAssert( !p1.E14().isStackPointer() );
  ufAssert( !p1.P0().isStackPointer() );
  ufAssert( !p1.P2().isStackPointer() );
  ufAssert( !p1.P4().isStackPointer() );
  ufAssert( !p1.P6().isStackPointer() );
  ufAssert( !p1.P8().isStackPointer() );
  ufAssert( !p1.P10().isStackPointer() );
  ufAssert( !p1.P12().isStackPointer() );
  ufAssert( !p1.P14().isStackPointer() );
  ufAssert( !p1.PSW_C().isStackPointer() );

  // Check PSW bit property.
  ufAssert( !TC13::isPSW_C( p1.A0() ) );
  ufAssert( !TC13::isPSW_C( p1.A1() ) );
  ufAssert( !TC13::isPSW_C( p1.A2() ) );
  ufAssert( !TC13::isPSW_C( p1.A3() ) );
  ufAssert( !TC13::isPSW_C( p1.A4() ) );
  ufAssert( !TC13::isPSW_C( p1.A5() ) );
  ufAssert( !TC13::isPSW_C( p1.A6() ) );
  ufAssert( !TC13::isPSW_C( p1.A7() ) );
  ufAssert( !TC13::isPSW_C( p1.A8() ) );
  ufAssert( !TC13::isPSW_C( p1.A9() ) );
  ufAssert( !TC13::isPSW_C( p1.A10() ) );
  ufAssert( !TC13::isPSW_C( p1.A11() ) );
  ufAssert( !TC13::isPSW_C( p1.A12() ) );
  ufAssert( !TC13::isPSW_C( p1.A13() ) );
  ufAssert( !TC13::isPSW_C( p1.A14() ) );
  ufAssert( !TC13::isPSW_C( p1.A15() ) );
  ufAssert( !TC13::isPSW_C( p1.D0() ) );
  ufAssert( !TC13::isPSW_C( p1.D1() ) );
  ufAssert( !TC13::isPSW_C( p1.D2() ) );
  ufAssert( !TC13::isPSW_C( p1.D3() ) );
  ufAssert( !TC13::isPSW_C( p1.D4() ) );
  ufAssert( !TC13::isPSW_C( p1.D5() ) );
  ufAssert( !TC13::isPSW_C( p1.D6() ) );
  ufAssert( !TC13::isPSW_C( p1.D7() ) );
  ufAssert( !TC13::isPSW_C( p1.D8() ) );
  ufAssert( !TC13::isPSW_C( p1.D9() ) );
  ufAssert( !TC13::isPSW_C( p1.D10() ) );
  ufAssert( !TC13::isPSW_C( p1.D11() ) );
  ufAssert( !TC13::isPSW_C( p1.D12() ) );
  ufAssert( !TC13::isPSW_C( p1.D13() ) );
  ufAssert( !TC13::isPSW_C( p1.D14() ) );
  ufAssert( !TC13::isPSW_C( p1.D15() ) );
  ufAssert( !TC13::isPSW_C( p1.E0() ) );
  ufAssert( !TC13::isPSW_C( p1.E2() ) );
  ufAssert( !TC13::isPSW_C( p1.E4() ) );
  ufAssert( !TC13::isPSW_C( p1.E6() ) );
  ufAssert( !TC13::isPSW_C( p1.E8() ) );
  ufAssert( !TC13::isPSW_C( p1.E10() ) );
  ufAssert( !TC13::isPSW_C( p1.E12() ) );
  ufAssert( !TC13::isPSW_C( p1.E14() ) );
  ufAssert( !TC13::isPSW_C( p1.P0() ) );
  ufAssert( !TC13::isPSW_C( p1.P2() ) );
  ufAssert( !TC13::isPSW_C( p1.P4() ) );
  ufAssert( !TC13::isPSW_C( p1.P6() ) );
  ufAssert( !TC13::isPSW_C( p1.P8() ) );
  ufAssert( !TC13::isPSW_C( p1.P10() ) );
  ufAssert( !TC13::isPSW_C( p1.P12() ) );
  ufAssert( !TC13::isPSW_C( p1.P14() ) );
  ufAssert( TC13::isPSW_C( p1.PSW_C() ) );

  // Check vectors of physical registers.
  ufAssert( p1.getPhRegs().size() == 49 );

  auto v1( p1.getPhRegs() );
  auto it1 = v1.begin();
  ufAssert( (*it1++).get().getName() == "a0" );
  ufAssert( (*it1++).get().getName() == "a1" );
  ufAssert( (*it1++).get().getName() == "a2" );
  ufAssert( (*it1++).get().getName() == "a3" );
  ufAssert( (*it1++).get().getName() == "a4" );
  ufAssert( (*it1++).get().getName() == "a5" );
  ufAssert( (*it1++).get().getName() == "a6" );
  ufAssert( (*it1++).get().getName() == "a7" );
  ufAssert( (*it1++).get().getName() == "a8" );
  ufAssert( (*it1++).get().getName() == "a9" );
  ufAssert( (*it1++).get().getName() == "a10" );
  ufAssert( (*it1++).get().getName() == "a11" );
  ufAssert( (*it1++).get().getName() == "a12" );
  ufAssert( (*it1++).get().getName() == "a13" );
  ufAssert( (*it1++).get().getName() == "a14" );
  ufAssert( (*it1++).get().getName() == "a15" );
  ufAssert( (*it1++).get().getName() == "d0" );
  ufAssert( (*it1++).get().getName() == "d1" );
  ufAssert( (*it1++).get().getName() == "d2" );
  ufAssert( (*it1++).get().getName() == "d3" );
  ufAssert( (*it1++).get().getName() == "d4" );
  ufAssert( (*it1++).get().getName() == "d5" );
  ufAssert( (*it1++).get().getName() == "d6" );
  ufAssert( (*it1++).get().getName() == "d7" );
  ufAssert( (*it1++).get().getName() == "d8" );
  ufAssert( (*it1++).get().getName() == "d9" );
  ufAssert( (*it1++).get().getName() == "d10" );
  ufAssert( (*it1++).get().getName() == "d11" );
  ufAssert( (*it1++).get().getName() == "d12" );
  ufAssert( (*it1++).get().getName() == "d13" );
  ufAssert( (*it1++).get().getName() == "d14" );
  ufAssert( (*it1++).get().getName() == "d15" );
  ufAssert( (*it1++).get().getName() == "e0" );
  ufAssert( (*it1++).get().getName() == "e2" );
  ufAssert( (*it1++).get().getName() == "e4" );
  ufAssert( (*it1++).get().getName() == "e6" );
  ufAssert( (*it1++).get().getName() == "e8" );
  ufAssert( (*it1++).get().getName() == "e10" );
  ufAssert( (*it1++).get().getName() == "e12" );
  ufAssert( (*it1++).get().getName() == "e14" );
  ufAssert( (*it1++).get().getName() == "p0" );
  ufAssert( (*it1++).get().getName() == "p2" );
  ufAssert( (*it1++).get().getName() == "p4" );
  ufAssert( (*it1++).get().getName() == "p6" );
  ufAssert( (*it1++).get().getName() == "p8" );
  ufAssert( (*it1++).get().getName() == "p10" );
  ufAssert( (*it1++).get().getName() == "p12" );
  ufAssert( (*it1++).get().getName() == "p14" );
  ufAssert( (*it1++).get().getName() == "PSW.C" );

  auto v2( p1.getPhRegs( TC131::RegisterType::dReg ) );
  auto it2 = v2.begin();
  ufAssert( v2.size() == 16 );
  ufAssert( (*it2++).get().getName() == "d0" );
  ufAssert( (*it2++).get().getName() == "d1" );
  ufAssert( (*it2++).get().getName() == "d2" );
  ufAssert( (*it2++).get().getName() == "d3" );
  ufAssert( (*it2++).get().getName() == "d4" );
  ufAssert( (*it2++).get().getName() == "d5" );
  ufAssert( (*it2++).get().getName() == "d6" );
  ufAssert( (*it2++).get().getName() == "d7" );
  ufAssert( (*it2++).get().getName() == "d8" );
  ufAssert( (*it2++).get().getName() == "d9" );
  ufAssert( (*it2++).get().getName() == "d10" );
  ufAssert( (*it2++).get().getName() == "d11" );
  ufAssert( (*it2++).get().getName() == "d12" );
  ufAssert( (*it2++).get().getName() == "d13" );
  ufAssert( (*it2++).get().getName() == "d14" );
  ufAssert( (*it2++).get().getName() == "d15" );

  auto v3( p1.getPhRegs( TC131::RegisterType::aReg ) );
  auto it3 = v3.begin();
  ufAssert( v2.size() == 16 );
  ufAssert( (*it3++).get().getName() == "a0" );
  ufAssert( (*it3++).get().getName() == "a1" );
  ufAssert( (*it3++).get().getName() == "a2" );
  ufAssert( (*it3++).get().getName() == "a3" );
  ufAssert( (*it3++).get().getName() == "a4" );
  ufAssert( (*it3++).get().getName() == "a5" );
  ufAssert( (*it3++).get().getName() == "a6" );
  ufAssert( (*it3++).get().getName() == "a7" );
  ufAssert( (*it3++).get().getName() == "a8" );
  ufAssert( (*it3++).get().getName() == "a9" );
  ufAssert( (*it3++).get().getName() == "a10" );
  ufAssert( (*it3++).get().getName() == "a11" );
  ufAssert( (*it3++).get().getName() == "a12" );
  ufAssert( (*it3++).get().getName() == "a13" );
  ufAssert( (*it3++).get().getName() == "a14" );
  ufAssert( (*it3++).get().getName() == "a15" );

  auto v4( p1.getPhRegs( TC131::RegisterType::eReg ) );
  auto it4 = v4.begin();
  ufAssert( v4.size() == 8 );
  ufAssert( (*it4++).get().getName() == "e0" );
  ufAssert( (*it4++).get().getName() == "e2" );
  ufAssert( (*it4++).get().getName() == "e4" );
  ufAssert( (*it4++).get().getName() == "e6" );
  ufAssert( (*it4++).get().getName() == "e8" );
  ufAssert( (*it4++).get().getName() == "e10" );
  ufAssert( (*it4++).get().getName() == "e12" );
  ufAssert( (*it4++).get().getName() == "e14" );

  auto v5( p1.getPhRegs( TC131::RegisterType::pReg ) );
  auto it5 = v5.begin();
  ufAssert( v5.size() == 8 );
  ufAssert( (*it5++).get().getName() == "p0" );
  ufAssert( (*it5++).get().getName() == "p2" );
  ufAssert( (*it5++).get().getName() == "p4" );
  ufAssert( (*it5++).get().getName() == "p6" );
  ufAssert( (*it5++).get().getName() == "p8" );
  ufAssert( (*it5++).get().getName() == "p10" );
  ufAssert( (*it5++).get().getName() == "p12" );
  ufAssert( (*it5++).get().getName() == "p14" );

  auto v6( p1.getPhRegs( TC131::RegisterType::pswBit ) );
  auto it6 = v6.begin();
  ufAssert( v6.size() == 1 );
  ufAssert( (*it6++).get().getName() == "PSW.C" );

  // Check parent/child relationships.
  ufAssert( p1.E0().hasChilds() );
  ufAssert( !p1.E0().isChild() );
  ufAssert( !p1.D0().hasChilds() );
  ufAssert( p1.D0().isChild() );
  ufAssert( !p1.D1().hasChilds() );
  ufAssert( p1.D1().isChild() );

  ufAssert( p1.E0().getChilds().size() == 2 );
  ufAssert( p1.E0().getChilds().front().get() == p1.D0() );
  ufAssert( p1.E0().getChilds().back().get() == p1.D1() );
  ufAssert( p1.D0().getParent() == p1.E0() );
  ufAssert( p1.D1().getParent() == p1.E0() );
  ufAssert( p1.E0().getParent() == p1.E0() );
  ufAssert( p1.D0().getRoot() == p1.E0() );
  ufAssert( p1.D1().getRoot() == p1.E0() );
  ufAssert( p1.E0().getRoot() == p1.E0() );
  auto v = p1.E0().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D0() );
  ufAssert( v[ 1 ].get() == p1.D1() );

  ufAssert( p1.E2().hasChilds() );
  ufAssert( !p1.E2().isChild() );
  ufAssert( !p1.D2().hasChilds() );
  ufAssert( p1.D2().isChild() );
  ufAssert( !p1.D3().hasChilds() );
  ufAssert( p1.D3().isChild() );

  ufAssert( p1.E2().getChilds().size() == 2 );
  ufAssert( p1.E2().getChilds().front().get() == p1.D2() );
  ufAssert( p1.E2().getChilds().back().get() == p1.D3() );
  ufAssert( p1.D2().getParent() == p1.E2() );
  ufAssert( p1.D3().getParent() == p1.E2() );
  ufAssert( p1.E2().getParent() == p1.E2() );
  ufAssert( p1.D2().getRoot() == p1.E2() );
  ufAssert( p1.D3().getRoot() == p1.E2() );
  ufAssert( p1.E2().getRoot() == p1.E2() );
  v = p1.E2().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D2() );
  ufAssert( v[ 1 ].get() == p1.D3() );

  ufAssert( p1.E4().hasChilds() );
  ufAssert( !p1.E4().isChild() );
  ufAssert( !p1.D4().hasChilds() );
  ufAssert( p1.D4().isChild() );
  ufAssert( !p1.D5().hasChilds() );
  ufAssert( p1.D5().isChild() );

  ufAssert( p1.E4().getChilds().size() == 2 );
  ufAssert( p1.E4().getChilds().front().get() == p1.D4() );
  ufAssert( p1.E4().getChilds().back().get() == p1.D5() );
  ufAssert( p1.D4().getParent() == p1.E4() );
  ufAssert( p1.D5().getParent() == p1.E4() );
  ufAssert( p1.E4().getParent() == p1.E4() );
  ufAssert( p1.D4().getRoot() == p1.E4() );
  ufAssert( p1.D5().getRoot() == p1.E4() );
  ufAssert( p1.E4().getRoot() == p1.E4() );
  v = p1.E4().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D4() );
  ufAssert( v[ 1 ].get() == p1.D5() );

  ufAssert( p1.E6().hasChilds() );
  ufAssert( !p1.E6().isChild() );
  ufAssert( !p1.D6().hasChilds() );
  ufAssert( p1.D6().isChild() );
  ufAssert( !p1.D7().hasChilds() );
  ufAssert( p1.D7().isChild() );

  ufAssert( p1.E6().getChilds().size() == 2 );
  ufAssert( p1.E6().getChilds().front().get() == p1.D6() );
  ufAssert( p1.E6().getChilds().back().get() == p1.D7() );
  ufAssert( p1.D6().getParent() == p1.E6() );
  ufAssert( p1.D7().getParent() == p1.E6() );
  ufAssert( p1.E6().getParent() == p1.E6() );
  ufAssert( p1.D6().getRoot() == p1.E6() );
  ufAssert( p1.D7().getRoot() == p1.E6() );
  ufAssert( p1.E6().getRoot() == p1.E6() );
  v = p1.E6().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D6() );
  ufAssert( v[ 1 ].get() == p1.D7() );

  ufAssert( p1.E8().hasChilds() );
  ufAssert( !p1.E8().isChild() );
  ufAssert( !p1.D8().hasChilds() );
  ufAssert( p1.D8().isChild() );
  ufAssert( !p1.D9().hasChilds() );
  ufAssert( p1.D9().isChild() );

  ufAssert( p1.E8().getChilds().size() == 2 );
  ufAssert( p1.E8().getChilds().front().get() == p1.D8() );
  ufAssert( p1.E8().getChilds().back().get() == p1.D9() );
  ufAssert( p1.D8().getParent() == p1.E8() );
  ufAssert( p1.D9().getParent() == p1.E8() );
  ufAssert( p1.E8().getParent() == p1.E8() );
  ufAssert( p1.D8().getRoot() == p1.E8() );
  ufAssert( p1.D9().getRoot() == p1.E8() );
  ufAssert( p1.E8().getRoot() == p1.E8() );
  v = p1.E8().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D8() );
  ufAssert( v[ 1 ].get() == p1.D9() );

  ufAssert( p1.E10().hasChilds() );
  ufAssert( !p1.E10().isChild() );
  ufAssert( !p1.D10().hasChilds() );
  ufAssert( p1.D10().isChild() );
  ufAssert( !p1.D11().hasChilds() );
  ufAssert( p1.D11().isChild() );

  ufAssert( p1.E10().getChilds().size() == 2 );
  ufAssert( p1.E10().getChilds().front().get() == p1.D10() );
  ufAssert( p1.E10().getChilds().back().get() == p1.D11() );
  ufAssert( p1.D10().getParent() == p1.E10() );
  ufAssert( p1.D11().getParent() == p1.E10() );
  ufAssert( p1.E10().getParent() == p1.E10() );
  ufAssert( p1.D10().getRoot() == p1.E10() );
  ufAssert( p1.D11().getRoot() == p1.E10() );
  ufAssert( p1.E10().getRoot() == p1.E10() );
  v = p1.E10().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D10() );
  ufAssert( v[ 1 ].get() == p1.D11() );

  ufAssert( p1.E12().hasChilds() );
  ufAssert( !p1.E12().isChild() );
  ufAssert( !p1.D12().hasChilds() );
  ufAssert( p1.D12().isChild() );
  ufAssert( !p1.D13().hasChilds() );
  ufAssert( p1.D13().isChild() );

  ufAssert( p1.E12().getChilds().size() == 2 );
  ufAssert( p1.E12().getChilds().front().get() == p1.D12() );
  ufAssert( p1.E12().getChilds().back().get() == p1.D13() );
  ufAssert( p1.D12().getParent() == p1.E12() );
  ufAssert( p1.D13().getParent() == p1.E12() );
  ufAssert( p1.E12().getParent() == p1.E12() );
  ufAssert( p1.D12().getRoot() == p1.E12() );
  ufAssert( p1.D13().getRoot() == p1.E12() );
  ufAssert( p1.E12().getRoot() == p1.E12() );
  v = p1.E12().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D12() );
  ufAssert( v[ 1 ].get() == p1.D13() );

  ufAssert( p1.E14().hasChilds() );
  ufAssert( !p1.E14().isChild() );
  ufAssert( !p1.D14().hasChilds() );
  ufAssert( p1.D14().isChild() );
  ufAssert( !p1.D15().hasChilds() );
  ufAssert( p1.D15().isChild() );

  ufAssert( p1.E14().getChilds().size() == 2 );
  ufAssert( p1.E14().getChilds().front().get() == p1.D14() );
  ufAssert( p1.E14().getChilds().back().get() == p1.D15() );
  ufAssert( p1.D14().getParent() == p1.E14() );
  ufAssert( p1.D15().getParent() == p1.E14() );
  ufAssert( p1.E14().getParent() == p1.E14() );
  ufAssert( p1.D14().getRoot() == p1.E14() );
  ufAssert( p1.D15().getRoot() == p1.E14() );
  ufAssert( p1.E14().getRoot() == p1.E14() );
  v = p1.E14().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.D14() );
  ufAssert( v[ 1 ].get() == p1.D15() );

  ufAssert( p1.P0().hasChilds() );
  ufAssert( !p1.P0().isChild() );
  ufAssert( !p1.A0().hasChilds() );
  ufAssert( p1.A0().isChild() );
  ufAssert( !p1.A1().hasChilds() );
  ufAssert( p1.A1().isChild() );

  ufAssert( p1.P0().getChilds().size() == 2 );
  ufAssert( p1.P0().getChilds().front().get() == p1.A0() );
  ufAssert( p1.P0().getChilds().back().get() == p1.A1() );
  ufAssert( p1.A0().getParent() == p1.P0() );
  ufAssert( p1.A1().getParent() == p1.P0() );
  ufAssert( p1.P0().getParent() == p1.P0() );
  ufAssert( p1.A0().getRoot() == p1.P0() );
  ufAssert( p1.A1().getRoot() == p1.P0() );
  ufAssert( p1.P0().getRoot() == p1.P0() );
  v = p1.P0().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A0() );
  ufAssert( v[ 1 ].get() == p1.A1() );

  ufAssert( p1.P2().hasChilds() );
  ufAssert( !p1.P2().isChild() );
  ufAssert( !p1.A2().hasChilds() );
  ufAssert( p1.A2().isChild() );
  ufAssert( !p1.A3().hasChilds() );
  ufAssert( p1.A3().isChild() );

  ufAssert( p1.P2().getChilds().size() == 2 );
  ufAssert( p1.P2().getChilds().front().get() == p1.A2() );
  ufAssert( p1.P2().getChilds().back().get() == p1.A3() );
  ufAssert( p1.A2().getParent() == p1.P2() );
  ufAssert( p1.A3().getParent() == p1.P2() );
  ufAssert( p1.P2().getParent() == p1.P2() );
  ufAssert( p1.A2().getRoot() == p1.P2() );
  ufAssert( p1.A3().getRoot() == p1.P2() );
  ufAssert( p1.P2().getRoot() == p1.P2() );
  v = p1.P2().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A2() );
  ufAssert( v[ 1 ].get() == p1.A3() );

  ufAssert( p1.P4().hasChilds() );
  ufAssert( !p1.P4().isChild() );
  ufAssert( !p1.A4().hasChilds() );
  ufAssert( p1.A4().isChild() );
  ufAssert( !p1.A5().hasChilds() );
  ufAssert( p1.A5().isChild() );

  ufAssert( p1.P4().getChilds().size() == 2 );
  ufAssert( p1.P4().getChilds().front().get() == p1.A4() );
  ufAssert( p1.P4().getChilds().back().get() == p1.A5() );
  ufAssert( p1.A4().getParent() == p1.P4() );
  ufAssert( p1.A5().getParent() == p1.P4() );
  ufAssert( p1.P4().getParent() == p1.P4() );
  ufAssert( p1.A4().getRoot() == p1.P4() );
  ufAssert( p1.A5().getRoot() == p1.P4() );
  ufAssert( p1.P4().getRoot() == p1.P4() );
  v = p1.P4().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A4() );
  ufAssert( v[ 1 ].get() == p1.A5() );

  ufAssert( p1.P6().hasChilds() );
  ufAssert( !p1.P6().isChild() );
  ufAssert( !p1.A6().hasChilds() );
  ufAssert( p1.A6().isChild() );
  ufAssert( !p1.A7().hasChilds() );
  ufAssert( p1.A7().isChild() );

  ufAssert( p1.P6().getChilds().size() == 2 );
  ufAssert( p1.P6().getChilds().front().get() == p1.A6() );
  ufAssert( p1.P6().getChilds().back().get() == p1.A7() );
  ufAssert( p1.A6().getParent() == p1.P6() );
  ufAssert( p1.A7().getParent() == p1.P6() );
  ufAssert( p1.P6().getParent() == p1.P6() );
  ufAssert( p1.A6().getRoot() == p1.P6() );
  ufAssert( p1.A7().getRoot() == p1.P6() );
  ufAssert( p1.P6().getRoot() == p1.P6() );
  v = p1.P6().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A6() );
  ufAssert( v[ 1 ].get() == p1.A7() );

  ufAssert( p1.P8().hasChilds() );
  ufAssert( !p1.P8().isChild() );
  ufAssert( !p1.A8().hasChilds() );
  ufAssert( p1.A8().isChild() );
  ufAssert( !p1.A9().hasChilds() );
  ufAssert( p1.A9().isChild() );

  ufAssert( p1.P8().getChilds().size() == 2 );
  ufAssert( p1.P8().getChilds().front().get() == p1.A8() );
  ufAssert( p1.P8().getChilds().back().get() == p1.A9() );
  ufAssert( p1.A8().getParent() == p1.P8() );
  ufAssert( p1.A9().getParent() == p1.P8() );
  ufAssert( p1.P8().getParent() == p1.P8() );
  ufAssert( p1.A8().getRoot() == p1.P8() );
  ufAssert( p1.A9().getRoot() == p1.P8() );
  ufAssert( p1.P8().getRoot() == p1.P8() );
  v = p1.P8().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A8() );
  ufAssert( v[ 1 ].get() == p1.A9() );

  ufAssert( p1.P10().hasChilds() );
  ufAssert( !p1.P10().isChild() );
  ufAssert( !p1.A10().hasChilds() );
  ufAssert( p1.A10().isChild() );
  ufAssert( !p1.A11().hasChilds() );
  ufAssert( p1.A11().isChild() );

  ufAssert( p1.P10().getChilds().size() == 2 );
  ufAssert( p1.P10().getChilds().front().get() == p1.A10() );
  ufAssert( p1.P10().getChilds().back().get() == p1.A11() );
  ufAssert( p1.A10().getParent() == p1.P10() );
  ufAssert( p1.A11().getParent() == p1.P10() );
  ufAssert( p1.P10().getParent() == p1.P10() );
  ufAssert( p1.A10().getRoot() == p1.P10() );
  ufAssert( p1.A11().getRoot() == p1.P10() );
  ufAssert( p1.P10().getRoot() == p1.P10() );
  v = p1.P10().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A10() );
  ufAssert( v[ 1 ].get() == p1.A11() );

  ufAssert( p1.P12().hasChilds() );
  ufAssert( !p1.P12().isChild() );
  ufAssert( !p1.A12().hasChilds() );
  ufAssert( p1.A12().isChild() );
  ufAssert( !p1.A13().hasChilds() );
  ufAssert( p1.A13().isChild() );

  ufAssert( p1.P12().getChilds().size() == 2 );
  ufAssert( p1.P12().getChilds().front().get() == p1.A12() );
  ufAssert( p1.P12().getChilds().back().get() == p1.A13() );
  ufAssert( p1.A12().getParent() == p1.P12() );
  ufAssert( p1.A13().getParent() == p1.P12() );
  ufAssert( p1.P12().getParent() == p1.P12() );
  ufAssert( p1.A12().getRoot() == p1.P12() );
  ufAssert( p1.A13().getRoot() == p1.P12() );
  ufAssert( p1.P12().getRoot() == p1.P12() );
  v = p1.P12().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A12() );
  ufAssert( v[ 1 ].get() == p1.A13() );

  ufAssert( p1.P14().hasChilds() );
  ufAssert( !p1.P14().isChild() );
  ufAssert( !p1.A14().hasChilds() );
  ufAssert( p1.A14().isChild() );
  ufAssert( !p1.A15().hasChilds() );
  ufAssert( p1.A15().isChild() );

  ufAssert( p1.P14().getChilds().size() == 2 );
  ufAssert( p1.P14().getChilds().front().get() == p1.A14() );
  ufAssert( p1.P14().getChilds().back().get() == p1.A15() );
  ufAssert( p1.A14().getParent() == p1.P14() );
  ufAssert( p1.A15().getParent() == p1.P14() );
  ufAssert( p1.P14().getParent() == p1.P14() );
  ufAssert( p1.A14().getRoot() == p1.P14() );
  ufAssert( p1.A15().getRoot() == p1.P14() );
  ufAssert( p1.P14().getRoot() == p1.P14() );
  v = p1.P14().getLeafs();
  ufAssert( v.size() == 2 );
  ufAssert( v[ 0 ].get() == p1.A14() );
  ufAssert( v[ 1 ].get() == p1.A15() );

  return( 0 );
}
