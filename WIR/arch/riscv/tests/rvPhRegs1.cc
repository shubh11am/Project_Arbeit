/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Intermediate Representation Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

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
#include <arch/riscv/rv32i.h>


using namespace std;
using namespace WIR;


int main( void )
{
  WIR_Init();

  RV32I p1;

  // Check names of all physical registers.
  ufAssert( p1.x0().getName() == "x0" );
  ufAssert( p1.x1().getName() == "x1" );
  ufAssert( p1.x2().getName() == "x2" );
  ufAssert( p1.x3().getName() == "x3" );
  ufAssert( p1.x4().getName() == "x4" );
  ufAssert( p1.x5().getName() == "x5" );
  ufAssert( p1.x6().getName() == "x6" );
  ufAssert( p1.x7().getName() == "x7" );
  ufAssert( p1.x8().getName() == "x8" );
  ufAssert( p1.x9().getName() == "x9" );
  ufAssert( p1.x10().getName() == "x10" );
  ufAssert( p1.x11().getName() == "x11" );
  ufAssert( p1.x12().getName() == "x12" );
  ufAssert( p1.x13().getName() == "x13" );
  ufAssert( p1.x14().getName() == "x14" );
  ufAssert( p1.x15().getName() == "x15" );
  ufAssert( p1.x16().getName() == "x16" );
  ufAssert( p1.x17().getName() == "x17" );
  ufAssert( p1.x18().getName() == "x18" );
  ufAssert( p1.x19().getName() == "x19" );
  ufAssert( p1.x20().getName() == "x20" );
  ufAssert( p1.x21().getName() == "x21" );
  ufAssert( p1.x22().getName() == "x22" );
  ufAssert( p1.x23().getName() == "x23" );
  ufAssert( p1.x24().getName() == "x24" );
  ufAssert( p1.x25().getName() == "x25" );
  ufAssert( p1.x26().getName() == "x26" );
  ufAssert( p1.x27().getName() == "x27" );
  ufAssert( p1.x28().getName() == "x28" );
  ufAssert( p1.x29().getName() == "x29" );
  ufAssert( p1.x30().getName() == "x30" );
  ufAssert( p1.x31().getName() == "x31" );

  ufAssert( p1.SP().getName() == "x2" );
  ufAssert( p1.RA().getName() == "x1" );

  // Check virtual/physical property.
  ufAssert( p1.x0().isPhysical() );
  ufAssert( p1.x1().isPhysical() );
  ufAssert( p1.x2().isPhysical() );
  ufAssert( p1.x3().isPhysical() );
  ufAssert( p1.x4().isPhysical() );
  ufAssert( p1.x5().isPhysical() );
  ufAssert( p1.x6().isPhysical() );
  ufAssert( p1.x7().isPhysical() );
  ufAssert( p1.x8().isPhysical() );
  ufAssert( p1.x9().isPhysical() );
  ufAssert( p1.x10().isPhysical() );
  ufAssert( p1.x11().isPhysical() );
  ufAssert( p1.x12().isPhysical() );
  ufAssert( p1.x13().isPhysical() );
  ufAssert( p1.x14().isPhysical() );
  ufAssert( p1.x15().isPhysical() );
  ufAssert( p1.x16().isPhysical() );
  ufAssert( p1.x17().isPhysical() );
  ufAssert( p1.x18().isPhysical() );
  ufAssert( p1.x19().isPhysical() );
  ufAssert( p1.x20().isPhysical() );
  ufAssert( p1.x21().isPhysical() );
  ufAssert( p1.x22().isPhysical() );
  ufAssert( p1.x23().isPhysical() );
  ufAssert( p1.x24().isPhysical() );
  ufAssert( p1.x25().isPhysical() );
  ufAssert( p1.x26().isPhysical() );
  ufAssert( p1.x27().isPhysical() );
  ufAssert( p1.x28().isPhysical() );
  ufAssert( p1.x29().isPhysical() );
  ufAssert( p1.x30().isPhysical() );
  ufAssert( p1.x31().isPhysical() );

  ufAssert( !p1.x0().isVirtual() );
  ufAssert( !p1.x1().isVirtual() );
  ufAssert( !p1.x2().isVirtual() );
  ufAssert( !p1.x3().isVirtual() );
  ufAssert( !p1.x4().isVirtual() );
  ufAssert( !p1.x5().isVirtual() );
  ufAssert( !p1.x6().isVirtual() );
  ufAssert( !p1.x7().isVirtual() );
  ufAssert( !p1.x8().isVirtual() );
  ufAssert( !p1.x9().isVirtual() );
  ufAssert( !p1.x10().isVirtual() );
  ufAssert( !p1.x11().isVirtual() );
  ufAssert( !p1.x12().isVirtual() );
  ufAssert( !p1.x13().isVirtual() );
  ufAssert( !p1.x14().isVirtual() );
  ufAssert( !p1.x15().isVirtual() );
  ufAssert( !p1.x16().isVirtual() );
  ufAssert( !p1.x17().isVirtual() );
  ufAssert( !p1.x18().isVirtual() );
  ufAssert( !p1.x19().isVirtual() );
  ufAssert( !p1.x20().isVirtual() );
  ufAssert( !p1.x21().isVirtual() );
  ufAssert( !p1.x22().isVirtual() );
  ufAssert( !p1.x23().isVirtual() );
  ufAssert( !p1.x24().isVirtual() );
  ufAssert( !p1.x25().isVirtual() );
  ufAssert( !p1.x26().isVirtual() );
  ufAssert( !p1.x27().isVirtual() );
  ufAssert( !p1.x28().isVirtual() );
  ufAssert( !p1.x29().isVirtual() );
  ufAssert( !p1.x30().isVirtual() );
  ufAssert( !p1.x31().isVirtual() );

  // Check register type.
  ufAssert( p1.x0().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x1().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x2().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x3().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x4().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x5().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x6().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x7().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x8().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x9().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x10().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x11().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x12().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x13().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x14().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x15().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x16().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x17().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x18().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x19().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x20().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x21().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x22().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x23().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x24().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x25().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x26().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x27().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x28().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x29().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x30().getType() == RV32I::RegisterType::reg );
  ufAssert( p1.x31().getType() == RV32I::RegisterType::reg );

  // Check insertion.
  ufAssert( p1.x0().isInserted() );
  ufAssert( p1.x1().isInserted() );
  ufAssert( p1.x2().isInserted() );
  ufAssert( p1.x3().isInserted() );
  ufAssert( p1.x4().isInserted() );
  ufAssert( p1.x5().isInserted() );
  ufAssert( p1.x6().isInserted() );
  ufAssert( p1.x7().isInserted() );
  ufAssert( p1.x8().isInserted() );
  ufAssert( p1.x9().isInserted() );
  ufAssert( p1.x10().isInserted() );
  ufAssert( p1.x11().isInserted() );
  ufAssert( p1.x12().isInserted() );
  ufAssert( p1.x13().isInserted() );
  ufAssert( p1.x14().isInserted() );
  ufAssert( p1.x15().isInserted() );
  ufAssert( p1.x16().isInserted() );
  ufAssert( p1.x17().isInserted() );
  ufAssert( p1.x18().isInserted() );
  ufAssert( p1.x19().isInserted() );
  ufAssert( p1.x20().isInserted() );
  ufAssert( p1.x21().isInserted() );
  ufAssert( p1.x22().isInserted() );
  ufAssert( p1.x23().isInserted() );
  ufAssert( p1.x24().isInserted() );
  ufAssert( p1.x25().isInserted() );
  ufAssert( p1.x26().isInserted() );
  ufAssert( p1.x27().isInserted() );
  ufAssert( p1.x28().isInserted() );
  ufAssert( p1.x29().isInserted() );
  ufAssert( p1.x30().isInserted() );
  ufAssert( p1.x31().isInserted() );

  // Check bit widths.
  ufAssert( RV32I::RegisterType::reg.getBitWidth() == 32 );

  ufAssert( p1.x0().getBitWidth() == 32 );
  ufAssert( p1.x1().getBitWidth() == 32 );
  ufAssert( p1.x2().getBitWidth() == 32 );
  ufAssert( p1.x3().getBitWidth() == 32 );
  ufAssert( p1.x4().getBitWidth() == 32 );
  ufAssert( p1.x5().getBitWidth() == 32 );
  ufAssert( p1.x6().getBitWidth() == 32 );
  ufAssert( p1.x7().getBitWidth() == 32 );
  ufAssert( p1.x8().getBitWidth() == 32 );
  ufAssert( p1.x9().getBitWidth() == 32 );
  ufAssert( p1.x10().getBitWidth() == 32 );
  ufAssert( p1.x11().getBitWidth() == 32 );
  ufAssert( p1.x12().getBitWidth() == 32 );
  ufAssert( p1.x13().getBitWidth() == 32 );
  ufAssert( p1.x14().getBitWidth() == 32 );
  ufAssert( p1.x15().getBitWidth() == 32 );
  ufAssert( p1.x16().getBitWidth() == 32 );
  ufAssert( p1.x17().getBitWidth() == 32 );
  ufAssert( p1.x18().getBitWidth() == 32 );
  ufAssert( p1.x19().getBitWidth() == 32 );
  ufAssert( p1.x20().getBitWidth() == 32 );
  ufAssert( p1.x21().getBitWidth() == 32 );
  ufAssert( p1.x22().getBitWidth() == 32 );
  ufAssert( p1.x23().getBitWidth() == 32 );
  ufAssert( p1.x24().getBitWidth() == 32 );
  ufAssert( p1.x25().getBitWidth() == 32 );
  ufAssert( p1.x26().getBitWidth() == 32 );
  ufAssert( p1.x27().getBitWidth() == 32 );
  ufAssert( p1.x28().getBitWidth() == 32 );
  ufAssert( p1.x29().getBitWidth() == 32 );
  ufAssert( p1.x30().getBitWidth() == 32 );
  ufAssert( p1.x31().getBitWidth() == 32 );

  // Check stack pointer property.
  ufAssert( !p1.x0().isStackPointer() );
  ufAssert( !p1.x1().isStackPointer() );
  ufAssert( p1.x2().isStackPointer() );
  ufAssert( !p1.x3().isStackPointer() );
  ufAssert( !p1.x4().isStackPointer() );
  ufAssert( !p1.x5().isStackPointer() );
  ufAssert( !p1.x6().isStackPointer() );
  ufAssert( !p1.x7().isStackPointer() );
  ufAssert( !p1.x8().isStackPointer() );
  ufAssert( !p1.x9().isStackPointer() );
  ufAssert( !p1.x10().isStackPointer() );
  ufAssert( !p1.x11().isStackPointer() );
  ufAssert( !p1.x12().isStackPointer() );
  ufAssert( !p1.x13().isStackPointer() );
  ufAssert( !p1.x14().isStackPointer() );
  ufAssert( !p1.x15().isStackPointer() );
  ufAssert( !p1.x16().isStackPointer() );
  ufAssert( !p1.x17().isStackPointer() );
  ufAssert( !p1.x18().isStackPointer() );
  ufAssert( !p1.x19().isStackPointer() );
  ufAssert( !p1.x20().isStackPointer() );
  ufAssert( !p1.x21().isStackPointer() );
  ufAssert( !p1.x22().isStackPointer() );
  ufAssert( !p1.x23().isStackPointer() );
  ufAssert( !p1.x24().isStackPointer() );
  ufAssert( !p1.x25().isStackPointer() );
  ufAssert( !p1.x26().isStackPointer() );
  ufAssert( !p1.x27().isStackPointer() );
  ufAssert( !p1.x28().isStackPointer() );
  ufAssert( !p1.x29().isStackPointer() );
  ufAssert( !p1.x30().isStackPointer() );
  ufAssert( !p1.x31().isStackPointer() );

  // Check vectors of physical registers.
  ufAssert( p1.getPhRegs().size() == 32 );

  auto v1( p1.getPhRegs() );
  auto it1 = v1.begin();
  ufAssert( (*it1++).get().getName() == "x0" );
  ufAssert( (*it1++).get().getName() == "x1" );
  ufAssert( (*it1++).get().getName() == "x2" );
  ufAssert( (*it1++).get().getName() == "x3" );
  ufAssert( (*it1++).get().getName() == "x4" );
  ufAssert( (*it1++).get().getName() == "x5" );
  ufAssert( (*it1++).get().getName() == "x6" );
  ufAssert( (*it1++).get().getName() == "x7" );
  ufAssert( (*it1++).get().getName() == "x8" );
  ufAssert( (*it1++).get().getName() == "x9" );
  ufAssert( (*it1++).get().getName() == "x10" );
  ufAssert( (*it1++).get().getName() == "x11" );
  ufAssert( (*it1++).get().getName() == "x12" );
  ufAssert( (*it1++).get().getName() == "x13" );
  ufAssert( (*it1++).get().getName() == "x14" );
  ufAssert( (*it1++).get().getName() == "x15" );
  ufAssert( (*it1++).get().getName() == "x16" );
  ufAssert( (*it1++).get().getName() == "x17" );
  ufAssert( (*it1++).get().getName() == "x18" );
  ufAssert( (*it1++).get().getName() == "x19" );
  ufAssert( (*it1++).get().getName() == "x20" );
  ufAssert( (*it1++).get().getName() == "x21" );
  ufAssert( (*it1++).get().getName() == "x22" );
  ufAssert( (*it1++).get().getName() == "x23" );
  ufAssert( (*it1++).get().getName() == "x24" );
  ufAssert( (*it1++).get().getName() == "x25" );
  ufAssert( (*it1++).get().getName() == "x26" );
  ufAssert( (*it1++).get().getName() == "x27" );
  ufAssert( (*it1++).get().getName() == "x28" );
  ufAssert( (*it1++).get().getName() == "x29" );
  ufAssert( (*it1++).get().getName() == "x30" );
  ufAssert( (*it1++).get().getName() == "x31" );

  auto v2( p1.getPhRegs( RV32I::RegisterType::reg ) );
  auto it2 = v2.begin();
  ufAssert( v2.size() == 32 );
  ufAssert( (*it2++).get().getName() == "x0" );
  ufAssert( (*it2++).get().getName() == "x1" );
  ufAssert( (*it2++).get().getName() == "x2" );
  ufAssert( (*it2++).get().getName() == "x3" );
  ufAssert( (*it2++).get().getName() == "x4" );
  ufAssert( (*it2++).get().getName() == "x5" );
  ufAssert( (*it2++).get().getName() == "x6" );
  ufAssert( (*it2++).get().getName() == "x7" );
  ufAssert( (*it2++).get().getName() == "x8" );
  ufAssert( (*it2++).get().getName() == "x9" );
  ufAssert( (*it2++).get().getName() == "x10" );
  ufAssert( (*it2++).get().getName() == "x11" );
  ufAssert( (*it2++).get().getName() == "x12" );
  ufAssert( (*it2++).get().getName() == "x13" );
  ufAssert( (*it2++).get().getName() == "x14" );
  ufAssert( (*it2++).get().getName() == "x15" );
  ufAssert( (*it2++).get().getName() == "x16" );
  ufAssert( (*it2++).get().getName() == "x17" );
  ufAssert( (*it2++).get().getName() == "x18" );
  ufAssert( (*it2++).get().getName() == "x19" );
  ufAssert( (*it2++).get().getName() == "x20" );
  ufAssert( (*it2++).get().getName() == "x21" );
  ufAssert( (*it2++).get().getName() == "x22" );
  ufAssert( (*it2++).get().getName() == "x23" );
  ufAssert( (*it2++).get().getName() == "x24" );
  ufAssert( (*it2++).get().getName() == "x25" );
  ufAssert( (*it2++).get().getName() == "x26" );
  ufAssert( (*it2++).get().getName() == "x27" );
  ufAssert( (*it2++).get().getName() == "x28" );
  ufAssert( (*it2++).get().getName() == "x29" );
  ufAssert( (*it2++).get().getName() == "x30" );
  ufAssert( (*it2++).get().getName() == "x31" );

  return( 0 );
}
