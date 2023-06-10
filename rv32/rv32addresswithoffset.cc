/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2020 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include local headers
#include <rv32/rv32addresswithoffset.h>


//
// Code section
//


using namespace std;
using namespace WIR;

namespace RV32 {
//
// Public class methods
//

/*
  Constructor initializing an empty address + offset.
*/
RV32_AddressWithOffset::RV32_AddressWithOffset( void ) :
  AddressWithOffset {},
  mAReg { nullptr }
{
  DSTART( "RV_AddressWithOffset::RV_AddressWithOffset()" );
};


/*
  Constructor initializing an empty address + offset based memory access.
*/
RV32_AddressWithOffset::RV32_AddressWithOffset( WIR::WIR_BaseRegister &r, long o ) :
  AddressWithOffset { nullptr, o },
  mAReg { &(dynamic_cast<WIR::RV_RegV &>( r )) }
{
  DSTART(
    "RV_AddressWithOffset::RV_AddressWithOffset(const RV_RegV&, long int)" );
};


/*
  getAReg returns the address register involved in an address modification.
*/
WIR::RV_RegV &RV32_AddressWithOffset::getAReg( void ) const
{
  DSTART( "RV_RegV& RV_AddressWithOffset::getAReg() const" );

  return( *mAReg );
};

}       // namespace RV32