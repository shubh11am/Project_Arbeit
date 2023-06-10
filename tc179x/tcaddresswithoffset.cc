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
#include <arch/tricore/tc131.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include local headers
#include <tc179x/tcaddresswithoffset.h>


//
// Code section
//


using namespace std;
using namespace WIR;


//
// Public class methods
//

/*
  Constructor initializing an empty address + offset.
*/
TC_AddressWithOffset::TC_AddressWithOffset( void ) :
  AddressWithOffset {},
  mAReg { nullptr }
{
  DSTART( "TC_AddressWithOffset::TC_AddressWithOffset()" );
};


/*
  Constructor initializing an empty address + offset based memory access.
*/
TC_AddressWithOffset::TC_AddressWithOffset( LLIR_Register *reg,
                                            const WIR::TC_ARegV &r, long o ) :
  AddressWithOffset { reg, o },
  mAReg { &(const_cast<WIR::TC_ARegV &>( r )) }
{
  DSTART(
    "TC_AddressWithOffset::TC_AddressWithOffset(LLIR_Register*, const TC_ARegV&, long int)" );
};


/*
  getAReg returns the address register involved in an address modification.
*/
WIR::TC_ARegV &TC_AddressWithOffset::getAReg( void ) const
{
  DSTART( "TC_ARegV& TC_AddressWithOffset::getAReg() const" );

  return( *mAReg );
};
