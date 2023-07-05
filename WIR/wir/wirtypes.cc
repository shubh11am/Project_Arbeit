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

/*!
  @file wirtypes.cc
  @brief This file implements several simple basic data types that are used here
         and there within the %WIR library.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


/*
  WIR_Compare_BasicBlockPair is a comparator class that is used to sort, e.g.,
  sets of basic block pairs uniquely.
*/
bool WIR_Compare_BasicBlockPair::operator()( const WIR_BasicBlockPair &lhs,
                                             const WIR_BasicBlockPair &rhs ) const
{
  return(
    ( lhs.first.get().getID() < rhs.first.get().getID() ) ? true :
      ( lhs.first.get().getID() > rhs.first.get().getID() ) ? false :
        lhs.second.get().getID() < rhs.second.get().getID() );
};


/*
  This operator performs a less-than comparison of two WIR_MemoryRegions.
*/
bool operator < ( const WIR_MemoryRegion &lhs, const WIR_MemoryRegion &rhs )
{
  DSTART( "bool operator<(const WIR_MemoryRegion&, const WIR_MemoryRegion&)" );

  return( lhs.getBaseAddress() < rhs.getBaseAddress() );
};


/*
  WIR_Compare_MemoryRegions is a comparator class that is used to sort, e.g.,
  sets of WIR memory regions uniquely by their base addresses.
*/
bool WIR_Compare_MemoryRegions::operator()( const reference_wrapper<WIR_MemoryRegion> &lhs,
                                            const reference_wrapper<WIR_MemoryRegion> &rhs ) const
{
  return(
    ( lhs.get().getBaseAddress() < rhs.get().getBaseAddress() ) ? true :
      ( lhs.get().getBaseAddress() > rhs.get().getBaseAddress() ) ? false :
        ( lhs.get().getID() < rhs.get().getID() ) );
};


/*
  This operator compares two WIR_RegisterSets for equality.
*/
bool operator == ( const WIR_RegisterSet &lhs, const WIR_RegisterSet &rhs )
{
  DSTART( "bool operator==(const WIR_RegisterSet&, const WIR_RegisterSet&)" );

  if ( lhs.size() != rhs.size() )
    return( false );

  auto it1 = lhs.begin();
  auto it2 = rhs.begin();
  for ( ; it1 != lhs.end(); ++it1, ++it2 )
    if ( (*it1).get() != (*it2).get() )
      return( false );

  return( true );
};


/*
  This operator compares two WIR_RegisterSets for inequality.
*/
bool operator != ( const WIR_RegisterSet &lhs, const WIR_RegisterSet &rhs )
{
  DSTART( "bool operator!=(const WIR_RegisterSet&, const WIR_RegisterSet&)" );

  return( !( lhs == rhs ) );
};

}       // namespace WIR
