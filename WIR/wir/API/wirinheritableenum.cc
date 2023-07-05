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
  @file wirinheritableenum.cc
  @brief This file implements an API for inheritable enumeration classes.

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
#include "wirinheritableenum.h"


//
// Code section
//

namespace WIR {


using namespace std;


unsigned int WIR_InheritableEnum::mMaxValue = 0;


//
// Public class methods
//

/*
  The == operator checks for equality of enumerators.
*/
bool WIR_InheritableEnum::operator == ( const WIR_InheritableEnum & __o ) const
{
  DSTART(
    "bool WIR_InheritableEnum::operator==(const WIR_InheritableEnum&) const" );

  return( mID == __o.mID );
};


/*
  The != operator checks for inequality of enumerators.
*/
bool WIR_InheritableEnum::operator != ( const WIR_InheritableEnum & __o ) const
{
  DSTART(
    "bool WIR_InheritableEnum::operator!=(const WIR_InheritableEnum&) const" );

  return( mID != __o.mID );
};


/*
  The < operator checks for less-than of enumerators.
*/
bool WIR_InheritableEnum::operator < ( const WIR_InheritableEnum & __o ) const
{
  DSTART(
    "bool WIR_InheritableEnum::operator<(const WIR_InheritableEnum&) const" );

  return( mID < __o.mID );
};


//
// Protected class methods
//

/*
  Default constructor assigning a new unique ID to an enumerator.
*/
WIR_InheritableEnum::WIR_InheritableEnum() :
  mID { mMaxValue++ }
{
  DSTART( "WIR_InheritableEnum::WIR_InheritableEnum()" );
};

}       // namespace WIR
