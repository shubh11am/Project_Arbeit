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
  @file tcnoofsuccspriority.cc
  @brief This file implements a  class computing the number of successor
         operations scheduling priority for TriCore operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "tcnoofsuccspriority.h"


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor.
*/
TC_NoOfSuccsPriority::TC_NoOfSuccsPriority( void ) :
  WIR_NoOfSuccsPriority {},
  TC_SchedulingPriority {}
{
  DSTART( "TC_NoOfSuccsPriority::TC_NoOfSuccsPriority()" );
};


/*
  Copy constructor.
*/
TC_NoOfSuccsPriority::TC_NoOfSuccsPriority( const TC_NoOfSuccsPriority &__o ) :
  WIR_SchedulingPriority { __o },
  WIR_NoOfSuccsPriority { __o },
  TC_SchedulingPriority { __o }
{
  DSTART(
    "TC_NoOfSuccsPriority::TC_NoOfSuccsPriority(const TC_NoOfSuccsPriority&)" );
};


/*
  Destructor.
*/
TC_NoOfSuccsPriority::~TC_NoOfSuccsPriority( void )
{
  DSTART( "virtual TC_NoOfSuccsPriority::~TC_NoOfSuccsPriority()" );
};


/*
  Copy-assignment operator.
*/
TC_NoOfSuccsPriority & TC_NoOfSuccsPriority::operator = ( const TC_NoOfSuccsPriority &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_NoOfSuccsPriority::operator = ( __o );
  TC_SchedulingPriority::operator = ( __o );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a TriCore maximum delay scheduling priority.
*/
WIR_SchedulingPriority *TC_NoOfSuccsPriority::clone( void ) const
{
  DSTART(
    "virtual WIR_SchedulingPriority* TC_NoOfSuccsPriority::clone() const" );

  return( new TC_NoOfSuccsPriority( *this ) );
};

}       // namespace WIR
