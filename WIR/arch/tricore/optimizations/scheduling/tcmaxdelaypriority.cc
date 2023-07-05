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
  @file tcmaxdelaypriority.cc
  @brief This file implements a class computing the maximum delay scheduling
         priority for TriCore operations.

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
#include "tcmaxdelaypriority.h"


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
TC_MaxDelayPriority::TC_MaxDelayPriority( void ) :
  WIR_MaxDelayPriority {},
  TC_SchedulingPriority {}
{
  DSTART( "TC_MaxDelayPriority::TC_MaxDelayPriority()" );
};


/*
  Copy constructor.
*/
TC_MaxDelayPriority::TC_MaxDelayPriority( const TC_MaxDelayPriority &__o ) :
  WIR_SchedulingPriority { __o },
  WIR_MaxDelayPriority { __o },
  TC_SchedulingPriority { __o }
{
  DSTART(
    "TC_MaxDelayPriority::TC_MaxDelayPriority(const TC_MaxDelayPriority&)" );
};


/*
  Destructor.
*/
TC_MaxDelayPriority::~TC_MaxDelayPriority( void )
{
  DSTART( "virtual TC_MaxDelayPriority::~TC_MaxDelayPriority()" );
};


/*
  Copy-assignment operator.
*/
TC_MaxDelayPriority & TC_MaxDelayPriority::operator = ( const TC_MaxDelayPriority &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_MaxDelayPriority::operator = ( __o );
  TC_SchedulingPriority::operator = ( __o );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a TriCore maximum delay scheduling priority.
*/
WIR_SchedulingPriority *TC_MaxDelayPriority::clone( void ) const
{
  DSTART(
    "virtual WIR_SchedulingPriority* TC_MaxDelayPriority::clone() const" );

  return( new TC_MaxDelayPriority( *this ) );
};

}       // namespace WIR
