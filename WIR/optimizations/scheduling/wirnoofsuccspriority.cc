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
  @file wirnoofsuccspriority.h
  @brief This file implements an abstract base class computing the number of
         successor operations scheduling priority for %WIR operations.

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
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>

// Include local headers
#include "wirnoofsuccspriority.h"
#include "wirschedulingregion.h"


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
WIR_NoOfSuccsPriority::WIR_NoOfSuccsPriority( void ) :
  WIR_SchedulingPriority {}
{
  DSTART( "WIR_NoOfSuccsPriority::WIR_NoOfSuccsPriority()" );
};


/*
  Copy constructor.
*/
WIR_NoOfSuccsPriority::WIR_NoOfSuccsPriority( const WIR_NoOfSuccsPriority &__o ) :
  WIR_SchedulingPriority { __o }
{
  DSTART(
    "WIR_NoOfSuccsPriority::WIR_NoOfSuccsPriority(const WIR_NoOfSuccsPriority&)" );
};


/*
  Destructor.
*/
WIR_NoOfSuccsPriority::~WIR_NoOfSuccsPriority( void )
{
  DSTART( "virtual WIR_NoOfSuccsPriority::~WIR_NoOfSuccsPriority()" );
};


/*
  Copy-assignment operator.
*/
WIR_NoOfSuccsPriority & WIR_NoOfSuccsPriority::operator = ( const WIR_NoOfSuccsPriority &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_SchedulingPriority::operator = ( __o );

  return( *this );
};


/*
  getPriority determines an operation's scheduling priority using the number of
  successors of an operation in the dependence graph.
*/
long long WIR_NoOfSuccsPriority::getPriority( const WIR_Operation &o ) const
{
  DSTART(
    "virtual long long int WIR_NoOfSuccsPriority::getPriority(const WIR_Operation&) const" );

  return( mRegion->getNumberOfSuccessors( o ) );
};

}       // namespace WIR
