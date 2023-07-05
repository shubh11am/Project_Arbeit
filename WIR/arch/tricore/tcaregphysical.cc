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
  @file tcaregphysical.cc
  @brief This file implements physical TriCore address registers.

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
#include <arch/tricore/tc13.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Destructor.
*/
TC_ARegP::~TC_ARegP( void )
{
  DSTART( "virtual TC_ARegP::~TC_ARegP()" );
};


//
// Private class methods
//

/*
  Default constructor for physical address registers.
*/
TC_ARegP::TC_ARegP( const std::string &__s, bool __sp ) :
  WIR_PhysicalRegister { TC13::RegisterType::aReg, __s, __sp }
{
  DSTART( "TC_ARegP::TC_ARegP(const string&, bool)" );
};


/*
  Copy constructor.
*/
TC_ARegP::TC_ARegP( const TC_ARegP &__o ) :
  WIR_PhysicalRegister { __o }
{
  DSTART( "TC_ARegP::TC_ARegP(const TC_ARegP&)" );
};


/*
  Copy-assignment operator.
*/
TC_ARegP & TC_ARegP::operator = ( const TC_ARegP &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_PhysicalRegister::operator = ( __o );

  return( *this );
};


/*
  clone creates a copy of a physical TriCore address register.
*/
TC_ARegP *TC_ARegP::clone( void ) const
{
  DSTART( "virtual TC_ARegP* TC_ARegP::clone() const" );

  return( new TC_ARegP( *this ) );
};

}       // namespace WIR
