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
  @file tcpswbitphysical.cc
  @brief This file implements physical TriCore Processor Status Word (PSW) bits.

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
TC_PSWBit::~TC_PSWBit( void )
{
  DSTART( "virtual TC_PSWBit::~TC_PSWBit()" );
};


//
// Private class methods
//

/*
  Default constructor for physical Processor Status Word (PSW) bits.
*/
TC_PSWBit::TC_PSWBit( const std::string &__s, bool __sp ) :
  WIR_PhysicalRegister { TC13::RegisterType::pswBit, __s, __sp }
{
  DSTART( "TC_PSWBit::TC_PSWBit(const string&, bool)" );
};


/*
  Copy constructor.
*/
TC_PSWBit::TC_PSWBit( const TC_PSWBit &__o ) :
  WIR_PhysicalRegister { __o }
{
  DSTART( "TC_PSWBit::TC_PSWBit(const TC_PSWBit&)" );
};


/*
  Copy-assignment operator.
*/
TC_PSWBit & TC_PSWBit::operator = ( const TC_PSWBit &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_PhysicalRegister::operator = ( __o );

  return( *this );
};


/*
  clone creates a copy of a physical TriCore Processor Status Word bit.
*/
TC_PSWBit *TC_PSWBit::clone( void ) const
{
  DSTART( "virtual TC_PSWBit* TC_PSWBit::clone() const" );

  return( new TC_PSWBit( *this ) );
};

}       // namespace WIR
