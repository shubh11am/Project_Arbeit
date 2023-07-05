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
  @file tceregphysical.cc
  @brief This file implements physical TriCore extended data registers.

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
TC_ERegP::~TC_ERegP( void )
{
  DSTART( "virtual TC_ERegP::~TC_ERegP()" );
};


//
// Private class methods
//

/*
  Default constructor for physical extended data registers.
*/
TC_ERegP::TC_ERegP( const std::string &__s, bool __sp ) :
  WIR_PhysicalRegister { TC13::RegisterType::eReg, __s, __sp }
{
  DSTART( "TC_ERegP::TC_ERegP(const string&, bool)" );
};


/*
  Copy constructor.
*/
TC_ERegP::TC_ERegP( const TC_ERegP &__o ) :
  WIR_PhysicalRegister { __o }
{
  DSTART( "TC_ERegP::TC_ERegP(const TC_ERegP&)" );
};


/*
  Copy-assignment operator.
*/
TC_ERegP & TC_ERegP::operator = ( const TC_ERegP &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_PhysicalRegister::operator = ( __o );

  return( *this );
};


/*
  Upon creation of TriCore physical register pairs, addChilds adds the two
  specified physical registers as childs.
*/
void TC_ERegP::addChilds( const WIR_PhysicalRegister &c1,
                          const WIR_PhysicalRegister &c2 )
{
  DSTART(
    "void TC_ERegP::addChilds(const WIR_PhysicalRegister&, const WIR_PhysicalRegister&)" );

  pushBackChild( const_cast<WIR_PhysicalRegister &>( c1 ) );
  pushBackChild( const_cast<WIR_PhysicalRegister &>( c2 ) );
};


/*
  clone creates a copy of a physical TriCore extended data register.
*/
TC_ERegP *TC_ERegP::clone( void ) const
{
  DSTART( "virtual TC_ERegP* TC_ERegP::clone() const" );

  return( new TC_ERegP( *this ) );
};

}       // namespace WIR
