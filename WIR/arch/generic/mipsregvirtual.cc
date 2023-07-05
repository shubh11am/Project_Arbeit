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
  @file mipsregvirtual.cc
  @brief This file implements virtual MIPS integer registers.

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
#include <arch/generic/mips.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Public class methods
//

/*
  Default constructor for virtual integer registers.
*/
MIPS_RegV::MIPS_RegV( void ) :
  WIR_VirtualRegister { MIPS::RegisterType::reg }
{
  DSTART( "MIPS_RegV::MIPS_RegV()" );
};


/*
  Copy constructor.
*/
MIPS_RegV::MIPS_RegV( const MIPS_RegV &__o ) :
  WIR_VirtualRegister { __o }
{
  DSTART( "MIPS_RegV::MIPS_RegV(const MIPS_RegV&)" );
};


/*
  Move constructor.
*/
MIPS_RegV::MIPS_RegV( MIPS_RegV &&__o ) :
  WIR_VirtualRegister { move( __o ) }
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );
};


/*
  Destructor.
*/
MIPS_RegV::~MIPS_RegV( void )
{
  DSTART( "virtual MIPS_RegV::~MIPS_RegV()" );
};


/*
  Copy-assignment operator.
*/
MIPS_RegV & MIPS_RegV::operator = ( const MIPS_RegV &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_VirtualRegister::operator = ( __o );

  return( *this );
};


/*
  Move-assignment operator.
*/
MIPS_RegV & MIPS_RegV::operator = ( MIPS_RegV &&__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_VirtualRegister::operator = ( move( __o ) );

  return( *this );
};


//
// Protected class methods
//

/*
  clone creates a copy of a virtual MIPS integer register.
*/
MIPS_RegV *MIPS_RegV::clone( void ) const
{
  DSTART( "virtual MIPS_RegV* MIPS_RegV::clone() const" );

  return( new MIPS_RegV( *this ) );
};

}       // namespace WIR
