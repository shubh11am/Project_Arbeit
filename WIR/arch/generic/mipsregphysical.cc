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
  @file mipsregphysical.cc
  @brief This file implements physical MIPS integer registers.

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
  Destructor.
*/
MIPS_RegP::~MIPS_RegP( void )
{
  DSTART( "virtual MIPS_RegP::~MIPS_RegP()" );
};


//
// Private class methods
//

/*
  Default constructor for physical integer registers.
*/
MIPS_RegP::MIPS_RegP( const std::string &__s, bool __sp ) :
  WIR_PhysicalRegister { MIPS::RegisterType::reg, __s, __sp }
{
  DSTART( "MIPS_RegP::MIPS_RegP(const string&, bool)" );
};


/*
  Copy constructor.
*/
MIPS_RegP::MIPS_RegP( const MIPS_RegP &__o ) :
  WIR_PhysicalRegister { __o }
{
  DSTART( "MIPS_RegP::MIPS_RegP(const MIPS_RegP&)" );
};


/*
  Copy-assignment operator.
*/
MIPS_RegP & MIPS_RegP::operator = ( const MIPS_RegP &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_PhysicalRegister::operator = ( __o );

  return( *this );
};


/*
  clone creates a copy of a physical MIPS integer register.
*/
MIPS_RegP *MIPS_RegP::clone( void ) const
{
  DSTART( "virtual MIPS_RegP* MIPS_RegP::clone() const" );

  return( new MIPS_RegP( *this ) );
};

}       // namespace WIR
