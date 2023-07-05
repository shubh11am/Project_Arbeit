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
  @file wirphysicalregister.cc
  @brief This file implements physical %WIR registers.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include standard headers
#include <iostream>

// Include boost headers
#include <boost/current_function.hpp>

// Include libuseful headers
#include <libuseful/debugmacros.h>

// Include WIR headers
#include <wir/wir.h>


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
WIR_PhysicalRegister::~WIR_PhysicalRegister( void )
{
  DSTART( "virtual WIR_PhysicalRegister::~WIR_PhysicalRegister()" );
};


//
// Protected class methods
//

/*
  Default constructor for physical registers.
*/
WIR_PhysicalRegister::WIR_PhysicalRegister( const WIR_BaseProcessor::RegisterType &__r,
                                            const std::string &__s,
                                            const bool __sp ) :
  WIR_Register<WIR_PhysicalRegister, false> { __r, __s, __sp }
{
  DSTART(
    "WIR_PhysicalRegister::WIR_PhysicalRegister(const WIR_BaseProcessor::RegisterType&, const string&, bool)" );
};


/*
  Copy constructor.

  When copying a register that is inserted in some WIR processor, the resulting
  copy will not be inserted in a processor.
*/
WIR_PhysicalRegister::WIR_PhysicalRegister( const WIR_PhysicalRegister &__o ) :
  WIR_Register<WIR_PhysicalRegister, false> { __o }
{
  DSTART(
    "WIR_PhysicalRegister::WIR_PhysicalRegister(const WIR_PhysicalRegister&)" );
};


/*
  Copy-assignment operator.

  When copying a register that is inserted in some %WIR processor, the resulting
  copy will not be inserted in a processor.
*/
WIR_PhysicalRegister & WIR_PhysicalRegister::operator = ( const WIR_PhysicalRegister &__o )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  WIR_Register<WIR_PhysicalRegister, false>::operator = ( __o );

  return( *this );
};

}       // namespace WIR
