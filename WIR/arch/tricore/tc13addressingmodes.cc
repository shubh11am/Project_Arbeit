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
  @file tc13addressingmodes.cc
  @brief This file declares the Infineon TriCore TC13's addressing modes.

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
// Global initializations
//

// Addressing modes
const TC13::AddressingMode TC13::AddressingMode::pre  { string { "preincr" } };
const TC13::AddressingMode TC13::AddressingMode::post { string { "postincr" } };


/*
  getProcessorTypeName returns a string containing the C++-mangled name of the
  TC13 class to which an addressing mode belongs.

  This method is only used for comparing addressing modes of different processor
  architectures in WIR_Operation::checkParameters().
*/
std::string TC13::AddressingMode::getProcessorTypeName( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( string( typeid( TC13 ).name() ) );
};

}       // namespace WIR
