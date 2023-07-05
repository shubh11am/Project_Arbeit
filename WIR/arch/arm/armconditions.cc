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
  @file armconditions.cc
  @brief This file declares the ARM's condition codes.

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
#include <arch/arm/armbase.h>


//
// Code section
//

namespace WIR {


using namespace std;


//
// Global initializations
//

// Condition codes
const ARM_Base::Condition ARM_Base::Condition::eq { string { "eq" } };
const ARM_Base::Condition ARM_Base::Condition::ne { string { "ne" } };
const ARM_Base::Condition ARM_Base::Condition::hs { string { "hs" } };
const ARM_Base::Condition ARM_Base::Condition::lo { string { "lo" } };
const ARM_Base::Condition ARM_Base::Condition::mi { string { "mi" } };
const ARM_Base::Condition ARM_Base::Condition::pl { string { "pl" } };
const ARM_Base::Condition ARM_Base::Condition::vs { string { "vs" } };
const ARM_Base::Condition ARM_Base::Condition::vc { string { "vc" } };
const ARM_Base::Condition ARM_Base::Condition::hi { string { "hi" } };
const ARM_Base::Condition ARM_Base::Condition::ls { string { "ls" } };
const ARM_Base::Condition ARM_Base::Condition::ge { string { "ge" } };
const ARM_Base::Condition ARM_Base::Condition::lt { string { "lt" } };
const ARM_Base::Condition ARM_Base::Condition::gt { string { "gt" } };
const ARM_Base::Condition ARM_Base::Condition::le { string { "le" } };
const ARM_Base::Condition ARM_Base::Condition::al { string { "al" } };


/*
  getProcessorTypeName returns a string containing the C++-mangled name of the
  ARM class to which a condition belongs.

  This method is only used for comparing addressing modes of different processor
  architectures in WIR_Operation::checkParameters().
*/
std::string ARM_Base::Condition::getProcessorTypeName( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( string( typeid( ARM_Base ).name() ) );
};

}       // namespace WIR
