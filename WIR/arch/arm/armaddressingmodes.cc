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
  @file armaddressingmodes.cc
  @brief This file declares the ARM's general addressing modes.

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

// Addressing modes
const ARM_Base::AddressingMode ARM_Base::AddressingMode::lsl     { string { "lsl" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::lsr     { string { "lsr" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::asr     { string { "asr" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ror     { string { "ror" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::plus    { string { "+" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::minus   { string { "-" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::pre     { string { "pre" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::post    { string { "post" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ia      { string { "ia" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ib      { string { "ib" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::da      { string { "da" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::db      { string { "db" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::fd      { string { "fd" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ed      { string { "ed" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::fa      { string { "fa" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ea      { string { "ea" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::c       { string { "c" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::x       { string { "x" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::s       { string { "s" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::f       { string { "f" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p0      { string { "p0" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p1      { string { "p1" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p2      { string { "p2" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p3      { string { "p3" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p4      { string { "p4" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p5      { string { "p5" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p6      { string { "p6" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p7      { string { "p7" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p8      { string { "p8" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p9      { string { "p9" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p10     { string { "p10" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p11     { string { "p11" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p12     { string { "p12" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p13     { string { "p13" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p14     { string { "p14" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::p15     { string { "p15" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::cpsra   { string { "a" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::cpsrf   { string { "f" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::cpsri   { string { "i" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::cpsraf  { string { "af" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::cpsrai  { string { "ai" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::cpsrfi  { string { "fi" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::cpsrafi { string { "afi" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::be      { string { "be" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::le      { string { "le" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ror0    { string { "ror #0" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ror8    { string { "ror #8" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ror16   { string { "ror #16" } };
const ARM_Base::AddressingMode ARM_Base::AddressingMode::ror24   { string { "ror #24" } };


/*
  getProcessorTypeName returns a string containing the C++-mangled name of the
  ARM class to which an addressing mode belongs.

  This method is only used for comparing addressing modes of different processor
  architectures in WIR_Operation::checkParameters().
*/
std::string ARM_Base::AddressingMode::getProcessorTypeName( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( string( typeid( ARM_Base ).name() ) );
};

}       // namespace WIR
