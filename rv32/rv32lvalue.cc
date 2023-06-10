/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2021 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file rv32lvalue.cc
  @brief This file implements the RISC-V RV32 class describing lvalues stored in
         memory.
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wcc.h>
#endif

// Include boost headers
#include <boost/current_function.hpp>

// Include WIR headers
#include <wir/wir.h>
#include <arch/riscv/rv32imc.h>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include local headers
#include <rv32/rv32addressmodification.h>
#include <codesel/addresswithoffset.h>
#include <rv32/rv32lvalue.h>
#include <rv32/rv32instructionfactory.h>
#include <rv32/rv32registrar.h>
#include <rv32/rv32_incl.h>

//
// Code section
//


using namespace std;
using namespace WIR;


namespace RV32 {

//
// Public class methods
//

/*
  Constructor initializing an empty lvalue.
*/
RV32_LValue::RV32_LValue( void ) :
  LValue {}
{
  DSTART( "RV32_LValue::RV32_LValue()" );

  mAddress.reset( new RV32_AddressModification {} );
};


/*
  Constructor initializing a base/offset-based lvalue with base address pre/post
  incr/decr.
*/
RV32_LValue::RV32_LValue( WIR::WIR_VirtualRegister *r,
                          const RV32_AddressModification &am ) :
  LValue { nullptr, r, am }
{
  DSTART( "RV32_LValue::RV32_LValue(WIR_VirtualRegister*, const "
          "RV32_AddressModification&)" );
};


/*
  Constructor initializing a data object label-based lvalue.
*/
RV32_LValue::RV32_LValue( const std::string &l, WIR::WIR_VirtualRegister *r, const WIR::WIR_Data &d,
                      IR_Type *t ) :
  LValue { nullptr, r }
{
  DSTART( "RV32_LValue::RV32_LValue(WIR_VirtualRegister*, const WIR_Data&, "
          "IR_Type*)" );

  mAddress.reset( new RV32_AddressModification { l, d, t, true } );
};


/*
  Constructor initializing a function label-based lvalue.
*/
RV32_LValue::RV32_LValue( WIR::WIR_VirtualRegister *r,
                      const WIR::WIR_Function &f, IR_Type *t ) :
  LValue { nullptr, r }
{
  DSTART( "RV32_LValue::RV32_LValue(WIR_VirtualRegister*, const WIR_Function&, "
          "IR_Type*)" );

  mAddress.reset( new RV32_AddressModification { f, t, true } );
};


/*
  Copy constructor.
*/
RV32_LValue::RV32_LValue( const RV32_LValue &am ) :
  LValue { am }
{
  DSTART( "RV32_LValue::RV32_LValue(const RV32_LValue&)" );
};


/*
  Destructor.
*/
RV32_LValue::~RV32_LValue( void )
{
  DSTART( "virtual RV32_LValue::~RV32_LValue()" );
};


/*
  Assignment operator.
*/
RV32_LValue & RV32_LValue::operator = ( const RV32_LValue &am )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  LValue::operator = ( am );

  return( *this );
};


/*
  getAddress returns the register that holds the base of the memory access plus
  any modification that should be applied to it.
*/
RV32_AddressModification &RV32_LValue::getAddress( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( dynamic_cast<RV32_AddressModification &>( *mAddress ) );
};


/*
  storeBack stores a register value back into memory.
*/
void RV32_LValue::storeBack( IR_Exp *exp, WIR::WIR_VirtualRegister *src )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  // Let the AddressModification handle this.
  getAddress().createStore(
    ( src == nullptr ) ? mRReg : src, exp, mIsBitfieldAccess, mBitfieldOffset,
    mBitfieldLength );
};


/*
  getBaseOffsetForm transforms a memory access into base + offset form.

  If the data location is given in label form, getBaseOffsetForm issues some
  instructions realizing the transformation to base + offset form.
*/
RV32_AddressWithOffset RV32_LValue::getBaseOffsetForm( const IR_Exp *exp )
{
  DSTART(
    "TC_AddressWithOffset TC_LValue::getBaseOffsetForm(const IR_Exp*) const" );

  if ( mAddress->containsAReg() ){
    auto temp = RV32_AddressWithOffset { mAddress->getAReg(), mAddress->getOffset() };
    return(
       temp );}

  // The address of global variables is given as label.
  // -> Convert it to base + address format.


  // WIR
  WIR::RV_RegV &r = RVINSTRUCTIONS.createReg();
  if ( mAddress->isDataLabel() ) {
    RVINSTRUCTIONS.insertLUI(
      r, dynamic_cast<WIR_Data &>( *(mAddress->getLabel()) ), exp );
    RVINSTRUCTIONS.insertADDI(
      r, r, dynamic_cast<WIR_Data &>( *(mAddress->getLabel()) ), exp );
  } else {
    //functions not yet implemented here
    //RVINSTRUCTIONS.insertLUI(
    //  r, dynamic_cast<WIR_Function &>( *(mAddress->getLabel()) ), exp );
    //RVINSTRUCTIONS.insertADDI(
    //  r, r, dynamic_cast<WIR_Function &>( *(mAddress->getLabel()) ), exp );
    ufAssertT( true, "unctions not yet implemented in getBaseOffsetForm" );
  }
  auto temp = RV32_AddressWithOffset { r , 0 };
  return( temp );
};


/*
  convertToBaseOffsetForm uses getBaseOffsetForm to convert a label-based lvalue
  into base + offset form.
*/
void RV32_LValue::convertToBaseOffsetForm( const IR_Exp *exp )
{
  DSTART( "void RV32_LValue::convertToBaseOffsetForm(const IR_Exp*)" );

  if ( mAddress->getLabel() != nullptr ) {
    auto boForm = getBaseOffsetForm( exp );

    mAddress.reset(
      new RV32_AddressModification {
        boForm.getAReg(), boForm.getOffset(), &exp->getType(),
        true} );
  }
};


/*
  For the given bitfield offset and length, getBitsInNextWord returns the number
  of bits which span into the next word.
*/
int RV32_LValue::getBitsInNextWord( int bo, int bl )
{
  DSTART( BOOST_CURRENT_FUNCTION );

  ufAssert( bo < 32 );

  const int bits = bo + bl - 32;
  return( bits >= 0 ? bits : 0 );
};


/*
  calculateAddress applies the pointer arithmetics denoted by the embedded
  RV32_AddressModification object and returns the calculated register.

  calculateAddress requires that createLoad/createStore wasn't applied to the
  RV32_AddressModification object yet.
*/
WIR::RV_RegV* RV32::RV32_LValue::calculateAddress( IR_Exp *exp )
{
  DSTART( "WIR_BaseRegister RV32_LValue::calculateAddress(IR_Exp*)" );

  // Never call this on a dry run instance.
  //ufAssertT(
  //  !mDryRun, "Do not call 'calculateAddress' on a dry run instance." );

  // Pass this through to the RV32_AddressModification.
  return( getAddress().applyModification( exp ) );
};


/*
  calculateAddressCost returns the costs for the pointer arithmetics encoded by
  this object.
*/
COST RV32::RV32_LValue::calculateAddressCost( void ) const
{
  DSTART( "COST RV32_LValue::calculateAddressCost() const" );

  // Let the ARM_AddressModification answer this.
  return( getAddress().applyModificationCost() );
};


//
// Protected class methods
//

/*
  clone creates a copy of an lvalue.

  clone just calls the corresponding copy constructor.
*/
LValue *RV32_LValue::clone( void ) const
{
  DSTART( BOOST_CURRENT_FUNCTION );

  return( new RV32_LValue( *this ) );
};

}       // namespace RV32
