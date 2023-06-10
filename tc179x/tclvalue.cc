/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2020 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

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

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include local headers
#include <tc179x/instructionfactory.h>
#include <tc179x/registrar.h>
#include <tc179x/tcaddressmodification.h>
#include <tc179x/tclvalue.h>


//
// Code section
//


using namespace std;
using namespace WIR;


//
// Public class methods
//

/*
  Constructor initializing an empty lvalue.
*/
TC_LValue::TC_LValue( void ) :
  LValue {}
{
  DSTART( "TC_LValue::TC_LValue()" );

  mAddress.reset( new TC_AddressModification {} );
};


/*
  Constructor initializing a data object label-based lvalue.
*/
TC_LValue::TC_LValue( LLIR_Register *reg, const std::string &l,
                      WIR::WIR_VirtualRegister *r, const WIR::WIR_Data &d,
                      IR_Type *t ) :
  LValue { reg, r }
{
  DSTART(
    "TC_LValue::TC_LValue(LLIR_Register*, const string&, WIR_VirtualRegister*, const WIR_Data&, IR_Type*)" );

  mAddress.reset( new TC_AddressModification { l, d, t, true } );
};


/*
  Constructor initializing a function label-based lvalue.
*/
TC_LValue::TC_LValue( LLIR_Register *reg, const std::string &l,
                      WIR::WIR_VirtualRegister *r,
                      const WIR::WIR_Function &f, IR_Type *t ) :
  LValue { reg, r }
{
  DSTART(
    "TC_LValue::TC_LValue(LLIR_Register*, const string&, WIR_VirtualRegister*, const WIR_Function&, IR_Type*)" );

  mAddress.reset( new TC_AddressModification { l, f, t, true } );
};


/*
  Constructor initializing a base/offset-based lvalue with base address pre/post
  incr/decr.
*/
TC_LValue::TC_LValue( LLIR_Register *reg, WIR::WIR_VirtualRegister *r,
                      const TC_AddressModification &am ) :
  LValue { reg, r, am }
{
  DSTART(
    "TC_LValue::TC_LValue(LLIR_Register*, WIR_VirtualRegister*, const TC_AddressModification&)" );
};


/*
  Constructor initializing a bitfield-based lvalue.
*/
TC_LValue::TC_LValue( LLIR_Register *reg, WIR::WIR_VirtualRegister *r,
                      const TC_AddressModification &am, unsigned char bo,
                      unsigned char bl ) :
  LValue { reg, r, am, bo, bl }
{
  DSTART(
    "TC_LValue::TC_LValue(LLIR_Register*, WIR_VirtualRegister*, const TC_AddressModification&, unsigned char, unsigned char)" );
};


/*
  Copy constructor.
*/
TC_LValue::TC_LValue( const TC_LValue &am ) :
  LValue { am }
{
  DSTART( "TC_LValue::TC_LValue(const TC_LValue&)" );
};


/*
  Destructor.
*/
TC_LValue::~TC_LValue( void )
{
  DSTART( "virtual TC_LValue::~TC_LValue()" );
};


/*
  Assignment operator.
*/
TC_LValue & TC_LValue::operator = ( const TC_LValue &am )
{
cout << BOOST_CURRENT_FUNCTION << endl;
  DSTART( BOOST_CURRENT_FUNCTION );

  LValue::operator = ( am );

  return( *this );
};


/*
  getAddress returns the register that holds the base of the memory access plus
  any modification that should be applied to it.
*/
TC_AddressModification &TC_LValue::getAddress( void ) const
{
  DSTART( "TC_AddressModification& TC_LValue::getAddress() const" );

  return( dynamic_cast<TC_AddressModification &>( *mAddress ) );
};


/*
  storeBack stores a register value back into memory.
*/
void TC_LValue::storeBack( IR_Exp *exp, LLIR_Register *source,
                           WIR::WIR_VirtualRegister *src )
{
  DSTART(
    "void TC_LValue::storeBack(IR_Exp*, LLIR_Register*, WIR_VirtualRegister*)" );

  // Let the AddressModification handle this.
  getAddress().createStore(
    ( source == nullptr ) ? mResultRegister : source,
    ( src == nullptr ) ? mRReg : src, exp, mIsBitfieldAccess, mBitfieldOffset,
    mBitfieldLength );
};


/*
  getBaseOffsetForm transforms a memory access into base + offset form.

  If the data location is given in label form, getBaseOffsetForm issues some
  instructions realizing the transformation to base + offset form.
*/
TC_AddressWithOffset TC_LValue::getBaseOffsetForm( const IR_Exp *exp ) const
{
  DSTART(
    "TC_AddressWithOffset TC_LValue::getBaseOffsetForm(const IR_Exp*) const" );

  if ( mAddress->containsAReg() )
    return(
      TC_AddressWithOffset {
        mAddress->getARegister(),
        dynamic_cast<TC_ARegV &>( mAddress->getAReg() ),
        mAddress->getOffset() } );

  // The address of global variables is given as label.
  // -> Convert it to base + address format.

  // LLIR
  auto *iaddr = TCINSTRUCTIONS.CreateRegister( "", true );
  TCINSTRUCTIONS.insertMOVH_A(
    iaddr, OPER_LAB_HI, mAddress->getDataLabel(), exp );
  TCINSTRUCTIONS.insertLEA(
    iaddr, OPER_BASE, iaddr, OPER_LAB_LO, mAddress->getDataLabel(), exp );

  // WIR
  auto &r = TCINSTRUCTIONS.createAReg();
  if ( mAddress->isDataLabel() ) {
    TCINSTRUCTIONS.insertMOVH_A(
      r, dynamic_cast<WIR_Data &>( *(mAddress->getLabel()) ), exp );
    TCINSTRUCTIONS.insertLEA(
      r, r, dynamic_cast<WIR_Data &>( *(mAddress->getLabel()) ), exp );
  } else {
    TCINSTRUCTIONS.insertMOVH_A(
      r, dynamic_cast<WIR_Function &>( *(mAddress->getLabel()) ), exp );
    TCINSTRUCTIONS.insertLEA(
      r, r, dynamic_cast<WIR_Function &>( *(mAddress->getLabel()) ), exp );
  }

  return( TC_AddressWithOffset { iaddr, r, 0 } );
};


/*
  convertToBaseOffsetForm uses getBaseOffsetForm to convert a label-based lvalue
  into base + offset form.
*/
void TC_LValue::convertToBaseOffsetForm( const IR_Exp *exp )
{
  DSTART( "void TC_LValue::convertToBaseOffsetForm(const IR_Exp*)" );

  if ( mAddress->getLabel() != nullptr ) {
    auto boForm = getBaseOffsetForm( exp );

    mAddress.reset(
      new TC_AddressModification {
        boForm.getARegister(), boForm.getAReg(), boForm.getOffset(), nullptr,
        true, TC_AddressModification::ModTime::NONE,
        TC_AddressModification::ModOper::ADD } );
  }
};


/*
  For the given bitfield offset and length, getBitsInNextWord returns the number
  of bits which span into the next word.
*/
int TC_LValue::getBitsInNextWord( int bo, int bl )
{
  DSTART( "static int TC_LValue::getBitsInNextWord(int, int)" );

  ufAssert( bo < 32 );

  const int bits = bo + bl - 32;
  return( bits >= 0 ? bits : 0 );
};


//
// Protected class methods
//

/*
  clone creates a copy of an lvalue.

  clone just calls the corresponding copy constructor.
*/
LValue *TC_LValue::clone( void ) const
{
  DSTART( "virtual LValue* TC_LValue::clone() const" );

  return( new TC_LValue( *this ) );
};
