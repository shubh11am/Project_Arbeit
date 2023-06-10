/*

   This source file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2009 - 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/


RegPair::RegPair( void ) :
  reg0 { nullptr },
  reg1 { nullptr },
  r1 { ref( dummyDRegV ) },
  r2 { ref( dummyDRegV ) }
{
};


RegPair::RegPair( LLIR_Register *p1, LLIR_Register *p2,
                  const reference_wrapper<WIR::TC_DRegV> &p3,
                  const reference_wrapper<WIR::TC_DRegV> &p4 ) :
  reg0 { p1 },
  reg1 { p2 },
  r1 { p3 },
  r2 { p4 }
{
};


RegConst::RegConst( void ) :
  reg0 { nullptr },
  r { ref( dummyDRegV ) },
  constant { 0 }
{
};


RegConst::RegConst( LLIR_Register *p1,
                    const reference_wrapper<WIR::TC_DRegV> &p2,
                    unsigned long c ) :
  reg0 { p1 },
  r { p2 },
  constant { c }
{
};


/*
  checkSignednessForMADD checks whether the given IR expression has a form that
  allows to generate MADD instructions for it.

  In more detail, checkSignednessForMADD checks whether:
   - 'exp' is a binary PLUS or MINUS expression (a +/- b)
   - its second operand is a multiplication (a +/- (b * c)), and
   - 'a', 'b' and 'c' have the same sign.
*/
bool checkSignednessForMADD( const IR_Exp &exp )
{
  DSTART( "bool checkSignednessForMADD(const IR_Exp&)" );

  auto *bexp = dynamic_cast<const IR_BinaryExp *>( &exp );

  if ( bexp &&
       ( ( bexp->getOperator() == IR_BinaryExp::PLUS ) ||
         ( bexp->getOperator() == IR_BinaryExp::MINUS ) ) ) {
    auto *multExp = dynamic_cast<const IR_BinaryExp *>( &bexp->getOp2() );

    if ( multExp &&
         ( multExp->getOperator() == IR_BinaryExp::MULT ) )
      if ( ( bexp->getOp1().getType().isSignedType() ==
               multExp->getOp1().getType().isSignedType() ) &&
           ( multExp->getOp1().getType().isSignedType() ==
               multExp->getOp2().getType().isSignedType() ) )
          return( true );
  }

  return( false );
};
