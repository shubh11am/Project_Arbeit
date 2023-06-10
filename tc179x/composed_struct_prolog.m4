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


include(composed_struct_bitfield_prolog.m4)

/*
  If the symExp denotes a composed type element, this method returns the type
  of the composed type. Otherwise, nullptr is returned.
*/
IR_ComposedType *getParentComposedType( const IR_SymbolExp &symExp )
{
  auto *cexp = dynamic_cast<IR_ComponentAccessExp *>( symExp.getParent() );

  if ( cexp ) {
    auto *directAccess =
      dynamic_cast<IR_ComposedType *>( &cexp->getBaseExp().getType() );
    auto *indirectAccess =
      dynamic_cast<IR_PointerType*>( &cexp->getBaseExp().getType() );
    IR_ComposedType * const compType =
      ( directAccess ?
          directAccess :
          dynamic_cast<IR_ComposedType *>( &indirectAccess->getBaseType() ) );

    return( compType );
  } else
    return( nullptr );
};


/*
  Returns whether the exp denotes a composed type element.
*/
bool isComponentExp( const IR_Exp &exp )
{
  auto *symExp = dynamic_cast<const IR_SymbolExp *>( &exp );

  if ( symExp ) {
    auto *cexp = dynamic_cast<IR_ComponentAccessExp *>( symExp->getParent() );
    return( cexp && ( &cexp->getComponentExp() == symExp ) );
  } else
    return( false );
};
