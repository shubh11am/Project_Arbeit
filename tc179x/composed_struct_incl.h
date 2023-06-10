/*

   This header file belongs to the

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


#ifndef _COMPOSED_STRUCT_INCL_H_
#define _COMPOSED_STRUCT_INCL_H_


//
// Include section
//

// Include local headers
#include "composed_struct_bitfield_incl.h"


//
// Class forward declarations
//

class IR_ComposedType;
class IR_Exp;
class IR_SymbolExp;


//
// Header section
//

/*!
  If the symExp denotes a composed type element, this method returns the type
  of the composed type. Otherwise, nullptr is returned.
*/
inline IR_ComposedType *getParentComposedType( const IR_SymbolExp & );


/*!
  Returns whether the exp denotes a composed type element.
*/
inline bool isComponentExp( const IR_Exp & );

#endif
