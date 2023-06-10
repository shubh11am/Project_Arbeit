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

#ifndef _ARM7_CASTING_INCL_H
#define _ARM7_CASTING_INCL_H

/*! Returns either the implicit cast type of the expression, or, if that does
    not exist, then it returns the true type of the expression. */
inline IR_Type &effectiveType( const IR_Exp &exp )
{
  IR_Type *ret;
  return ( ret = exp.getImplicitCastType() ) ? *ret : exp.getType();
}

#endif  // _ARM7_CASTING_INCL_H
