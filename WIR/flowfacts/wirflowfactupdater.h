/*

   This header file belongs to the

            Hamburg University of Technology (TUHH)
              WCC Compiler Framework

   and is property of its respective copyright holder. It must neither be used
   nor published even in parts without explicit written permission.

   Copyright 2022

   Hamburg University of Technology (TUHH)
   Institute of Embedded Systems
   21071 Hamburg
   Germany

   http://www.tuhh.de/es/esd/research/wcc

*/

/*!
  @file wirflowfactupdater.h
  @brief This file provides the interface of various functions to update and
         delete %WIR flow facts.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/


#ifndef _WIR_FLOWFACTUPDATER_H
#define _WIR_FLOWFACTUPDATER_H


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;


/*!
  @brief Namespace WIR_FlowFactUpdater provides update mechanisms for %WIR flow
         facts.

  Because of the strong dependence of flow facts on a program's control flow,
  flow facts have to be updated every time the (control flow of the) program may
  be changed. For this purpose, WIR_FlowFactUpdater provides some functions
  which have to be used in %WIR optimizations. The programmer of such
  optimizations is responsible for the correct use of the update mechanisms.

  @warning A wrong or a forgotten update of flow facts within a %WIR
           optimization could result in unsafe WCET estimates without any
           warning or hint!

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
namespace WIR_FlowFactUpdater {

//
// Update of flow restrictions
//

/*!
  @brief replaceBasicBlock replaces a basic block in flow restrictions by
         another one.

  @param b A reference to the basic block to be replaced in flow restrictions.
  @param bNew A reference to the new basic block the flow restrictions will
              refer to.

  replaceBasicBlock should be used if a basic block is available which has
  exactly the same execution frequency as the original one has had, and if both
  basic blocks belong to the same function of the programm.

  replaceBasicBlock examines all flow restrictions that b refers to via a
  WIR_FlowFactRef and replaces all occurrences of b by bNew therein.

  Loop bounds are not touched.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
void replaceBasicBlock( WIR_BasicBlock &b, WIR_BasicBlock &bNew );


/*!
  @brief estimateBasicBlock estimates a basic block in flow restrictions by
         another one or by zero.

  @param b A reference to the basic block to be replaced in flow restrictions.
  @param bNew A reference to the basic block to which flow restrictions might
               refer to, if applicable as replacement.

  estimateBasicBlock should be used if no basic blocks with the same execution
  frequency as b are available.

  All occurrences of b in the less-equal sides of flow restrictions will be
  estimated by zero, occurrences in the greater-equal sides will be replaced by
  bNew. For this purpose, the execution frequency of bNew has to be
  greater-equal than the one of b.

  Loop bounds are not touched.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
void estimateBasicBlock( WIR_BasicBlock &b, WIR_BasicBlock &bNew );


/*!
  @brief eraseFlowRestrictions erases all flow restrictions referring to a given
         basic block.

  @param b A reference to the basic block whose flow restrictions shall be
           erased.

  eraseFlowRestrictions does not erase a reference of a flow restriction to the
  given basic block, but it instead erases the entire flow restriction as a
  whole.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
void eraseFlowRestrictions( WIR_BasicBlock &b );


//
// Update of both flow restrictions and loop bounds
//

/*!
  @brief eraseUnreachableBasicBlock updates flow facts for basic blocks which
         are never executed.

  @param b A reference to an unreachable basic block whose flow facts shall be
           erased.

  An unreachable basic block has an execution frequency of zero. Due to this, a
  loop bound can simply be deleted (never executed). Furthermore, each reference
  to this basic block in flow restrictions can be replaced by zero, i.e., the
  corresponding summand of the flow restriction can be erased.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
void eraseUnreachableBasicBlock( WIR_BasicBlock &b );


//
// Update of loop bounds
//

/*!
  @brief moveLoopBounds moves all loop bounds from one loop to another.

  @param b A reference to a basic block from which loop bounds shall be moved.
  @param bNew A reference to a basic block to which loop bounds shall be moved.

  All loop bounds of a loop given by b will be moved to the loop denoted by bNew
  (which may also be the same loop).

  @warning: It is in the programmer's responsibility to ensure that the target
            basic block bNew is member of an unannotated loop.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
void moveLoopBounds( WIR_BasicBlock &b, WIR_BasicBlock &bNew );

/*!
  @brief createLoopBound creates a loop bound for a given basic block.

  @param b A reference to a basic block for which a new loop bound is created.
  @param min An integer denoting the minimum number of iterations of the loop.
  @param max An integer denoting the maxinum number of iterations of the loop.

  It is in the programmer's responsibility to ensure that:
  - the given basic block b is part of a loop;
  - that loop is not already annotated by another loop bound;
  - that loop is not a multientry loop, because the WCET analyzer aiT does not
    recognize them;
  - a head-controlled loop is annotated with n+1 if the loop body executes for n
    iterations.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
void createLoopBound( WIR_BasicBlock &b, int min, int max );

/*!
  @brief eraseLoopBound erases all loop bounds referring to a given basic
         block.

  @param b A reference to the basic block whose loop bounds shall be erased.

  Applying eraseLoopBound does not mean that the loop is guaranteed to be no
  longer annotated any more, because another basic block of this loop may be
  annotated.

  @author Til Mauersberger <Til.Mauersberger@tuhh.de>
*/
void eraseLoopBound( WIR_BasicBlock &b );

}       // namespace WIR_FlowFactUpdater

}       // namespace WIR

#endif  // _WIR_FLOWFACTUPDATER_H
