/*

   This header file belongs to the

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
  @file wirreachability.h
  @brief This file provides the interface of a %WIR container representing
         reachability control flow information.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REACHABILITY_H
#define _WIR_REACHABILITY_H


//
// Include section
//

// Include standard headers
#include <set>

// Include WIR headers
#include <wir/wircontainer.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Instruction;


/*!
  @brief Class WIR_Reachability models reachability between basic blocks.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Reachability : public WIR_Container<WIR_Reachability>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in] b A const reference to a %WIR basic block to which this
                   container is attached.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Reachability( const WIR_BasicBlock & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Reachability( const WIR_Reachability & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Reachability( WIR_Reachability && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Reachability( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Reachability & operator = ( const WIR_Reachability & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Reachability & operator = ( WIR_Reachability && );

    /*!
      @brief isUnique returns whether reachability information is unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always true, reachability information is unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Reachable block handling.
    //

    /*!
      @brief addReachableBlock adds the specified basic block to the set of
             reachable blocks.

      @param[in] b A reference to a new reachable basic block.
      @param[in] be A Boolean defaulting to true that denotes whether the given
                    basic block shall be added to the set of basic blocks
                    reachable via back edges (true) or to the set of basic
                    blocks reachable only via non-back edges (false).

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addReachableBlock( WIR_BasicBlock &, bool = true );

    /*!
      @brief getReachableBlocks returns the set of reachable basic blocks.

      @param[in] be A Boolean defaulting to true that denotes whether the set of
                    basic blocks reachable via back edges shall be returned
                    (true) or the set of basic blocks reachable only via
                    non-back edges (false).
      @return A reference to the set containing all reachable basic blocks.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_BasicBlockSet &getReachableBlocks( bool = true ) const;

    /*!
      @brief isReachable determines whether the given basic block is reachable
             via a control flow path starting in that basic block to which this
             container is attached.

      @param[in] b A const reference to a %WIR basic block whose reachability is
                   queried.
      @param[in] be A Boolean defaulting to true that denotes whether potential
                    control flow paths to b may contain CFG back edges (true)
                    or not (false).
      @return true if b is reachable from the block holding this container,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isReachable( const WIR_BasicBlock &, bool = true ) const;

    /*!
      @brief This static variant of isReachable determines whether the two given
             instructions are reachable via a control flow path starting in i1
             and ending in i2.

      @param[in] i1 A const reference to a %WIR instruction serving as start
                    point of reachability.
      @param[in] i2 A const reference to a %WIR instruction for which it shall
                    be determined whether it is reachable from i1.
      @param[in] be A Boolean defaulting to true that denotes whether potential
                    control flow paths between i1 and i2 may contain CFG back
                    edges (true) or not (false).
      @return true if i2 is reachable from i1, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isReachable( const WIR_Instruction &, const WIR_Instruction &,
                             bool = true );


  private:

    /*!
      @brief No standard construction allowed, users must use one of the above
             default constructors instead.
    */
    WIR_Reachability( void ) = delete;

    /*!
      @brief mBB points to that %WIR basic block to which this container is
             attached.
    */
    WIR_BasicBlock *mBB;

    /*!
      @brief mReachableBlocks stores (wrapped) references to all other reachable
             basic blocks.
    */
    WIR_BasicBlockSet mReachableBlocks;

    /*!
      @brief mReachableBlocks stores the IDs of all other reachable basic
             blocks.
    */
    std::set<WIR_id_t> mReachableBlockIDs;

    /*!
      @brief mReachableBlocksWOBackEdges stores (wrapped) references to all
             other basic blocks that are reachable, but not via back edges in
             the CFG.
    */
    WIR_BasicBlockSet mReachableBlocksWOBackEdges;

    /*!
      @brief mReachableBlockIDsWOBackEdges stores the IDs of all other basic
             blocks that are reachable, but not via back edges in the CFG.
    */
    std::set<WIR_id_t> mReachableBlockIDsWOBackEdges;

};

}       // namespace WIR

#endif  // _WIR_REACHABILITY_H
