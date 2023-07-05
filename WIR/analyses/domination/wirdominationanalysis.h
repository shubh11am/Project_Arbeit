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
  @file wirdominationanalysis.h
  @brief This file provides the interface of the %WIR domination analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DOMINATIONANALYSIS_H
#define _WIR_DOMINATIONANALYSIS_H


//
// Include section
//

// Include standard headers
#include <map>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/controlflow/wircfg.h>
#include <analyses/generic/wiranalysis.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_BitVector;
class WIR_Function;


/*!
  @brief Class WIR_DominationAnalysis is the %WIR domination analysis according
         to Andrew W. Appel, Modern Compiler Implementation in C, page 413ff or
         Steven S. Muchnick, Advanced Compiler Design and Implementation, page
         181ff and algorithm 7.14.

  Analysis results are stored in WIR_Domination containers that are attached to
  %WIR basic blocks or to both %WIR basic blocks and instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DominationAnalysis : public WIR_Analysis
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.
      @param[in] i A Boolean defaulting to false that denotes whether dominators
                   shall also be analyzed for %WIR instructions, in addition to
                   dominators at basic block-level only.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DominationAnalysis( WIR_Function &, bool = false );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_DominationAnalysis( void );


  protected:

    /*!
      @brief runAnalysis performs domination analysis by iteration of the given
             function.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runAnalysis( WIR_Function & );


  private:

    //
    // Private methods.
    //

    /*!
      @brief init initializes internal data structures by collecting information
             about predecessor/successor relations between basic blocks.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void init( WIR_Function & );

    /*!
      @brief propagateBB2Ins propagates basic block-level analysis results to
             instruction-level.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void propagateBB2Ins( WIR_Function & );

    /*!
      @brief mProblemSize stores how many basic blocks are subject to domination
             analysis.
    */
    size_t mProblemSize;

    /*!
      @brief mHash maps the ID of each basic block subject to domination
             analysis to a unique position in the bit vectors used during
             iterative analysis.
    */
    std::map<WIR_id_t, size_t> mHash;

    /*!
      @brief mReverseHash reverse-maps each bit vector position to its
             corresponding basic block subject to domination analysis.
    */
    std::map<size_t, WIR_BasicBlock *> mReverseHash;

    /*!
      @brief mBlockDominators maps a basic block's ID to a bit vector of all
             its dominator blocks.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockDominators;

    /*!
      @brief mPredecessors maps a basic block's ID to all its non-empty
             predecessors.
    */
    std::map<WIR_id_t, WIR_BasicBlockSet> mPredecessors;

    /*!
      @brief mSuccessors maps a basic block's ID to all its non-empty
             successors.
    */
    std::map<WIR_id_t, WIR_BasicBlockSet> mSuccessors;

    /*!
      @brief mUpdateBlock stores whether a basic block ID has to be re-evaluated
             during iterative control flow analysis.
    */
    std::map<WIR_id_t, bool> mUpdateBlock;

    //! mCFG holds the currently analyzed function's control flow graph.
    WIR_CFG mCFG;

    /*!
      @brief mAnalyzeInstructions stores whether dominators shall also be
             analyzed for %WIR instructions, in addition to dominators at basic
             block-level only.
    */
    bool mAnalyzeInstructions;

};

}       // namespace WIR

#endif  // _WIR_DOMINATIONANALYSIS_H
