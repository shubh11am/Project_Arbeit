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
  @file wirlifenessanalysis.h
  @brief This file provides the interface of the lifeness analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_LIFENESSANALYSIS_H
#define _WIR_LIFENESSANALYSIS_H


//
// Include section
//

// Include standard headers
#include <map>
#include <set>

// Include WIR headers
#include <wir/wirtypes.h>
#include <analyses/generic/wiranalysis.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;
class WIR_BitVector;
class WIR_Function;


/*!
  @brief Class WIR_LifenessAnalysis is the %WIR lifeness analysis according to
         Andrew W. Appel, Modern Compiler Implementation in C, page 221,
         algorithm 10.4.

  Analysis results are stored in WIR_LiveOut containers that are attached to
  %WIR instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_LifenessAnalysis : public WIR_Analysis
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for function-level analysis.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_LifenessAnalysis( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_LifenessAnalysis( void );


  protected:

    /*!
      @brief runAnalysis performs lifeness analysis by iteration of the given
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
             about register definitions/uses of %WIR instructions and of
             predecessor/successor relations between basic blocks.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void init( WIR_Function & );

    /*!
      @brief propagateIns2BB propagates instruction-level def/use information to
             basic block-level where lifeness analysis is actually done (Appel,
             pages 394-395).

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void propagateIns2BB( WIR_Function & );

    /*!
      @brief propagateBB2Ins propagates basic block-level analysis results to
             instruction-level.

      @param[in] f A reference to a WIR_Function to be analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void propagateBB2Ins( WIR_Function & );

    //! mProblemSize stores how many registers are subject to lifeness analysis.
    size_t mProblemSize;

    /*!
      @brief mHash maps the ID of each register subject to lifeness analysis to
             a unique position in the bit vectors used during iterative
             analysis.
    */
    std::map<WIR_id_t, size_t> mHash;

    /*!
      @brief mReverseHash reverse-maps each bit vector position to its
             corresponding register subject to lifeness analysis.
    */
    std::map<size_t, WIR_BaseRegister *> mReverseHash;

    /*!
      @brief mInstrDefs maps an instruction's ID to a bit vector of all
             registers it defines.
    */
    std::map<WIR_id_t, WIR_BitVector> mInstrDefs;

    /*!
      @brief mInstrUses maps an instruction's ID to a bit vector of all
             registers it uses.
    */
    std::map<WIR_id_t, WIR_BitVector> mInstrUses;

    /*!
      @brief mBlockDefs maps a basic block's ID to a bit vector of all registers
             it defines.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockDefs;

    /*!
      @brief mBlockLiveIn maps a basic block's ID to a bit vector of live-in
             registers.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockLiveIn;

    /*!
      @brief mBlockLiveIn maps a basic block's ID to a bit vector of live-out
             registers.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockLiveOut;

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
             during iterative data flow analysis.
    */
    std::map<WIR_id_t, bool> mUpdateBlock;

};

}       // namespace WIR

#endif  // _WIR_LIFENESSANALYSIS_H
