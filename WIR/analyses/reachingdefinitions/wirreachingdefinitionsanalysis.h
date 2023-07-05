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
  @file wirreachingdefinitionsanalysis.h
  @brief This file provides the interface of the reaching definitions analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REACHINGDEFINITIONSANALYSIS_H
#define _WIR_REACHINGDEFINITIONSANALYSIS_H


//
// Include section
//

// Include standard headers
#include <map>
#include <set>
#include <utility>

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

class WIR_BitVector;
class WIR_Function;
class WIR_PhysicalRegister;
class WIR_RegisterParameter;


/*!
  @brief Class WIR_ReachingDefinitionsAnalysis is the %WIR reaching definitions
         analysis according to Andrew W. Appel, Modern Compiler Implementation
         in C, page 388 or Steven S. Muchnick, Advanced Compiler Design and
         Implementation, page 221.

  Analysis results are stored in WIR_ReachingDefinitions containers that are
  attached to %WIR instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ReachingDefinitionsAnalysis : public WIR_Analysis
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
    explicit WIR_ReachingDefinitionsAnalysis( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ReachingDefinitionsAnalysis( void );


    //
    // Analysis of virtual and physical registers.
    //

    /*!
      @brief setVirtualRegistersOnly sets whether the analysis should consider
             only virtual registers or whether both virtual and physical
             registers are analyzed.

      @param[in] b A Boolean denoting whether a only virtual (true) or both
                   virtual and physical registers (false) are analyzed.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setVirtualRegistersOnly( bool = true );

    /*!
      @brief getVirtualRegistersOnly returns whether only virtual or both
             virtual and physical registers are analyzed.

      @return True if only virtual registers are analyzed, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getVirtualRegistersOnly( void ) const;


  protected:

    /*!
      @brief runAnalysis performs reaching definitions analysis by iteration of
             the given function.

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
      @brief propagateIns2BB propagates instruction-level gen/kill information
             to basic block-level where reaching-definition analysis is actually
             done (Appel, pages 394-395).

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

    /*!
      @brief mVirtualRegistersOnly stores whether the analysis should consider
             only virtual registers or whether both virtual and physical
             registers are analyzed.

      By defaults, both virtual and physical registers are analyzed.
    */
    bool mVirtualRegistersOnly;

    /*!
      @brief mProblemSize stores how many definitions in total are subject to
             reaching definitions analysis.
    */
    size_t mProblemSize;

    /*!
      @brief mRegisterDefinitionsSize stores how many true DEF register
             parameters are subject to reaching definitions analysis.
    */
    size_t mRegisterDefinitionsSize;

    /*!
      @brief mHash maps a register definition or an external function input
             subject to reaching definitions analysis to a unique position in
             the bit vectors used during iterative analysis.
    */
    std::map<WIR_id_t, size_t> mHash;

    /*!
      @brief mReverseHash reverse-maps each bit vector position to the
             corresponding register definition subject to reaching definitions
             analysis.
    */
    std::map<size_t, WIR_RegisterParameter *> mReverseHash;

    /*!
      @brief mReverseHashInputs reverse-maps each bit vector position to the
             corresponding physical input register of a function subject to
             reaching definitions analysis.
    */
    std::map<size_t, WIR_PhysicalRegister *> mReverseHashInputs;

    /*!
      @brief mInstrGen maps an instruction's ID to a bit vector of all parameter
             definitions it generates.
    */
    std::map<WIR_id_t, WIR_BitVector> mInstrGen;

    /*!
      @brief mInstrKill maps an instruction's ID to a bit vector of all
             parameter definitions it kills.
    */
    std::map<WIR_id_t, WIR_BitVector> mInstrKill;

    /*!
      @brief mBlockKill maps a basic block's ID to a bit vector of all parameter
             definitions it kills.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockKill;

    /*!
      @brief mBlockReachingDefsIn maps a basic block's ID to a bit vector of
             definitions reaching the beginning of the specified basic block.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockReachingDefsIn;

    /*!
      @brief mBlockReachingDefsOut maps a basic block's ID to a bit vector of
             definitions reaching the end of the specified basic block.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockReachingDefsOut;

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

#endif  // _WIR_REACHINGDEFINITIONSANALYSIS_H
