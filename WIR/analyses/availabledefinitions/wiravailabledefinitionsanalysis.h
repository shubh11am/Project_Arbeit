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
  @file wiravailabledefinitionsanalysis.h
  @brief This file provides the interface of the %WIR available definitions
         analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_AVAILABLEDEFINITIONSANALYSIS_H
#define _WIR_AVAILABLEDEFINITIONSANALYSIS_H


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
  @brief Class WIR_AvailableDefinitionsAnalysis is the %WIR available
         definitions analysis.

  Available definitions analysis heavily bases on the reaching definitions
  analysis according to Andrew W. Appel, Modern Compiler Implementation in C,
  page 388 or Steven S. Muchnick, Advanced Compiler Design and Implementation,
  page 221. The central difference between both analyses is that reaching
  definitions is a MAY analysis (i.e., it determines whether SOME control flow
  path exists along that a definition may reach a use), while available
  definitions is a MUST analysis that determines whether a definition reaches a
  use along ALL control flow paths.

  Analysis results are stored in WIR_AvailableDefinitions containers that are
  attached to %WIR instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_AvailableDefinitionsAnalysis : public WIR_Analysis
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
    explicit WIR_AvailableDefinitionsAnalysis( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_AvailableDefinitionsAnalysis( void );


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
      @brief runAnalysis performs available definitions analysis by iteration of
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
             to basic block-level where available-definition analysis is
             actually done (Appel, pages 394-395).

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
      @brief mProblemSize stores how many definitions are subject to available
             definitions analysis.
    */
    size_t mProblemSize;

    /*!
      @brief mRegisterDefinitionsSize stores how many true DEF register
             parameters are subject to reaching definitions analysis.
    */
    size_t mRegisterDefinitionsSize;

    /*!
      @brief mHash maps the a register definition subject to available
             definitions analysis to a unique position in the bit vectors used
             during iterative analysis.
    */
    std::map<WIR_id_t, size_t> mHash;

    /*!
      @brief mReverseHash reverse-maps each bit vector position to the
             corresponding register definition subject to available definitions
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
      @brief mBlockAvailableDefsIn maps a basic block's ID to a bit vector of
             definitions available at the beginning of the specified basic
             block.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockAvailableDefsIn;

    /*!
      @brief mBlockAvailableDefsOut maps a basic block's ID to a bit vector
             of definitions available at the end of the specified basic block.
    */
    std::map<WIR_id_t, WIR_BitVector> mBlockAvailableDefsOut;

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

#endif  // _WIR_AVAILABLEDEFINITIONSANALYSIS_H
