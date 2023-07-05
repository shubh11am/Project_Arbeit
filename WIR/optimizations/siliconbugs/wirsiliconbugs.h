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
  @file wirsiliconbugs.h
  @brief This file provides the interface of a generic peephole optimimzer for
         silicon bug detection and correction.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_SILICONBUGS_H
#define _WIR_SILICONBUGS_H


//
// Include section
//

// Include standard headers
#include <string>

// Include WIR headers
#include <optimizations/peephole/wirpeephole.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Function;
class WIR_Instruction;
class WIR_Operation;


/*!
  @brief Class WIR_SiliconBugs models a generic peephole optimizer for silicon
         bugs.

  Actual peephole optimizers are created by inheriting from this virtual base
  class.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_SiliconBugs : public WIR_Peephole
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @param[in,out] f A reference to a WIR_Function to be optimized.
      @param[in] n An R-value reference to a string holding a silicon bug's ID.
      @param[in] s A non-zero unsigned integer denoting the peephole size.
      @param[in] b A Boolean flag defaulting to false that denotes whether the
                   peephole may cross basic block boundaries or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SiliconBugs( WIR_Function &, std::string &&, unsigned int,
                     bool = false );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_SiliconBugs( void );


  protected:

    /*!
      @brief No standard construction allowed, users must use the above
             constructor instead.
    */
    WIR_SiliconBugs( void ) = delete;

    /*!
      @brief No copy construction allowed, users must use the above
             constructor instead.
    */
    WIR_SiliconBugs( const WIR_SiliconBugs & ) = delete;

    /*!
      @brief No move construction allowed, users must use the above
             constructor instead.
    */
    WIR_SiliconBugs( WIR_SiliconBugs && ) = delete;

    /*!
      @brief matchPeephole determines whether the specified peephole matches a
             silicon bug.

      @param[in] p A const reference to a peephole.
      @return true if p matches with a silicon bug, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchPeephole( const WIR_Peephole::peephole & );

    /*!
      @brief transformPeephole fixes a silicon bug.

      @param[in] p A const reference to a peephole.

      All %WIR instructions of the fixed code sequence are marked as
      unmodifiable for subsequent optimizations.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void transformPeephole( const WIR_Peephole::peephole & );

    /*!
      @brief matchSiliconBug determines whether the specified peephole matches
             some actual silicon bug.

      @param[in] p A const reference to a peephole.
      @return true if p matches with a silicon bug, false otherwise.

      Since the detection of actual silicon bugs is processor-specific,
      matchSiliconBug is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchSiliconBug( const WIR_Peephole::peephole &p ) = 0;

    /*!
      @brief fixSiliconBug fixes some actual silicon bug.

      @param[in] p A const reference to a peephole.
      @return The peephole after bug fixing.

      Since fixing actual silicon bugs is processor-specific, fixSiliconBug is
      purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Peephole::peephole fixSiliconBug( const WIR_Peephole::peephole &p ) const = 0;

    /*!
      @brief markInstruction marks the given instruction with a %WIR comment
             that identifies it as fix for a silicon bug.

      @param[in,out] i A reference to a %WIR instruction to be marked.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void markInstruction( WIR_Instruction & ) const;

    //! mName holds a silicon bug's ID for progress messages.
    std::string mName;

};

}       // namespace WIR

#endif  // _WIR_SILICONBUGS_H
