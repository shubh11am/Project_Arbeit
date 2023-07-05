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
  @file wirpeephole.h
  @brief This file provides the interface of a generic peephole optimimzer.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_PEEPHOLE_H
#define _WIR_PEEPHOLE_H


//
// Include section
//

// Include standard headers
#include <list>
#include <set>
#include <utility>
#include <vector>

// Include WIR headers
#include <optimizations/generic/wiroptimization.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_Instruction;


/*!
  @brief Class WIR_Peephole models a generic peephole optimizer.

  Actual peephole optimizers are created by inheriting from this virtual base
  class.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Peephole : virtual public WIR_Optimization
{

  public:

    //
    // Internal type definitions.
    //

    using peephole =
      std::vector<std::list<std::reference_wrapper<WIR::WIR_Instruction>>::const_iterator>;


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in,out] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Peephole( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in,out] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Peephole( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in,out] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Peephole( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Peephole( void );


    //
    // Optimization management.
    //

    /*!
      @brief addPeepholeSize adds a peephole size.

      @param[in] s An unsigned integer denoting the peephole size.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void addPeepholeSize( unsigned int );

    /*!
      @brief getPeepholeSizes returns the set of supported peephole sizes.

      @return A set of unsigned integers containing the peephole sizes.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::set<unsigned int> &getPeepholeSizes( void ) const;

    /*!
      @brief setCrossBasicBlocks sets whether peepholes may cross basic block
             boundaries or not.

      @param[in] b A Boolean defaulting to true that specifies whether peepholes
                   may cross basic block boundaries or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setCrossBasicBlocks( bool = true );

    /*!
      @brief getCrossBasicBlocks returns whether peepholes may cross basic block
             boundaries or not.

      @return true if basic blocks may cross basic block boundaries, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool getCrossBasicBlocks( void ) const;


  protected:

    using peepholeList = std::list<peephole>;

    /*!
      @brief runOptimization performs peephole optimization in the given system.

      @param[in] s A reference to a %WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization performs peephole optimization in the given
             compilation unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization performs peephole optimization in the given
             function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );


    //
    // Peephole handling.
    //

    /*!
      @brief getPeepholes determines all possible peepholes having the %WIR
             instruction pointed to by the iterator as very first element.

      @param[in] it A const iterator pointing to the %WIR instruction serving as
                    start of the peephole.
      @return A list containing all possible peepholes from the starting
              position it on.

      All returned peepholes have one of the sizes specified in mPeepholeSizes.
      The instructions in the returned peepholes may span multiple basic blocks
      as specified by mCrossBBs. The returned list of peepholes may be empty if
      no peepholes of a specified size exist at all from the starting position
      on.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    peepholeList getPeepholes( std::list<std::reference_wrapper<WIR::WIR_Instruction>>::const_iterator ) const;

    /*!
      @brief matchPeephole determines whether the specified peephole is suitable
             for optimization.

      @param[in] p A const reference to a peephole.
      @return true if p is suitable for optimization, false otherwise.

      Since actual peephole optimizations are defined by inheriting from this
      class, matchPeephole is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool matchPeephole( const peephole &p ) = 0;

    /*!
      @brief transformPeephole optimizes the specified peephole.

      @param[in] p A const reference to a peephole.

      Since actual peephole optimizations are defined by inheriting from this
      class, transformPeephole is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void transformPeephole( const peephole &p ) = 0;


  private:

    //! mPeepholeSizes stores all accepted peephole sizes.
    std::set<unsigned int> mPeepholeSizes;

    /*!
      @brief mCrossBBs stores whether the peephole may cross basic block
             boundaries or not.
    */
    bool mCrossBBs;

};

}       // namespace WIR

#endif  // _WIR_PEEPHOLE_H
