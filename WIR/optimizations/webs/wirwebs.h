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
  @file wirwebs.h
  @brief This file provides the interface of an optimization replacing
         occurrences of virtual registers by webs.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_WEBS_H
#define _WIR_WEBS_H


//
// Include section
//

// Include standard headers
#include <list>
#include <map>
#include <set>

// Include WIR headers
#include <wir/wirtypes.h>
#include <optimizations/generic/wiroptimization.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_CompilationUnit;
class WIR_Function;
class WIR_RegisterParameter;
class WIR_System;
class WIR_VirtualRegister;


/*!
  @brief Class WIR_Webs is an optimization that replaces occurrences of virtual
         registers by webs (cf. S. S. Muchnick, chapter 8.10, pp. 251).

  According to Muchnick, a web for a virtual register is the maximal union of
  intersecting def-use chains for that register. Hereby, two def-use chains
  intersect if they have a use in common (Muchnick, p. 490).

  This class first detects webs for a virtual register and then replaces that
  register by a novel virtual register for each individual web found.

  According to S. S. Muchnick, "Webs are particularly useful in global register
  allocation by graph coloring---they are the units that are candidates for
  allocation to registers.

  Note that one effect of constructing webs is to separate uses of a variable
  with a name like 'i' that may be used over and over again in a procedure as,
  say, a loop index, but whose uses are disjoint from each other. This can
  significantly improve register allocation by reducing the range over which a
  variable may require a register and can improve the effectiveness of other
  optimizations as well. In particular, optimizations that apply to a single
  variable or that apply to a limited range of program text, such as strength
  reduction applied to induction variables, may benefit from this."

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Webs : public WIR_Optimization
{

  public:

    //
    // Type definitions.
    //

    /*!
      @brief Struct Web represents a web for some virtual register.

      The struct distinguishes between those places where the register is
      defined and where it is used. The virtual register itself that is
      represented by this web is not modeled here.
    */
    struct Web
    {
      public:

        WIR_RegisterParameterSet defs;
        WIR_RegisterParameterSet uses;
    };


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Webs( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Webs( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_Webs( WIR_Function & );

    /*!
      @brief Destructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_Webs( void );

    /*!
      @brief getWebs returns the list of finally created webs.

      @return A list of webs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::list<struct Web> getWebs( void ) const;


  protected:

    /*!
      @brief runOptimization inserts webs in the given system.

      @param[in] s A reference to a %WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_System & );

    /*!
      @brief runOptimization inserts webs in the given compilation unit.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_CompilationUnit & );

    /*!
      @brief runOptimization inserts webs in the given function.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void runOptimization( WIR_Function & );


  private:

    //
    // Private methods for making webs.
    //

    /*!
      @brief identifyWebs identifies webs for a given virtual register.

      @param[in] r A reference to a virtual register for which webs shall be
                   identified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void identifyWebs( const WIR_VirtualRegister & );

    /*!
      @brief createWebs does the actual work to create the previously identified
             webs for the given virtual register.

      @param[in] r A reference to a virtual register for which webs shall be
                   created.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void createWebs( const WIR_VirtualRegister & );

    /*!
      @brief cloneRegisters creates an exact copy of the specified register and
             its entire register hierarchy and inserts it into the %WIR function
             to which r belongs.

      @param[in] r A reference to a virtual register that shall be cloned.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void cloneRegisters( const WIR_VirtualRegister & );

    /*!
      @brief cloneParameter creates a new %WIR register parameter as a clone for
             the specified parameter and replaces the old parameter by the the
             new one in its owning operation.

      @param[in] p A reference to a register parameter that shall be cloned.
      @param[in,out] newWeb A reference to a new web into which the newly
                            generated, cloned register parameters are inserted.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void cloneParameter( WIR_RegisterParameter &, struct Web &newWeb );

    //! mWebs stores all webs identified for some particular virtual register.
    std::list<struct Web> mWebs;

    /*!
      @brief mNewWebs stores all newly created webs after virtual registers have
             been replaced.
    */
    std::list<struct Web> mNewWebs;

    /*!
      @brief mClonedReg maps the IDs of virtual registers for which a web shall
             be generated to their created clone registers.
    */
    std::map<WIR_id_t, WIR_VirtualRegister *> mClonedReg;

};

}       // namespace WIR

#endif  // _WIR_WEBS_H
