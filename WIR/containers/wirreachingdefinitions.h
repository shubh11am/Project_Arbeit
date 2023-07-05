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
  @file wirreachingdefinitions.h
  @brief This file provides the interface of a %WIR container representing sets
         of register definitions that reach instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REACHINGDEFINITIONS_H
#define _WIR_REACHINGDEFINITIONS_H


//
// Include section
//

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

class WIR_PhysicalRegister;
class WIR_RegisterParameter;


/*!
  @brief Class WIR_ReachingDefinitions models sets of register definitions
         reaching instructions.

  WIR_ReachingDefinitions containers are attached to WIR_Instruction objects
  during reaching-definitions analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ReachingDefinitions : public WIR_Container<WIR_ReachingDefinitions>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ReachingDefinitions( void );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ReachingDefinitions( void );

    /*!
      @brief isUnique returns whether reaching-definition sets are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always true, reaching-definition sets are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Reaching-definition set handling.
    //

    /*!
      @brief insertReachingDefinition adds a new reaching definition to set
             mReachingDefinitions.

      @param[in] rp A const reference to the reaching definition to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertReachingDefinition( const WIR_RegisterParameter & );

    /*!
      @brief getReachingDefinitions returns the set mReachingDefinitions.

      @return A const reference to the set mReachingDefinitions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterParameterSet &getReachingDefinitions( void ) const;


    //
    // Reaching-input set handling.
    //

    /*!
      @brief insertReachingInput adds a new reaching function input to set
             mReachingInputs.

      @param[in] p A const reference to the physical register providing external
                   input to a function to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertReachingInput( const WIR_PhysicalRegister & );

    /*!
      @brief getReachingInputs returns the set mReachingInputs.

      @return A const reference to the set mReachingInputs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_PhysicalRegisterSet &getReachingInputs( void ) const;


  private:

    //! mReachingDefinitions holds the set of reaching definitions.
    WIR_RegisterParameterSet mReachingDefinitions;

    //! mReachingInputs holds the set of reaching external function inputs.
    WIR_PhysicalRegisterSet mReachingInputs;

};

}       // namespace WIR

#endif  // _WIR_REACHINGDEFINITIONS_H
