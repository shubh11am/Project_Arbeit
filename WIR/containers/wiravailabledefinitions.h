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
  @file wiravailabledefinitions.h
  @brief This file provides the interface of a %WIR container representing sets
         of register definitions that are available at instructions.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_AVAILABLEDEFINITIONS_H
#define _WIR_AVAILABLEDEFINITIONS_H


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
  @brief Class WIR_AvailableDefinitions models sets of register definitions that
         are available at instructions.

  WIR_AvailableDefinitions containers are attached to WIR_Instruction objects
  during available-definitions analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_AvailableDefinitions : public WIR_Container<WIR_AvailableDefinitions>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_AvailableDefinitions( void );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_AvailableDefinitions( void );

    /*!
      @brief isUnique returns whether available-definition sets are unique,
             i.e., whether at most one instance of this container type can be
             attached to a %WIR class.

      @return Always true, available-definition sets are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // Available-definition set handling.
    //

    /*!
      @brief insertAvailableDefinition adds a new available definition to set
             mAvailableDefinitions.

      @param[in] rp A const reference to the available definition to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAvailableDefinition( const WIR_RegisterParameter & );

    /*!
      @brief getAvailableDefinitions returns the set mAvailableDefinitions.

      @return A const reference to the set mAvailableDefinitions.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterParameterSet &getAvailableDefinitions( void ) const;


    //
    // Available-input set handling.
    //

    /*!
      @brief insertAvailableInput adds a new reaching function input to set
             mAvailableInputs.

      @param[in] p A const reference to the physical register providing external
                   input to a function to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertAvailableInput( const WIR_PhysicalRegister & );

    /*!
      @brief getAvailableInputs returns the set mAvailableInputs.

      @return A const reference to the set mAvailableInputs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_PhysicalRegisterSet &getAvailableInputs( void ) const;


  private:

    //! mAvailableDefinitions holds the set of available definitions.
    WIR_RegisterParameterSet mAvailableDefinitions;

    //! mAvailableInputs holds the set of available external function inputs.
    WIR_PhysicalRegisterSet mAvailableInputs;

};

}       // namespace WIR

#endif  // _WIR_AVAILABLEDEFINITIONS_H
