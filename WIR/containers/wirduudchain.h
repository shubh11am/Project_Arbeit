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
  @file wirduudchain.h
  @brief This file provides the interface of a %WIR container representing
         def-use and use-def chains of register parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_DUUDCHAIN_H
#define _WIR_DUUDCHAIN_H


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
  @brief Class WIR_DUUDChain models def-use and use-def chains of register
         parameters.

  WIR_DUUDChain containers are attached to WIR_RegisterParameter objects during
  def-use/use-def chain analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_DUUDChain : public WIR_Container<WIR_DUUDChain>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_DUUDChain( void );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_DUUDChain( void );

    /*!
      @brief isUnique returns whether def-use/use-def chains are unique, i.e.,
             whether at most one instance of this container type can be attached
             to a %WIR class.

      @return Always true, def-use/use-def chains are unique.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isUnique( void ) const;


    //
    // DU chain handling.
    //

    /*!
      @brief insertDUChain adds a new register parameter to set mDUChains.

      @param[in] o A const reference to the register parameter to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertDUChain( const WIR_RegisterParameter & );

    /*!
      @brief getDUChains returns the set mDUChains.

      @return A const reference to the set mDUChains.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterParameterSet &getDUChains( void ) const;


    //
    // UD chain handling.
    //

    /*!
      @brief insertUDChain adds a new register parameter to set mUDChains.

      @param[in] o A const reference to the register parameter to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertUDChain( const WIR_RegisterParameter & );

    /*!
      @brief getUDChains returns the set mUDChains.

      @return A const reference to the set mUDChains.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterParameterSet &getUDChains( void ) const;

    /*!
      @brief insertUDInput adds a new function input to set mUDInputs.

      @param[in] o A const reference to the physical register to be added.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void insertUDInput( const WIR_PhysicalRegister & );

    /*!
      @brief getUDInputs returns the set mUDInputs.

      @return A const reference to the set mUDInputs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_PhysicalRegisterSet &getUDInputs( void ) const;


  private:

    //! mDUChains holds the def-use chains for a register parameter.
    WIR_RegisterParameterSet mDUChains;

    //! mUDChains holds the use-def chains for a register parameter.
    WIR_RegisterParameterSet mUDChains;

    //! mUDInputs holds the use-def chains for external function inputs.
    WIR_PhysicalRegisterSet mUDInputs;

};

}       // namespace WIR

#endif  // _WIR_DUUDCHAIN_H
