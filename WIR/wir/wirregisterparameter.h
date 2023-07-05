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
  @file wirregisterparameter.h
  @brief This file provides the interface of register parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REGISTERPARAMETER_H
#define _WIR_REGISTERPARAMETER_H


//
// Include section
//

// Include WIR headers
#include <wir/wirtypes.h>
#include <wir/wirparameter.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;


/*!
  @brief Class WIR_RegisterParameter is the generic representation of register
         parameters of code operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_RegisterParameter : public WIR_Parameter
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for register parameters.

      @param[in] __r A const reference to a register.
      @param[in] __u Information about the usage type of the parameter.
      @param[in] __i A Boolean defaulting to false denoting whether the
                     parameter is implicit or not.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter( const WIR_BaseRegister &, WIR_Usage, bool = false );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter( const WIR_RegisterParameter & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter( WIR_RegisterParameter && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_RegisterParameter( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter & operator = ( const WIR_RegisterParameter & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_RegisterParameter & operator = ( WIR_RegisterParameter && );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR parameter, i.e., that it is a
             register parameter.

      @return WIR_ParameterType::reg

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_ParameterType getType( void ) const;


    //
    // Register handling.
    //

    /*!
      @brief getRegister returns the register associated with a parameter.

      @return A reference to the parameter's register.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseRegister &getRegister( void ) const;


    //
    // Usage handling.
    //

    /*!
      @brief setUsage sets a register parameter's usage type to the specified
             one.

      @param[in] t The parameter's new usage type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setUsage( WIR_Usage );

    /*!
      @brief getUsage gets a register parameter's usage type.

      @return The parameter's current usage type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Usage getUsage( void ) const;

    /*!
      @brief isDefined returns whether this register parameter is defined.

      @return true if its usage is def or defuse, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isDefined( void ) const;

    /*!
      @brief isUsed returns whether this register parameter is used.

      @return true if its usage is use or defuse, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isUsed( void ) const;

    /*!
      @brief isDefUsed returns whether this register parameter is defined and
             used.

      @return true if its usage is defuse, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isDefUsed( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR register parameter to an output
             stream.

      @param[in] os A reference to an output stream.
      @param[in] p A const reference to the %WIR register parameter to be
                   dumped.
      @return A reference to the same output stream.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_RegisterParameter & );


  protected:

    /*!
      @brief clone creates a copy of a register parameter.

      @return A pointer to the newly created copy of this parameter.

      Clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Parameter *clone( void ) const;


  private:

    friend class WIR_OperationFormat;

    /*!
      @brief No standard construction allowed, users must use
             WIR_RegisterParameter( const WIR_BaseRegister &, WIR_Usage )
             instead.
    */
    WIR_RegisterParameter( void ) = delete;

    //! mRegister points to this parameter's register.
    WIR_BaseRegister *mRegister;

    //! mUsage stores whether a register parameter is defined, used, or both.
    WIR_Usage mUsage;

};

}       // namespace WIR

#endif  // _WIR_REGISTERPARAMETER_H
