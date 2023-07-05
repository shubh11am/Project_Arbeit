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
  @file wirconditionfieldparameter.h
  @brief This file provides the interface of parameters representing condition
         fields for predicated execution of operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_CONDITIONFIELDPARAMETER_H
#define _WIR_CONDITIONFIELDPARAMETER_H


//
// Include section
//

// Include WIR headers
#include <wir/wirparameter.h>
#include <wir/wirbaseprocessor.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_ConditionFieldParameter is the generic representation of
         condition field parameters of code operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_ConditionFieldParameter : public WIR_Parameter
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for condition field parameters.

      @param[in] __c A const reference to a condition to be used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_ConditionFieldParameter( const WIR_BaseProcessor::Condition & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ConditionFieldParameter( const WIR_ConditionFieldParameter & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ConditionFieldParameter( WIR_ConditionFieldParameter && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_ConditionFieldParameter( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ConditionFieldParameter & operator = ( const WIR_ConditionFieldParameter & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_ConditionFieldParameter & operator = ( WIR_ConditionFieldParameter && );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR parameter, i.e., that it is a
             condition field parameter.

      @return WIR_ParameterType::cond

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_ParameterType getType( void ) const;


    //
    // Condition handling.
    //

    /*!
      @brief setCondition sets a parameter's actual condition.

      @param[in] c The parameter's new condition.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setCondition( const WIR_BaseProcessor::Condition & );

    /*!
      @brief getCondition gets a parameter's condition.

      @return The parameter's current condition.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor::Condition &getCondition( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR condition field parameter to an output
             stream.

      @param[in] os A reference to an output stream.
      @param[in] p A const reference to the %WIR condition field parameter to be
                   dumped.
      @return A reference to the same output stream.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_ConditionFieldParameter & );


  protected:

    /*!
      @brief clone creates a copy of a condition field parameter.

      @return A pointer to the newly created copy of this parameter.

      Clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Parameter *clone( void ) const;


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_ConditionFieldParameter( const WIR_Processor::Condition & )
             instead.
    */
    WIR_ConditionFieldParameter( void ) = delete;

    //! mCondition points to a parameter's actual condition.
    WIR_BaseProcessor::Condition *mCondition;

};

}       // namespace WIR

#endif  // _WIR_CONDITIONFIELDPARAMETER_H
