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
  @file wirbaseimmediateparameter.h
  @brief This file provides the basic interface of generic %WIR immediate
         parameters.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BASEIMMEDIATEPARAMETER_H
#define _WIR_BASEIMMEDIATEPARAMETER_H


//
// Include section
//

// Include standard headers
#include <string>

// Include WIR headers
#include <wir/wirparameter.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_BaseImmediateParameter is the generic representation of
         parameters holding immediate operands.

  Actual immediate parameters for real processor architectures are modeled by
  inheriting from this class. However, generic mechanisms are required in order
  to distinguish different types of immediate parameters, e.g., to distinguish
  between unsigned 6 bits wide and signed 8 bits wide parameters. This generic
  type handling is introduced in class WIR_ImmediateParameter that inherits from
  WIR_BaseImmediateParameter. So, actual immediate parameters for real
  processors must inherit from WIR_ImmediateParameter and not from
  WIR_BaseImmediateParameter. End-user code shall never make use of and directly
  inherit from WIR_BaseImmediateParameter.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BaseImmediateParameter : public WIR_Parameter
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an immediate parameter for the
             specified bit width.

      @param[in] __w The number of available bits.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_BaseImmediateParameter( unsigned int );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseImmediateParameter( const WIR_BaseImmediateParameter & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseImmediateParameter( WIR_BaseImmediateParameter && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BaseImmediateParameter( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseImmediateParameter & operator = ( const WIR_BaseImmediateParameter & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseImmediateParameter & operator = ( WIR_BaseImmediateParameter && );


    //
    // Generic type handling.
    //

    /*!
      @brief getImmediateType returns the ID of an immediate parameter type.

      @return The immediate parameter type's ID.

      getImmediateType can be used to query types for actual immediate parameter
      objects. Since actual immediates are defined by inheriting from this
      class, getImmediateType is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_id_t getImmediateType( void ) const = 0;

    /*!
      @brief getType returns the type of a %WIR parameter, i.e., that it is an
             immediate parameter.

      @return WIR_ParameterType::imm

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_ParameterType getType( void ) const;


    //
    // Value handling.
    //

    /*!
      @brief getSignedValue gets a signed immediate parameter's value.

      @return The parameter's current signed value.

      Since actual immediates are defined by inheriting from this class,
      getSignedValue is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual signed long long getSignedValue( void ) const = 0;

    /*!
      @brief getUnsignedValue gets an unsigned immediate parameter's value.

      @return The parameter's current unsigned value.

      Since actual immediates are defined by inheriting from this class,
      getUnsignedValue is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned long long getUnsignedValue( void ) const = 0;

    /*!
      @brief getBitWidth returns an immediate parameter's bit width.

      @return The immediate's bit width.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getBitWidth( void ) const;

    /*!
      @brief isSigned returns whether an immediate parameter is signed or not.

      @return true if the immediate parameter is signed, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isSigned( void ) const = 0;

    /*!
      @brief isUnsigned returns whether an immediate parameter is unsigned or
             not.

      @return true if the immediate parameter is unsigned, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isUnsigned( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR immediate parameter to an output
             stream.

      @param[in] os A reference to an output stream.
      @param[in] p A const reference to the %WIR immediate parameter to be
                   dumped.
      @return A reference to the same output stream.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_BaseImmediateParameter & );

    /*!
      @brief getValueString returns a string containing an immediate
             parameters's value.

      @return A string with the immediate's value.

      This method is internally used for doing I/O dumps of immediate
      parameters. Since the behavior of getValueString depends on whether an
      immediate is signed or unsigned, this method is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual std::string getValueString( void ) const = 0;


  protected:

    /*!
      @brief Class Registrator serves to initialize static members of %WIR
             immediates and automatically registers a new unique ID per
             immediate type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class Registrator
    {

      public:

        // This class has almost no public interface so that almost all standard
        // constructors and operators are deleted.
        Registrator( void ) = delete;
        Registrator( const Registrator & ) = delete;
        Registrator( Registrator && ) = delete;
        Registrator & operator = ( const Registrator & ) = delete;
        Registrator & operator = ( Registrator && ) = delete;

        /*!
          @brief Default constructor registering a new immediate type.

          @param[out] id A reference that finally holds the new immediate type's
                         unique ID.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit Registrator( WIR_id_t & );

        /*!
          @brief touch is a dummy method that just serves to activate the
                 initialization of static data members.

          Objects of class 'Registrator' are used as static initializers for
          class WIR_ImmediateParameter. Since WIR_ImmediateParameter is a
          templated class, instantiation of its static data members does not
          occur until these are explicitly referenced. For this purpose, this
          method is provided: WIR_ImmediateParameter can 'touch' its static data
          member so that it will get initialized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void touch( void );

    };

    /*!
      @brief mBitWidth stores an immediate parameter's bit width and thus
             specifies the parameter's value range.
    */
    unsigned int mBitWidth;


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_BaseImmediateParameter( unsigned int ) instead.
    */
    WIR_BaseImmediateParameter( void ) = delete;

    friend class Registrator;

    /*!
      @brief registerNewImmediateType registers a new immediate parameter type.

      @param[out] id A reference that finally holds the unique ID for the new
                     immediate type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void registerNewImmediateType( WIR_id_t & );

    /*!
      @brief clone creates a copy of a %WIR immediate parameter.

      @return A pointer to the newly created copy of this immediate.

      In the derived class WIR_ImmediateParameter<typename T>, clone shall just
      call the copy constructor so that by using the template argument, the
      correct copy constructor of the actual immediate parameter class (which is
      unknown here) is called. For this purpose, clone is a purely virtual
      method here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseImmediateParameter *clone( void ) const = 0;

    //! mTypeID holds the next free ID for new immediate parameter types.
    static WIR_id_t mTypeID;

};

}       // namespace WIR

#endif  // _WIR_BASEIMMEDIATEPARAMETER_H
