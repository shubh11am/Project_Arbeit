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
  @file wirlocation.h
  @brief This file provides the interface of location bits within the L4
         half-order.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_LOCATION_H
#define _WIR_LOCATION_H


//
// Include section
//

// Include standard headers
#include <functional>

// Include boost headers
#include <boost/optional.hpp>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_RegisterParameter;
class WIR_Symbol;


/*!
  @brief This enum represents different types of %WIR data flow graph nodes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
enum class WIR_LocationType : char
{
  //! A location refering to a register parameter.
  reg,

  //! A location refering to a %WIR symbol.
  sym
};


/*!
  @brief Class WIR_Location represents references to a bit in a register or in
         an address, at a given point in time. They are used to model locations
         (L) and negated locations (N), resp.

  According to Jens Wagner, Retargierbare Ausnutzung von Spezialoperationen für
  Eingebettete Systeme mit Hilfe bitgenauer Wertflussanalyse, page 124,
  Definition 3.18, locations are represented by a tuple l = (A, b, t) where A
  denotes the address of the location's memory word, b denotes the number of the
  bit inside this memory word, and t denotes the point in time at which a value
  was produced in this memory word.

  The point in time t can be simplified by providing a reference to an operation
  within the program code that produces the specified bit. Thus, it is
  sufficient to use references to register parameters or to %WIR symbols and
  their bit positions as locations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Location
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating a location refering to a bit within a
             register produced by a given parameter.

      @param[in] rp A const reference to a register parameter producing some
                    register.
      @param[in] b An unsigned integer denoting the location's bit position
                   within the produced register value.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Location( const WIR_RegisterParameter &, unsigned int );

    /*!
      @brief Default constructor creating a location refering to a bit of the
             given symbol's address.

      @param[in] s A const reference to a symbol.
      @param[in] b An unsigned integer denoting the location's bit position
                   within the refered symbol's address.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Location( const WIR_Symbol &, unsigned int );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Location( const WIR_Location & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Location( WIR_Location && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ~WIR_Location( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Location & operator = ( const WIR_Location & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Location & operator = ( WIR_Location && );


    //
    // Comparison operators.
    //

    /*!
      @brief The == operator checks for equality of locations.

      @param[in] __o A const reference to another location to be compared.
      @return true iff both locations are equal, false otherwise.

      Two locations are equal if and only if they are of the same type and refer
      to the very same bit of the very same register parameter or symbol, resp.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator == ( const WIR_Location & ) const;

    /*!
      @brief The != operator checks for inequality of locations.

      @param[in] __o A const reference to another location to be compared.
      @return true iff both locations are inequal, false otherwise.

      Two locations are inequal if they have different type or if they refer to
      different register parameters or different symbols, resp., or if they
      refer to different bits within the same register parameter or symbol,
      resp.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool operator != ( const WIR_Location & ) const;


    //
    // Type handling.
    //

    /*!
      @brief getType returns the type of a %WIR location, i.e., whether it
             refers to a register parameter or a symbol.

      @return The location's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LocationType getType( void ) const;

    /*!
      @brief isRegisterParameter returns whether a location refers to a register
             parameter.

      @return true if the location refers to a register parameter, false
              otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isRegisterParameter( void ) const;

    /*!
      @brief isSymbol returns whether a location refers to a symbol.

      @return true if the location refers to a symbol, false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool isSymbol( void ) const;

    /*!
      @brief getRegisterParameter returns the register parameter to which a
             location refers to.

      @return A const reference to the register parameter refered by a location.

      getRegisterParameter asserts if the location does not refer to a register
      parameter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_RegisterParameter &getRegisterParameter( void ) const;

    /*!
      @brief getSymbol returns the symbol to which a location refers to.

      @return A const reference to the symbol refered by a location.

      getSymbol asserts if the location does not refer to a symbol.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_Symbol &getSymbol( void ) const;

    /*!
      @brief getBitPosition returns a location's bit position within the refered
             register parameter or symbol address.

      @return The location's bit position.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned int getBitPosition( void ) const;


  private:

    /*!
      @brief No standard construction allowed, users must use one of the above
             default constructors instead.
    */
    WIR_Location( void ) = delete;

    /*!
      @brief mParam refers to the %WIR register parameter a location potentially
             refers to.
    */
    boost::optional<std::reference_wrapper<const WIR_RegisterParameter>> mParam;

    //! mSymbol refers to the %WIR symbol a location potentially refers to.
    boost::optional<std::reference_wrapper<const WIR_Symbol>> mSymbol;

    //! mType stores a location's actual type.
    WIR_LocationType mType;

    //! mBitPosition stores the number of the bit inside the refered location.
    unsigned int mBitPosition;

};

}       // namespace WIR

#endif  // _WIR_LOCATION_H
