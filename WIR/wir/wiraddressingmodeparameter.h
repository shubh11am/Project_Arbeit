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
  @file wiraddressingmodeparameter.h
  @brief This file provides the interface of parameters representing addressing
         modes.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_ADDRESSINGMOEPARAMETER_H
#define _WIR_ADDRESSINGMOEPARAMETER_H


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
  @brief Class WIR_AddressingModeParameter is the generic representation of
         addressing mode parameters of code operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_AddressingModeParameter : public WIR_Parameter
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for addressing mode parameters.

      @param[in] __r A const reference to an addressing mode to be used.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_AddressingModeParameter( const WIR_BaseProcessor::AddressingMode & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_AddressingModeParameter( const WIR_AddressingModeParameter & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_AddressingModeParameter( WIR_AddressingModeParameter && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_AddressingModeParameter( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_AddressingModeParameter & operator = ( const WIR_AddressingModeParameter & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_AddressingModeParameter & operator = ( WIR_AddressingModeParameter && );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR parameter, i.e., that it is an
             addressing mode parameter.

      @return WIR_ParameterType::addr

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_ParameterType getType( void ) const;


    //
    // Addressing mode handling.
    //

    /*!
      @brief setAddressingMode sets a parameter's actual addressing mode.

      @param[in] m The parameter's new addressing mode.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setAddressingMode( const WIR_BaseProcessor::AddressingMode & );

    /*!
      @brief getAddressingMode gets a parameter's addressing mode.

      @return The parameter's current addressing mode.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor::AddressingMode &getAddressingMode( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR addressing mode parameter to an output
             stream.

      @param[in] os A reference to an output stream.
      @param[in] p A const reference to the %WIR addressing mode parameter to be
                   dumped.
      @return A reference to the same output stream.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_AddressingModeParameter & );


  protected:

    /*!
      @brief clone creates a copy of an addressing mode parameter.

      @return A pointer to the newly created copy of this parameter.

      Clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Parameter *clone( void ) const;


  private:

    /*!
      @brief No standard construction allowed, users must use
             WIR_AddressingModeParameter( const WIR_Processor::AddressingMode & )
             instead.
    */
    WIR_AddressingModeParameter( void ) = delete;

    //! mMode points to a parameter's actual addressing mode.
    WIR_BaseProcessor::AddressingMode *mMode;

};

}       // namespace WIR

#endif  // _WIR_ADDRESSINGMOEPARAMETER_H
