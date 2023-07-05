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
  @file wirlabelparameter.h
  @brief This file provides the interface of parameters representing labels.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_LABELPARAMETER_H
#define _WIR_LABELPARAMETER_H


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

//
// Class forward declarations
//

class WIR_BasicBlock;
class WIR_Data;
class WIR_Function;
class WIR_ID_API;


/*!
  @brief Class WIR_LabelParameter is the generic representation of label
         parameters of code operations.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_LabelParameter : public WIR_Parameter
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for label parameters refering to basic blocks.

      @param[in] __o A const reference to a %WIR basic block.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_LabelParameter( const WIR_BasicBlock & );

    /*!
      @brief Default constructor for label parameters refering to data objects.

      @param[in] __o A const reference to a %WIR data object.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_LabelParameter( const WIR_Data & );

    /*!
      @brief Default constructor for label parameters refering to functions.

      @param[in] __o A const reference to a %WIR function.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_LabelParameter( const WIR_Function & );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LabelParameter( const WIR_LabelParameter & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LabelParameter( WIR_LabelParameter && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_LabelParameter( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LabelParameter & operator = ( const WIR_LabelParameter & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_LabelParameter & operator = ( WIR_LabelParameter && );


    //
    // Generic type handling.
    //

    /*!
      @brief getType returns the type of a %WIR parameter, i.e., that it is a
             label parameter.

      @return WIR_ParameterType::label

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_ParameterType getType( void ) const;

    /*!
      @brief getLabelType returns to which kind of entity a label parameter
             refers, i.e., whether a label points to a %WIR basic block or to a
             function etc.

      @return The kind of a label as expressed by the enumeration class
              WIR_SymbolType.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SymbolType getLabelType( void ) const;

    /*!
      @brief In case the label refers to a %WIR basic block, getBasicBlock
             returns the refered block.

      @return A reference to the refered basic block.

      If the label does not refer to a basic block, getBasicBlock fails with an
      assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BasicBlock &getBasicBlock( void ) const;

    /*!
      @brief In case the label refers to a %WIR data object, getData returns
             the refered data object.

      @return A reference to the refered data object.

      If the label does not refer to a data object, getData fails with an
      assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Data &getData( void ) const;

    /*!
      @brief In case the label refers to a %WIR function, getFunction returns
             the refered function.

      @return A reference to the refered function.

      If the label does not refer to a function, getFunction fails with an
      assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Function &getFunction( void ) const;


    //
    // Name handling.
    //

    /*!
      @brief getName returns a label's specific name.

      @return A string that holds the label's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getName( void ) const;


    //
    // Stream I/O.
    //

    /*!
      @brief The << operator dumps a %WIR label parameter to an output stream.

      @param[in] os A reference to an output stream.
      @param[in] p A const reference to the %WIR label parameter to be dumped.
      @return A reference to the same output stream.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    friend std::ostream & operator << ( std::ostream &,
                                        const WIR_LabelParameter & );


  protected:

    /*!
      @brief clone creates a copy of a label parameter.

      @return A pointer to the newly created copy of this parameter.

      Clone just calls the corresponding copy constructor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_Parameter *clone( void ) const;


  private:

    /*!
      @brief No standard construction allowed, users must use the constructors
             for basic blocks, data objects or functions instead.
    */
    WIR_LabelParameter( void ) = delete;

    /*!
      @brief mLabelType stores to which kind of entity a label refers, i.e.,
             whether a label points to a %WIR basic block or to a function etc.
    */
    WIR_SymbolType mLabelType;

    /*!
      @brief mReferedID points to the %WIR object to which a label refers.
    */
    WIR_ID_API *mReferedID;

};

}       // namespace WIR

#endif  // _WIR_LABELPARAMETER_H
