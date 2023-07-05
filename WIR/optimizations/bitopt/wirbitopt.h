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
  @file wirbitopt.h
  @brief This file provides the interface of a virtual base class for
         optimizations using bit-true data and value flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BITOPT_H
#define _WIR_BITOPT_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <map>

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

class WIR_BitDFA;
class WIR_CompilationUnit;
class WIR_Function;
class WIR_Instruction;
class WIR_RegisterParameter;
class WIR_System;
class WIR_UpDownValue;


/*!
  @brief Class WIR_BitOpt is a virtual base class for optimizations using
         bit-true data and value flow analysis.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BitOpt : virtual public WIR_Optimization
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor for system-level optimization.

      @param[in] s A reference to a WIR_System to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_BitOpt( WIR_System & );

    /*!
      @brief Default constructor for compilation unit-level optimization.

      @param[in] c A reference to a WIR_CompilationUnit to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_BitOpt( WIR_CompilationUnit & );

    /*!
      @brief Default constructor for function-level optimization.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit WIR_BitOpt( WIR_Function & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BitOpt( void );


    //
    // Handling of data flow analysis.
    //

    /*!
      @brief setRunDFA specifies whether a bit-true data flow analysis shall be
             carried out prior to the actual optimization or not.

      @param[in] f A Boolean flag defaulting to true that denotes whether a data
                   flow analysis will be carried out (true) or not.

      If no data flow analysis shall be done, a bit-true optimization uses
      bitValue containers that are already attached to the %WIR by prior
      analysis runs.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setRunDFA( bool = true );


  protected:

    //
    // Handling of data flow analysis.
    //

    /*!
      @brief setDFA sets the processor-specific bit-true data and value flow
             analyzer.

      @param[in] a A reference to a processor-specific bit-true data and value
                   flow analysis.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setDFA( WIR_BitDFA & );


    //
    // Handling of incoming and outgoing up/down values.
    //

    /*!
      @brief combineOutValues combines the outgoing up values of a register
             parameter with previous up values, if multiple edges with
             potentially different up values refer to the very same parameter of
             a %WIR operation.

      @param[in] rp A const reference to a register parameter.
      @param[in,out] outValue A reference to a map storing the combined outgoing
                              up value per %WIR parameter.
      @return An iterator pointing to the newly created entry of map outValue,
              or the end iterator.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::map<WIR_id_t, WIR_UpDownValue>::iterator combineOutValues( const WIR_RegisterParameter &,
                                                                    std::map<WIR_id_t, WIR_UpDownValue> & ) const;

    /*!
      @brief combineInValues combines the incoming up values of a register
             parameter with previous up values, if multiple edges with
             potentially different up values refer to the very same parameter of
             a %WIR operation.

      @param[in] rp A const reference to a register parameter.
      @param[in,out] inValue A reference to a map storing the combined incoming
                             up value per %WIR parameter.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void combineInValues( const WIR_RegisterParameter &,
                          std::map<WIR_id_t, WIR_UpDownValue> & ) const;

    /*!
      @brief updateLocations patches all up/down values attached to %WIR code of
             a given function such that location bits refering to old register
             parameters are replaced by locations of new parameters according to
             map mNewLocation.

      @param[in] f A reference to a WIR_Function to be optimized.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void updateLocations( WIR_Function & );

    /*!
      @brief verifyLocations verifies that location bits attached as bit values
             to the specified function actually refer to correct and existing
             register parameters.

      @param[in] f A const reference to a WIR_Function to be verified.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void verifyLocations( const WIR_Function & ) const;


    //
    // Attributes
    //

    /*!
      @brief mBitDFA is a pointer to an actual processor-specific bit-true data
             and value flow analysis.
    */
    WIR_BitDFA *mBitDFA;

    /*!
      @brief mRunDFA specifies whether a bit-true data flow analysis shall be
             carried out prior to the actual optimization (true), or whether
             existing bitValue containers already attached to the %WIR by
             prior analysis runs shall be used instead.
    */
    bool mRunDFA;

    /*!
      mNewLocation maps the ID of a register parameter in an old operation
      subject to constant folding to its corresponding register parameter within
      a new operation resulting from constant folding.

      During updateLocations, all bitValues containers are checked and up/down
      values containing locations refering to the old register parameter are
      modified to refer to the new register parameter afterwards.
    */
    std::map<WIR_id_t, std::reference_wrapper<WIR_RegisterParameter>> mNewLocation;

};

}       // namespace WIR

#endif  // _WIR_BITOPT_H
