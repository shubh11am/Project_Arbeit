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
  @file armconst10coprocessoroffset.h
  @brief This file provides the interface of unsigned 10 bits-wide immediate
         parameters for coprocessor load/store offsets.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _ARM_CONST10_COPROCESSOROFFSET_H
#define _ARM_CONST10_COPROCESSOROFFSET_H


//
// Include section
//

// Include WIR headers
#include <wir/wirunsignedimmediateparameter.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class ARM_Const10_CoprocessorOffset is the representation of unsigned
         const10 offsets for coprocessor loads/stores.

  According to the ARM Architecture Reference Manual (section A5.5.2, page
  A5-51ff.), such offsets are 8 bits wide and are multiplied by 4. Thus, the
  effective offsets for coprocessor loads and stores are all those 10 bits wide
  values that can evenly be divided by 4. Exactly this behavior is modeled by
  class ARM_Const10_CoprocessorOffset.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class ARM_Const10_CoprocessorOffset final : public WIR_UnsignedImmediateParameter<ARM_Const10_CoprocessorOffset>
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief No standard construction allowed, users must use
             ARM_Const10_CoprocessorOffset( unsigned long long ) instead.
    */
    ARM_Const10_CoprocessorOffset( void ) = delete;

    /*!
      @brief Default constructor for unsigned const10 offsets.

      @param[in] __i The immediate value.

      The constructor ensures that __i lies in the range of values that can be
      represented as coprocessor load/store offsets.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit ARM_Const10_CoprocessorOffset( unsigned long long );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const10_CoprocessorOffset( const ARM_Const10_CoprocessorOffset & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const10_CoprocessorOffset( ARM_Const10_CoprocessorOffset && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~ARM_Const10_CoprocessorOffset( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const10_CoprocessorOffset & operator = ( const ARM_Const10_CoprocessorOffset & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    ARM_Const10_CoprocessorOffset & operator = ( ARM_Const10_CoprocessorOffset && );


    //
    // Value handling.
    //

    /*!
      @brief setValue sets an unsigned immediate parameter's actual value.

      @param[in] i The parameter's new immediate value.

      setValue ensures that i lies in the range of values that can be
      represented as coprocessor load/store offsets.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual void setValue( unsigned long long );

};


/*!
  @brief Since 10 bits wide immediates that are evenly divided by 4 are also
         required for other ARM operations besides coprocessor operations, the
         following declaration provides an alias name.
*/
using ARM_Const10_Unsigned4 = ARM_Const10_CoprocessorOffset;

}       // namespace WIR

#endif  // _ARM_CONST10_COPROCESSOROFFSET_H
