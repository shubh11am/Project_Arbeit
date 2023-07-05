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
  @file tcasmargument.h
  @brief This file provides the interface of assembly operation arguments.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _TC_ASMARGUMENT_H
#define _TC_ASMARGUMENT_H


//
// Header section
//

namespace WIR {


/*!
  @brief Class TC_AsmArgument is the generic representation of arguments for an
         assembly operation.

  This class serves as virtual base class from which actual operation arguments
  are derived.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class TC_AsmArgument
{

  public:

    /*!
      @brief This enum represents different types of assembly arguments.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    enum class Type : unsigned long long
    {
      NONE =              (unsigned long long) 0,

      AREG =              (unsigned long long) 1 << 0,
      AREG_A10 =          (unsigned long long) 1 << 1,
      AREG_A15 =          (unsigned long long) 1 << 2,
      AREG64 =            (unsigned long long) 1 << 3,
      DREG =              (unsigned long long) 1 << 4,
      DREG_D15 =          (unsigned long long) 1 << 5,
      DREG_U =            (unsigned long long) 1 << 6,
      DREG_L =            (unsigned long long) 1 << 7,
      DREG_UU =           (unsigned long long) 1 << 8,
      DREG_UL =           (unsigned long long) 1 << 9,
      DREG_LU =           (unsigned long long) 1 << 10,
      DREG_LL =           (unsigned long long) 1 << 11,
      EREG =              (unsigned long long) 1 << 12,

      ANYAREG =           ( AREG | AREG_A10 | AREG_A15 ),
      ANYDREG =           ( DREG | DREG_D15 | DREG_U | DREG_L | DREG_UU |
                            DREG_UL | DREG_LU | DREG_LL ),
      ANYREG =            ( AREG | AREG_A15 | AREG64 | DREG | DREG_D15 |
                            DREG_U | DREG_L | DREG_UU | DREG_UL | DREG_LU |
                            DREG_LL | EREG ),

      AMODE_ABS =         (unsigned long long) 1 << 13, // label/const
      AMODE_BASE =        (unsigned long long) 1 << 14, // [Ax]offset10
      AMODE_BASELO =      (unsigned long long) 1 << 15, // [Ax]offset16
      AMODE_PREINC =      (unsigned long long) 1 << 16, // [+Ax]offset10
      AMODE_POSTINC =     (unsigned long long) 1 << 17, // [Ax+]offset10
      AMODE_CIRC =        (unsigned long long) 1 << 18, // [Ax+c]offset10
      AMODE_BREV =        (unsigned long long) 1 << 19, // [Ax+r]offset10
      AMODE_BASEHILAB =   (unsigned long long) 1 << 20, // [Ax]HI:label
      AMODE_BASELOLAB =   (unsigned long long) 1 << 21, // [Ax]LO:label

      AMODE_REGI =        (unsigned long long) 1 << 22, // [Ax]
      AMODE_BASE4 =       (unsigned long long) 1 << 23, // [Ax]offset4
      AMODE_A15BASE4 =    (unsigned long long) 1 << 24, // [A15]offset4
      AMODE_SPREL =       (unsigned long long) 1 << 25, // [A10]offset10
      AMODE_POSTINCCNST = (unsigned long long) 1 << 26, // [Ax+]

      AMODE32  =          ( AMODE_ABS | AMODE_BASE | AMODE_BASELO |
                            AMODE_PREINC | AMODE_POSTINC | AMODE_CIRC |
                            AMODE_BREV | AMODE_BASEHILAB | AMODE_BASELOLAB ),
      AMODE16S =          ( AMODE_REGI | AMODE_A15BASE4 | AMODE_POSTINCCNST ),
      AMODE16L =          ( AMODE_SPREL | AMODE_BASE4 ),
      AMODE16 =           ( AMODE16S | AMODE16L ),
      AMODE =             ( AMODE32 | AMODE16 ),

      CONST1 =            (unsigned long long) 1 << 27,
      CONST2 =            (unsigned long long) 1 << 28,
      CONST3 =            (unsigned long long) 1 << 29,
      CONST4 =            (unsigned long long) 1 << 30,
      CONST5 =            (unsigned long long) 1 << 31,
      CONST8 =            (unsigned long long) 1 << 32,
      CONST9 =            (unsigned long long) 1 << 33,
      CONST16 =           (unsigned long long) 1 << 34,

      CONST =             ( CONST1 | CONST2 | CONST3 | CONST4 | CONST5 |
                            CONST8 | CONST9 | CONST16 ),

      DISP4 =             (unsigned long long) 1 << 35,
      DISP8 =             (unsigned long long) 1 << 36,
      DISP15 =            (unsigned long long) 1 << 37,
      DISP24 =            (unsigned long long) 1 << 38,

      DISP =              ( DISP4 | DISP8 | DISP15 | DISP24 ),

      BITPOS =            (unsigned long long) 1 << 39,
      BFWIDTH =           (unsigned long long) 1 << 40,

      OFFSET18 =          (unsigned long long) 1 << 41,

      NUMERIC =           ( CONST1 | CONST2 | CONST3 | CONST4 | CONST5 |
                            CONST8 | CONST9 | CONST16 | DISP4 | DISP8 | DISP15 |
                            DISP24 | BITPOS | BFWIDTH | OFFSET18 )
    };


    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an argument of a given type.

      @param[in] t A specifier denoting the argument's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    explicit TC_AsmArgument( Type );

    /*!
      @brief Copy constructor.

      @param[in] a A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmArgument( const TC_AsmArgument & );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~TC_AsmArgument( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    TC_AsmArgument & operator = ( const TC_AsmArgument & );


    //
    // Type management.
    //

    /*!
      @brief setType sets an argument's specific type.

      @param[in] t A specifier denoting the argument's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setType( Type );

    /*!
      @brief getType returns the type of an assembly operation argument.

      @return The argument's type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    Type getType( void ) const;

    /*!
      @brief isCompatible returns whether an argument is compatible with a given
             argument type.

      @param[in] t A specifier denoting the argument's type.
      @return true if the argument is compatible with t's type, false otherwise.

      Since compatibility checks depend on actual arguments' implementation,
      isCompatible is a pure virtual method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual bool isCompatible( Type t ) const = 0;


  protected:

    /*!
      @brief clone creates a copy of an argument.

      @return A pointer to the newly created copy of this argument.

      Since the implementation details depend on some actual argument's
      characteristics, clone is a pure virtual method.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual TC_AsmArgument *clone( void ) const = 0;


  private:

    friend class TC_AsmContext;

    /*!
      @brief No standard construction allowed, users must use
             TC_AsmContext( Type )
             instead.
    */
    TC_AsmArgument( void ) = delete;

    //! mType stores an argument's actual type.
    Type mType;

};

}       // namespace WIR

#endif  // _TC_ASMARGUMENT_H
