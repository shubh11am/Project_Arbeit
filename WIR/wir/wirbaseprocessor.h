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
  @file wirbaseprocessor.h
  @brief This file provides the basic interface of generic %WIR processor
         architectures.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_BASEPROCESSOR_H
#define _WIR_BASEPROCESSOR_H


//
// Include section
//

// Include standard headers
#include <functional>
#include <memory>
#include <set>
#include <string>
#include <vector>

// Include WIR headers
#include <wir/API/wirinheritableenum.h>
#include <wir/wirsystemcomponent.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

//
// Class forward declarations
//

class WIR_BaseRegister;
class WIR_Operation;
class WIR_PhysicalRegister;
class WIR_Section;


/*!
  @brief Class WIR_BaseProcessor models a generic %WIR processor architecture.

  Besides ISA characteristics like, e.g., registers, operation formats and
  opcodes, this class also models ELF sections. The underlying rationale is that
  for each processor core within an arbitrary %WIR system, a dedicated
  executable file is produced. Thus, if %WIR is used to model code for a 4-core
  architecture, four individual executables will result. And since each
  executable has its very own sections for, e.g., code or data, these sections
  are thus managed by %WIR processor cores.

  Actual processors are modeled by inheriting from this class and by adding
  appropriate architecture and ISA features. However, generic mechanisms are
  required in order to distinguish different types of processors, e.g., to
  distinguish between ARM, MIPS and TriCore processors. This generic processor
  type handling is introduced in class WIR_Processor that inherits from
  WIR_BaseProcessor. So, actual processors for, e.g., ARM or TriCore
  architectures must inherit from WIR_Processor and not from WIR_BaseProcessor.
  End-user code shall never make use of and directly inherit from
  WIR_BaseProcessor.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_BaseProcessor : public WIR_SystemComponent
{

  public:

    //
    // Constructors and destructors.
    //

    /*!
      @brief Default constructor creating an empty processor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor( void );

    /*!
      @brief Copy constructor.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor( const WIR_BaseProcessor & );

    /*!
      @brief Move constructor.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor( WIR_BaseProcessor && );

    /*!
      @brief Destructor.
      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual ~WIR_BaseProcessor( void );

    /*!
      @brief Copy-assignment operator.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor & operator = ( const WIR_BaseProcessor & );

    /*!
      @brief Move-assignment operator.

      @param[in] __o An R-value reference to another object to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_BaseProcessor & operator = ( WIR_BaseProcessor && );


    //
    // Processor type handling.
    //

    /*!
      @brief getType returns the type of a system component, i.e., that it is a
             processor core.

      @return WIR_SystemComponentType::core

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_SystemComponentType getType( void ) const;

    /*!
      @brief getProcessorType returns the ID of a processor's type.

      @return The processor type's ID.

      getProcessorType can be used to query types for actual processor objects.
      Since actual processors are defined by inheriting from this class,
      getProcessorType is purely virtual here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_id_t getProcessorType( void ) const = 0;

    /*!
      @brief getProcessorName returns a processor's name as given by its
             manufacturer.

      @return A string that holds the processor's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getProcessorName( void ) const;

    /*!
      @brief getISAName returns a processor's ISA description.

      @return A string that holds the processor's ISA description.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    std::string getISAName( void ) const;

    /*!
      @brief getClockFrequency returns a processor's clock frequency.

      @return The processor's clock frequency in Hertz.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    unsigned long long getClockFrequency( void ) const;

    /*!
      @brief getVoltage returns a processor core's voltage.

      @return The processor's operation voltage.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    float getVoltage( void ) const;


    //
    // Data structures used to model a processor architecture.
    //

    class OpCode;
    class OperationFormat;

    /*!
      @brief Class AddressingMode models generic addressing modes of a CPU.

      This class serves as base class from which addressing modes of actual
      processor instances are derived.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class AddressingMode : public WIR_InheritableEnum
    {

      public:

        /*!
          @brief Destructor.
          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual ~AddressingMode( void );

        /*!
          @brief getProcessorTypeName returns a string containing the C++-
                 mangled name of the processor class to which an addressing
                 mode belongs.

          @return A string holding the processor's type name.

          This method is only used for comparing addressing modes of different
          processor architectures in WIR_Operation::checkParameters().

          Since the task of this method is processor-specific, this is a pure
          virtual method.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual std::string getProcessorTypeName( void ) const = 0;

        /*!
          @brief getName returns an addressing mode's specific name.

          @return A string that holds the addressing mode's name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        std::string getName( void ) const;

        /*!
          @brief isCompatible returns whether one addressing mode is compatible
                 with another.

          @param[in] t A const reference to the addressing mode whose
                       compatibility is to be checked.
          @param[in] f A const reference to the operation format whose
                       compatibility is to be checked.
          @param[in] o A const reference to the operation code whose
                       compatibility is to be checked.
          @return True iff two addressing modes are compatible, false otherwise.

          This compatibility check is used by WIR_Operation::checkParameters in
          order to verify that addressing mode parameters inserted into some
          %WIR operation actually match with the operation's format.

          By overloading this method in derived classes, specific compatibility
          checks for individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isCompatible( const AddressingMode &,
                                   const OperationFormat &,
                                   const OpCode & ) const;


      protected:

        /*!
          @brief No standard construction allowed, users must use the following
                 constructor below instead.
        */
        AddressingMode( void ) = delete;

        /*!
          @brief Default constructor for addressing modes.

          @param[in] __s A const reference to a string holding the addressing
                         mode's clear-text name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit AddressingMode( const std::string & );


      private:

        /*!
          @brief mName holds an addressing mode's clear-text name.
        */
        const std::string mName;

    };

    /*!
      @brief Class Condition models generic conditions of a CPU used for
             predicated execution of machine operations.

      This class serves as base class from which conditions of actual processor
      instances are derived.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class Condition : public WIR_InheritableEnum
    {

      public:

        virtual ~Condition( void ) = default;

        /*!
          @brief getProcessorTypeName returns a string containing the C++-
                 mangled name of the processor class to which a condition
                 belongs.

          @return A string holding the processor's type name.

          This method is only used for comparing conditions of different
          processor architectures in WIR_Operation::checkParameters().

          Since the task of this method is processor-specific, this is a pure
          virtual method.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual std::string getProcessorTypeName( void ) const = 0;

        /*!
          @brief getName returns a condition's specific name.

          @return A string that holds the condition's name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        std::string getName( void ) const;


      protected:

        /*!
          @brief No standard construction allowed, users must use the following
                 constructor below instead.
        */
        Condition( void ) = delete;

        /*!
          @brief Default constructor for conditions.

          @param[in] __s A const reference to a string holding the condition's
                         clear-text name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit Condition( const std::string & );


      private:

        /*!
          @brief mName holds a condition's clear-text name.
        */
        const std::string mName;

    };

    /*!
      @brief Class OpCode models generic opcodes of a CPU.

      This class serves as base class from which opcodes of actual processor
      instances are derived.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OpCode : public WIR_InheritableEnum
    {

      public:

        /*!
          @brief Destructor.
          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual ~OpCode( void );

        /*!
          @brief getName returns an opcode's specific mnemonic.

          @return A string that holds the opcode's mnemonic.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        std::string getName( void ) const;

        /*!
          @brief isMemoryAccess returns whether an opcode accesses memory
                 without being an explicit load or store operation.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode accesses memory, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isMemoryAccess( const WIR_Operation & ) const;

        /*!
          @brief isMemoryStore returns whether an opcode writes to memory.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode writes to memory, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isMemoryStore( const WIR_Operation & ) const;

        /*!
          @brief isMemoryLoad returns whether an opcode reads from memory.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode reads from memory, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isMemoryLoad( const WIR_Operation & ) const;

        /*!
          @brief isMove returns whether an opcode is a register-register move.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is a move, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isMove( const WIR_Operation & ) const;

        /*!
          @brief isCall returns whether an opcode calls a function.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode calls a function, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isCall( const WIR_Operation & ) const;

        /*!
          @brief isIndirectCall returns whether an opcode indirectly calls a
                 function.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is an indirect function call, false
                  otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isIndirectCall( const WIR_Operation & ) const;

        /*!
          @brief isReturn returns whether an opcode returns from a function
                 call.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is a function return, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isReturn( const WIR_Operation & ) const;

        /*!
          @brief isConditionalJump returns whether an opcode performs a
                 conditional jump.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is a conditional jump, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isConditionalJump( const WIR_Operation & ) const;

        /*!
          @brief isUnconditionalJump returns whether an opcode performs an
                 unconditional jump.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is an unconditional jump, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isUnconditionalJump( const WIR_Operation & ) const;

        /*!
          @brief isIndirectJump returns whether an opcode indirectly performs
                 a jump.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is an indirect jump, false otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isIndirectJump( const WIR_Operation & ) const;

        /*!
          @brief isAsmDataDirective returns whether an opcode is an assembly
                 data directive.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode is an assembly data directive, false
                  otherwise.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isAsmDataDirective( const WIR_Operation & ) const;

        /*!
          @brief hasSideEffects returns whether an opcode has side effects.

          @param[in] o A const reference to an operation to be checked.

          @return true if the opcode has side effects, false otherwise.

          Side effects could be non-obvious changes of a processor's internal
          status like, e.g., disabling interrupts, changing into supervisor mode
          or invalidating cache lines etc.

          By overloading this method in derived classes, specific tests for
          individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool hasSideEffects( const WIR_Operation & ) const;


      protected:

        /*!
          @brief No standard construction allowed, users must use the following
                 constructor below instead.
        */
        OpCode( void ) = delete;

        /*!
          @brief Default constructor for opcodes.

          @param[in] __s A const reference to a string holding the opcode's
                         mnemonic.
          @param[in] __ma A Boolean specifying whether the opcode accesses
                          memory without being an explicit load or store.
          @param[in] __st A Boolean specifying whether the opcode writes to
                          memory.
          @param[in] __ld A Boolean specifying whether the opcode reads from
                          memory.
          @param[in] __mv A Boolean specifying whether the opcode is a
                          register-register move.
          @param[in] __cl A Boolean specifying whether the opcode is a function
                          call.
          @param[in] __ic A Boolean specifying whether the opcode is an indirect
                          function call.
          @param[in] __rt A Boolean specifying whether the opcode is a return
                          from a function call.
          @param[in] __cj A Boolean specifying whether the opcode is a
                          conditional jump.
          @param[in] __uj A Boolean specifying whether the opcode is an
                          unconditional jump.
          @param[in] __ij A Boolean specifying whether the opcode is an indirect
                          jump.
          @param[in] __ad A Boolean specifying whether the opcode is an assembly
                          data directive.
          @param[in] __se A Boolean specifying whether the opcode has side
                          effects.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        OpCode( const std::string &, bool = false, bool = false, bool = false,
                bool = false, bool = false, bool = false, bool = false,
                bool = false, bool = false, bool = false, bool = false,
                bool = false );

        /*!
          @brief mName holds an opcode's mnemonic.
        */
        const std::string mName;

        /*!
          @brief mIsMemoryAccess specifies whether an opcode accesses memory
                 without being an explicit load or store operation.
        */
        const bool mIsMemoryAccess;

        /*!
          @brief mIsMemoryStore specifies whether an opcode writes to memory.
        */
        const bool mIsMemoryStore;

        /*!
          @brief mIsMemoryLoad specifies whether an opcode reads from memory.
        */
        const bool mIsMemoryLoad;

        /*!
          @brief mIsMove specifies whether an opcode is a register-register
                 move.
        */
        const bool mIsMove;

        /*!
          @brief mIsCall specifies whether an opcode calls a function.
        */
        const bool mIsCall;

        /*!
          @brief mIsIndirectCall specifies whether an opcode indirectly calls a
                 function.
        */
        const bool mIsIndirectCall;

        /*!
          @brief mIsReturn specifies whether an opcode returns from a function
                 call.
        */
        const bool mIsReturn;

        /*!
          @brief mIsConditionalJump specifies whether an opcode performs a
                 conditional jump.
        */
        const bool mIsConditionalJump;

        /*!
          @brief mIsUnconditionalJump specifies whether an opcode performs an
                 unconditional jump.
        */
        const bool mIsUnconditionalJump;

        /*!
          @brief mIsIndirectJump specifies whether an opcode indirectly performs
                 a jump.
        */
        const bool mIsIndirectJump;

        /*!
          @brief mIsAsmDataDirective specifies whether an opcode is an assembly
                 data directive.
        */
        const bool mIsAsmDataDirective;

        /*!
          @brief mHasSideEffects specifies whether an opcode has side effects.
        */
        const bool mHasSideEffects;

    };

    /*!
      @brief Class RegisterType models generic kinds of registers of a CPU.

      Using RegisterType, different classes of registers of an actual processor
      architecture can be distinguished. Furthermore, this class provides
      strings that are used as prefix and suffix to construct a register's name.
      Different pre- and suffixes can be provided for physical and virtual
      registers of a register class.

      This class serves as base class from which register types of actual
      processor instances are derived.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class RegisterType : public WIR_InheritableEnum
    {

      public:

        /*!
          @brief getPrefixes returns a register type's specific prefixes.

          @return A reference to an array holding the register type's prefixes
                  (both physical and virtual).

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        const std::string ( & getPrefixes( void ) const )[ 2 ];

        /*!
          @brief getSuffixes returns a register type's specific suffixes.

          @return A reference to an array holding the register type's suffixes
                  (both physical and virtual).

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        const std::string ( & getSuffixes( void ) const )[ 2 ];

        /*!
          @brief getBitWidth returns a register type's bit width.

          @return The register type's bit width.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        unsigned int getBitWidth( void ) const;

        /*!
          @brief isCompatible returns whether one register type is compatible
                 with another.

          @param[in] t A const reference to the register that is required by an
                       operation format.
          @param[in] ra A const reference to an operation's actual register.
          @param[in] rf A const reference to a register as required by an
                        operation format.
          @return True iff two register types are compatible, false otherwise.

          This compatibility check is used by WIR_Operation::checkParameters in
          order to verify that register parameters inserted into some %WIR
          operation actually match with the operation's format.

          By overloading this method in derived classes, specific compatibility
          checks for individual processor architectures can be realized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        virtual bool isCompatible( const RegisterType &,
                                   const WIR_BaseRegister &,
                                   const WIR_BaseRegister & ) const;


      protected:

        /*!
          @brief No standard construction allowed, users must use the following
                 constructor below instead.
        */
        RegisterType( void ) = delete;

        /*!
          @brief Default constructor for register types.

          @param[in] __pp A const reference to a string holding the physical
                          register type's prefix.
          @param[in] __pv A const reference to a string holding the virtual
                          register type's prefix.
          @param[in] __sp A const reference to a string holding the physical
                          register type's suffix.
          @param[in] __sv A const reference to a string holding the virtual
                          register type's suffix.
          @param[in] __w The register type's bit width.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit RegisterType( const std::string &, const std::string &,
                               const std::string &, const std::string &,
                               unsigned int );


      private:

        /*!
          @brief mPrefix stores prefixes that are put in front of each register
                 name (physical and virtual, respectively).
        */
        const std::string mPrefix[ 2 ];

        /*!
          @brief Suffix stores suffixes that are put after each register name
                 (physical and virtual, respectively).
        */
        const std::string mSuffix[ 2 ];

        //! mBitWidth stores a register type's bit width.
        const unsigned int mBitWidth;

    };

    /*!
      @brief Class OperationFormat models generic machine operation formats of a
             CPU.

      This class serves as base class from which operation formats of actual
      processor instances are derived.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class OperationFormat : public WIR_InheritableEnum
    {

      public:

        /*!
          @brief getBitWidth returns an operation format's bit width.

          @return The operation format's bit width.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        unsigned int getBitWidth( void ) const;

        /*!
          @brief getSize returns an operation format's size in bytes.

          @return The operation format's byte size.

          getSize rounds upwards. I.e., if an operation format occupies 20 bits,
          getSize returns a size of 3 bytes.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        unsigned int getSize( void ) const;

        /*!
          @brief getName returns an operation format's specific name.

          @return A string that holds the operation format's name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        std::string getName( void ) const;


      protected:

        /*!
          @brief No standard construction allowed, users must use the following
                 constructor below instead.
        */
        OperationFormat( void ) = delete;

        /*!
          @brief Default constructor for operation formats.

          @param[in] __w The operation format's bit width.
          @param[in] __s A const reference to a string that holds the operation
                         format's name.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit OperationFormat( unsigned int, const std::string & );


      private:

        //! mBitWidth stores an operation format's bit width.
        const unsigned int mBitWidth;

        //! mSize stores an operation format's byte size.
        const unsigned int mSize;

        //! mName holds an operation format's readable name.
        const std::string mName;

    };


    //
    // Physical register handling.
    //

    /*!
      @brief getPhRegs returns a vector containing all physical registers of a
             processor architecture.

      @return A vector of registers.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::vector<std::reference_wrapper<const WIR_PhysicalRegister>> getPhRegs( void ) const;

    /*!
      @brief getPhRegs returns a vector containing all physical registers of a
             processor architecture of the specified type.

      @param[in] t A const reference to the desired register type.
      @return A vector of registers of the specified type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const std::vector<std::reference_wrapper<const WIR_PhysicalRegister>> getPhRegs( const RegisterType & ) const;


    //
    // Section handling.
    //

    /*!
      @brief getSections returns the set mSectionReferences.

      @return A const reference to the set mSectionReferences.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    const WIR_SectionSet &getSections( void ) const;

    /*!
      @brief begin returns an iterator to the first section of a processor.

      @return A const iterator pointing to the first section.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SectionSet::const_iterator begin( void ) const;

    /*!
      @brief end returns an iterator to the end of a processor's section list.

      @return A const iterator pointing to the position after the last section
              of a processor.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SectionSet::const_iterator end( void ) const;

    /*!
      @brief findSection finds a WIR_Section with the specified ID in set
             mSectionReferences.

      @param[in] id A section's ID to be found.
      @return An iterator pointing to the found element with the specified ID,
              or the end() iterator otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SectionSet::const_iterator findSection( WIR_id_t ) const;

    /*!
      @brief findSection finds a WIR_Section with the specified name in set
             mSectionReferences.

      @param[in] n A section's name to be found.
      @return An iterator pointing to the found element with the specified name,
              or the end() iterator otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SectionSet::const_iterator findSection( const std::string & ) const;

    /*!
      @brief findSectionByAt finds the WIR_Section with the specified Loaded
             Memory Address (LMA).

      @param[in] a A section's LMA.
      @return An iterator pointing to the found element with the specified LMA,
              or the end() iterator otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SectionSet::const_iterator findSectionByAt( const WIR_MemoryAddress & ) const;

    /*!
      @brief findSectionByStart finds the WIR_Section with the specified Virtual
             Memory Address (VMA).

      @param[in] a A section's VMA.
      @return An iterator pointing to the found element with the specified VMA,
              or the end() iterator otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SectionSet::const_iterator findSectionByStart( const WIR_MemoryAddress & ) const;

    /*!
      @brief getTextSection returns the default text section ".text".

      @return A reference to the section with name ".text".

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section &getTextSection( void ) const;

    /*!
      @brief getDataSection returns the default initialized data section
             ".data".

      @return A reference to the section with name ".data".

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section &getDataSection( void ) const;

    /*!
      @brief getBssSection returns the default uninitialized data section
             ".bss".

      @return A reference to the section with name ".bss".

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section &getBssSection( void ) const;

    /*!
      @brief getRODataSection returns the default read-only data section
             ".rodata".

      @return A reference to the section with name ".rodata".

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section &getRODataSection( void ) const;

    /*!
      @brief containsSection returns whether set mSectionReferences contains a
             WIR_Section with the specified ID.

      @param[in] id A section's ID to be found.
      @return true if mSectionReferences contains an object with the given ID,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSection( WIR_id_t ) const;

    /*!
      @brief containsSection returns whether set mSectionReferences contains a
             WIR_Section with the specified name.

      @param[in] n A section's name to be found.
      @return true if mSectionReferences contains an object with the given name,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSection( const std::string & ) const;

    /*!
      @brief containsSection returns whether set mSectionReferences contains a
             WIR_Section with the specified Loaded Memory Address (LMA).

      @param[in] a A section's LMA to be found.
      @return true if mSectionReferences contains an object with the given LMA,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSectionAt( const WIR_MemoryAddress & ) const;

    /*!
      @brief containsSection returns whether set mSectionReferences contains a
             WIR_Section with the specified Virtual Memory Address (VMA).

      @param[in] a A section's VMA to be found.
      @return true if mSectionReferences contains an object with the given VMA,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    bool containsSectionStart( const WIR_MemoryAddress & ) const;


  protected:

    /*!
      @brief Class Registrator serves to initialize static members of %WIR
             processors and automatically registers a new unique ID per
             processor type.

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
          @brief Default constructor registering a new processor type.

          @param[out] id A reference that finally holds the new processor type's
                         unique ID.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        explicit Registrator( WIR_id_t & );

        /*!
          @brief touch is a dummy method that just serves to activate the
                 initialization of static data members.

          Objects of class 'Registrator' are used as static initializers for
          class WIR_Processor. Since WIR_Processor is a templated class,
          instantiation of its static data members does not occur until these
          are explicitly referenced. For this purpose, this method is provided:
          WIR_Processor can 'touch' its static data member so that it will get
          initialized.

          @author Heiko Falk <Heiko.Falk@tuhh.de>
        */
        void touch( void );

    };

    /*!
      @brief setProcessorName sets a processor's name as given by its
             manufacturer.

      @param[in] s A const reference to a string holding the processor's name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setProcessorName( const std::string & );

    /*!
      @brief setProcessorName sets a processor's name as given by its
             manufacturer.

      @param[in] s An R-value reference to a string holding the processor's
                   name.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setProcessorName( std::string && );

    /*!
      @brief setISAName sets a processor's description of its instruction set
             architecture (ISA).

      @param[in] s A const reference to a string holding the ISA description.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setISAName( const std::string & );

    /*!
      @brief setISAName sets a processor's description of its instruction set
             architecture (ISA).

      @param[in] s An R-value reference to a string holding the ISA description.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setISAName( std::string && );

    /*!
      @brief setClockFrequency sets a processor's clock frequency.

      @param[in] f The processor's clock frequency in Hertz.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setClockFrequency( unsigned long long );

    /*!
      @brief setVoltage sets a processor core's voltage.

      @param[in] u The processor's operation voltage.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void setVoltage( float );

    /*!
      @brief addPhReg serves to add physical registers to some actual processor
             model.

      @param[in] s A const reference to a string holding the physical register's
                   processor-specific name (excluding its pre- and suffixes as
                   specified using class RegisterType).
      @param[in] sp A Boolean denoting whether the physical register is the
                    proessor's stack pointer.
      @tparam WIRPhReg This template argument is used to specify the actual type
                       of the physical register to be added and should thus be
                       some class that inherits from WIR_PhysicalRegister.

      Physical registers can only be added to a processor model during its
      construction. Thus, addPhReg should be called by the constructors of
      classes that inherit from WIR_Processor and that model actual processor
      architectures.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    template<typename WIRPhReg>
    WIRPhReg &addPhReg( const std::string &s, bool sp = false )
    {
      WIRPhReg *p = new WIRPhReg( s, sp );
      mPhRegPointers.push_back( std::unique_ptr<WIR_PhysicalRegister>( p ) );
      p->onInsert( this );
      mPhRegReferences.push_back( *p );

      return( *p );
    };

    /*!
      @brief mPhRegPointers holds managed pointers to all physical registers
             that a processor architecture possesses.
    */
    std::vector<std::unique_ptr<WIR_PhysicalRegister>> mPhRegPointers;

    /*!
      @brief mPhRegReferences holds (wrapped) references to all physical
             registers that a processor architecture possesses.
    */
    std::vector<std::reference_wrapper<WIR_PhysicalRegister>> mPhRegReferences;


  private:

    friend class WIR_Registry;
    friend class WIR_System;
    friend class Registrator;

    /*!
      @brief getMaxDelay returns the maximum number of clock cycles for which
             a system component can delay a memory access.

      @param[in] d The delay by which an access may be delayed by other
                   components in the memory hierarchy that are behind this
                   current component.
      @return The maximal delay for an access to this component.

      This function does not make real sense for processor cores but is
      inherited from WIR_SystemComponent. Thus, its implementation here is
      private and only a dummy.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual unsigned int getMaxDelay( const unsigned int d = 0 ) const;

    /*!
      @brief registerNewProcessorType registers a new processor type.

      @param[out] id A reference that finally holds the unique ID for the new
                     processor type.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void registerNewProcessorType( WIR_id_t & );

    /*!
      @brief clone creates a copy of a %WIR processor.

      @return A pointer to the newly created copy of this processor.

      In the derived class WIR_Processor<typename T>, clone shall just call the
      copy constructor so that by using the template argument, the correct copy
      constructor of the actual processor class (which is unknown here) is
      called. For this purpose, clone is a purely virtual method here.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    virtual WIR_BaseProcessor *clone( void ) const = 0;

    /*!
      @brief copyProcessor performs actions common to the copy constructor and
             copy assignment operator of %WIR processors.

      @param[in] __o A const reference to another object to be copied.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void copyProcessor( const WIR_BaseProcessor & );


    //
    // Section handling.
    //

    /*!
      @brief insertSection adds a new WIR_Section to the sets mSections and
             mSectionReferences.

      @param[in] o An R-value reference to the WIR_Section to be move-added.
      @return A reference to the newly inserted element.

      insertSection asserts if a section with the same name as an already
      existing section is to be inserted. The content of o is moved to the new
      set element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_Section &insertSection( WIR_Section && );

    /*!
      @brief ersaseSection removes a single WIR_Section from the specified
             position.

      @param[in] pos An iterator denoting the position where the element is
                     removed.
      @return An iterator pointing to the element following the erased element.

      eraseSection asserts if one of the default sections .text, .data, .bss or
      .data is removed. This destroys the removed element.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    WIR_SectionSet::iterator eraseSection( WIR_SectionSet::const_iterator );

    /*!
      @brief clearSections removes all elements from sets mSections and
             mSectionReferences.

      This destroys all removed elements.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    void clearSections( void );

    //! mTypeID holds the next free ID for new processor types.
    static WIR_id_t mTypeID;

    /*!
      @brief mProcessorName holds a description of a processor's name as given
             by its manufacturer.
    */
    std::string mProcessorName;

    /*!
      @brief mISAName holds a description of a processor's instruction set
             architecture (ISA).
    */
    std::string mISAName;

    //! mClockFrequency holds a processor's clock frequency in Hertz.
    unsigned long long mClockFrequency;

    //! mVoltage holds a processor core's voltage.
    float mVoltage;

    //! mSections holds all stored %WIR sections.
    std::set<WIR_Section> mSections;

    /*!
      @brief mSectionReferences holds (wrapped) references to all stored %WIR
             sections.
    */
    WIR_SectionSet mSectionReferences;

};

}       // namespace WIR

#endif  // _WIR_BASEPROCESSOR_H
