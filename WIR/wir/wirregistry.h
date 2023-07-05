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
  @file wirregistry.h
  @brief This file provides the interface of the central %WIR registry.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


#ifndef _WIR_REGISTRY_H
#define _WIR_REGISTRY_H


//
// Include section
//

// Include standard headers
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wirbaseprocessor.h>
#include <wir/wirio.h>
#include <wir/wirmisc.h>
#include <wir/wiroperationformat.h>
#include <wir/wirtypes.h>


//
// Header section
//

namespace WIR {

/*!
  @brief Class WIR_Registry is used internally to register all %WIR objects
         created during compilation by using their numerical IDs and their
         pointers.

  This class is for WIR-internal use only. For this reason, WIR_Registry has no
  public interface at all. It only consists of private and static members that
  keep the registry's map up to date. Only very few specific friend classes and
  functions are allowed to access the registry.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/
class WIR_Registry
{

  public:

    // This class has almost no public interface. Only the following friends are
    // allowed to manipulate this registry.
    friend void WIR_Init( void );
    friend class WIR_Operation;
    friend class WIR_System;

    WIR_Registry( void ) = delete;
    WIR_Registry( const WIR_Registry & ) = delete;
    WIR_Registry( WIR_Registry && ) = delete;
    ~WIR_Registry( void ) = delete;
    WIR_Registry & operator = ( const WIR_Registry & ) = delete;
    WIR_Registry & operator = ( WIR_Registry && ) = delete;

    /*!
      @brief registerProcessor registers the given %WIR processor model under
             its ISA name.

      @param[in] p An R-value reference to a processor model to be registered.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void registerProcessor( WIR_BaseProcessor && );

    /*!
      @brief registerOperationFormat registers the given %WIR operation format
             under the given ID.

      @param[in] id A const reference to an operation format's identifier.
      @param[in] f An R-value reference to a %WIR operation format to be moved.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void registerOperationFormat( const WIR_BaseProcessor::OperationFormat &,
                                         WIR_OperationFormat && );

    /*!
      @brief registerOpCode registers the given operation format as a legal
             format for operations with the given opcode.

      @param[in] o A const reference to an opcode.
      @param[in] f A const reference to an operation format's identifier.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static void registerOpCode( const WIR_BaseProcessor::OpCode &,
                                const WIR_BaseProcessor::OperationFormat & );

    /*!
      @brief registerBasicBlockDumper registers a function pointer for a
             processor-specific WIR_BasicBlock I/O function under the numerical
             ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   basic blocks.

      registerBasicBlockDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerBasicBlockDumper( unsigned int,
                                          WIR_BasicBlockDumper );

    /*!
      @brief getBasicBlockDumper returns a pointer to a processor-specific
             WIR_BasicBlock I/O function for the given processor architecture
             ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              basic blocks.

      If some unknown/unregistered processor ID is passed to
      getBasicBlockDumper, the function pointer registered for the default ID 0
      is returned.
    */
    static WIR_BasicBlockDumper getBasicBlockDumper( unsigned int );

    /*!
      @brief registerBlockLabelDumper registers a function pointer for a
             processor-specific WIR_BasicBlock label I/O function under the
             numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   basic block labels.

      registerBlockLabelDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerBlockLabelDumper( unsigned int,
                                          WIR_BlockLabelDumper );

    /*!
      @brief getBlockLabelDumper returns a pointer to a processor-specific
             WIR_BasicBlock label I/O function for the given processor
             architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              basic block labels.

      If some unknown/unregistered processor ID is passed to
      getBlockLabelDumper, the function pointer registered for the default ID 0
      is returned.
    */
    static WIR_BlockLabelDumper getBlockLabelDumper( unsigned int );

    /*!
      @brief registerCommentDumper registers a function pointer for a
             processor-specific WIR_Comment I/O function under the numerical ID
             of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   comments.

      registerCommentDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerCommentDumper( unsigned int, WIR_CommentDumper );

    /*!
      @brief getCommentDumper returns a pointer to a processor-specific
             WIR_Comment I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR comments.

      If some unknown/unregistered processor ID is passed to getCommentDumper,
      the function pointer registered for the default ID 0 is returned.
    */
    static WIR_CommentDumper getCommentDumper( unsigned int );

    /*!
      @brief registerCompilationUnitDumper registers a function pointer for a
             processor-specific WIR_CompilationUnit I/O function under the
             numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   compilation units.

      registerCompilationUnitDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerCompilationUnitDumper( unsigned int,
                                               WIR_CompilationUnitDumper );

    /*!
      @brief getCompilationUnitDumper returns a pointer to a processor-specific
             WIR_CompilationUnit I/O function for the given processor
             architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              compilation units.

      If some unknown/unregistered processor ID is passed to
      getCompilationUnitDumper, the function pointer registered for the default
      ID 0 is returned.
    */
    static WIR_CompilationUnitDumper getCompilationUnitDumper( unsigned int );

    /*!
      @brief registerDataDumper registers a function pointer for a processor-
             specific WIR_Data I/O function under the numerical ID of some
             processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   data objects.

      registerDataDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerDataDumper( unsigned int, WIR_DataDumper );

    /*!
      @brief getDataDumper returns a pointer to a processor-specific WIR_Data
             I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR data
              objects.

      If some unknown/unregistered processor ID is passed to getDataDumper, the
      function pointer registered for the default ID 0 is returned.
    */
    static WIR_DataDumper getDataDumper( unsigned int );

    /*!
      @brief registerDataSectionDumper registers a function pointer for a
             processor-specific I/O function for data sections under the
             numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for data
                   sections.

      registerDataDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerDataSectionDumper( unsigned int,
                                           WIR_DataSectionDumper );

    /*!
      @brief getDataSectionDumper returns a pointer to a processor-specific data
             section I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for data sections.

      If some unknown/unregistered processor ID is passed to
      getDataSectionDumper, the function pointer registered for the default ID 0
      is returned.
    */
    static WIR_DataSectionDumper getDataSectionDumper( unsigned int );

    /*!
      @brief registerFileInfoDumper registers a function pointer for a
             processor-specific WIR_FileInfo I/O function under the numerical ID
             of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   file information.

      registerFileInfoDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerFileInfoDumper( unsigned int, WIR_FileInfoDumper );

    /*!
      @brief getFileInfoDumper returns a pointer to a processor-specific
             WIR_FileInfo I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR file
              information.

      If some unknown/unregistered processor ID is passed to getFileInfoDumper,
      the function pointer registered for the default ID 0 is returned.
    */
    static WIR_FileInfoDumper getFileInfoDumper( unsigned int );

    /*!
      @brief registerFunctionDumper registers a function pointer for a
             processor-specific WIR_Function I/O function under the numerical ID
             of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   functions.

      registerFunctionDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerFunctionDumper( unsigned int, WIR_FunctionDumper );

    /*!
      @brief getFunctionDumper returns a pointer to a processor-specific
             WIR_Function I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              functions.

      If some unknown/unregistered processor ID is passed to getFunctionDumper,
      the function pointer registered for the default ID 0 is returned.
    */
    static WIR_FunctionDumper getFunctionDumper( unsigned int );

    /*!
      @brief registerInstructionDumper registers a function pointer for a
             processor-specific WIR_Instruction I/O function under the numerical
             ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   instructions.

      registerInstructionDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerInstructionDumper( unsigned int,
                                           WIR_InstructionDumper );

    /*!
      @brief getInstructionDumper returns a pointer to a processor-specific
             WIR_Instruction I/O function for the given processor architecture
             ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              instructions.

      If some unknown/unregistered processor ID is passed to
      getInstructionDumper, the function pointer registered for the default ID 0
      is returned.
    */
    static WIR_InstructionDumper getInstructionDumper( unsigned int );

    /*!
      @brief registerOperationDumper registers a function pointer for a
             processor-specific WIR_Operation I/O function under the numerical
             ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   operations.

      registerOperationDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerOperationDumper( unsigned int, WIR_OperationDumper );

    /*!
      @brief getOperationDumper returns a pointer to a processor-specific
             WIR_Operation I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              operations.

      If some unknown/unregistered processor ID is passed to getOperationDumper,
      the function pointer registered for the default ID 0 is returned.
    */
    static WIR_OperationDumper getOperationDumper( unsigned int );

    /*!
      @brief registerAddressingModeParameterDumper registers a function pointer
             for a processor-specific WIR_AddressingModeParameter I/O function
             under the numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   addressing mode parameters.

      registerAddressingModeParameterDumper shall be called by the init methods
      of class WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerAddressingModeParameterDumper( unsigned int,
                                                       WIR_AddressingModeParameterDumper );

    /*!
      @brief getAddressingModeParameterDumper returns a pointer to a
             processor-specific WIR_AddressingModeParameter I/O function for the
             given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              addressing mode parameters.

      If some unknown/unregistered processor ID is passed to
      getAddressingModeParameterDumper, the function pointer registered for the
      default ID 0 is returned.
    */
    static WIR_AddressingModeParameterDumper getAddressingModeParameterDumper( unsigned int );

    /*!
      @brief registerConditionFieldParameterDumper registers a function pointer
             for a processor-specific WIR_ConditionFieldParameter I/O function
             under the numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   condition field parameters.

      registerConditionFieldParameterDumper shall be called by the init methods
      of class WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerConditionFieldParameterDumper( unsigned int,
                                                       WIR_ConditionFieldParameterDumper );

    /*!
      @brief getConditionFieldParameterDumper returns a pointer to a
             processor-specific WIR_ConditionFieldParameter I/O function for the
             given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR condition
              field parameters.

      If some unknown/unregistered processor ID is passed to
      getConditionFieldParameterDumper, the function pointer registered for the
      default ID 0 is returned.
    */
    static WIR_ConditionFieldParameterDumper getConditionFieldParameterDumper( unsigned int );

    /*!
      @brief registerImmediateParameterDumper registers a function pointer for a
             processor-specific WIR_BaseImmediateParameter I/O function under
             the numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   immediate parameters.

      registerImmediateParameterDumper shall be called by the init methods of
      class WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerImmediateParameterDumper( unsigned int,
                                                  WIR_ImmediateParameterDumper );

    /*!
      @brief getImmediateParameterDumper returns a pointer to a
             processor-specific WIR_BaseImmediateParameter I/O function for
             the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              immediate parameters.

      If some unknown/unregistered processor ID is passed to
      getImmediateParameterDumper, the function pointer registered for the
      default ID 0 is returned.
    */
    static WIR_ImmediateParameterDumper getImmediateParameterDumper( unsigned int );

    /*!
      @brief registerLabelParameterDumper registers a function pointer for a
             processor-specific WIR_LabelParameter I/O function under the
             numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   label parameters.

      registerLabelParameterDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerLabelParameterDumper( unsigned int,
                                              WIR_LabelParameterDumper );

    /*!
      @brief getLabelParameterDumper returns a pointer to a processor-specific
             WIR_LabelParameter I/O function for the given processor
             architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR label
              parameters.

      If some unknown/unregistered processor ID is passed to
      getLabelParameterDumper, the function pointer registered for the
      default ID 0 is returned.
    */
    static WIR_LabelParameterDumper getLabelParameterDumper( unsigned int );

    /*!
      @brief registerLdScriptDumper registers a function pointer for a
             processor-specific linker script I/O function under the numerical
             ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for linker
                   script.

      registerLdScriptDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerLdScriptDumper( unsigned int, WIR_LdScriptDumper );

    /*!
      @brief getLdScriptDumper returns a pointer to a processor-specific linker
             script I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for linker scripts.

      If some unknown/unregistered processor ID is passed to getLdScriptDumper,
      the function pointer registered for the default ID 0 is returned.
    */
    static WIR_LdScriptDumper getLdScriptDumper( unsigned int );

    /*!
      @brief registerLdScriptSectionDumper registers a function pointer for a
             processor-specific section linker script I/O function under the
             numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for sections
                   within linker scripts.

      registerLdScriptSectionDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerLdScriptSectionDumper( unsigned int,
                                               WIR_LdScriptSectionDumper );

    /*!
      @brief getLdScriptSectionDumper returns a pointer to a processor-specific
             section linker script I/O function for the given processor
             architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for sections within
              linker scripts.

      If some unknown/unregistered processor ID is passed to
      getLdScriptSectionDumper, the function pointer registered for the default
      ID 0 is returned.
    */
    static WIR_LdScriptSectionDumper getLdScriptSectionDumper( unsigned int );

    /*!
      @brief registerRegisterParameterDumper registers a function pointer for a
             processor-specific WIR_RegisterParameter I/O function under the
             numerical ID of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   register parameters.

      registerRegisterParameterDumper shall be called by the init methods of
      class WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerRegisterParameterDumper( unsigned int,
                                                 WIR_RegisterParameterDumper );

    /*!
      @brief getRegisterParameterDumper returns a pointer to a
             processor-specific WIR_RegisterParameter I/O function for the given
             processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              register parameters.

      If some unknown/unregistered processor ID is passed to
      getRegisterParameterDumper, the function pointer registered for the
      default ID 0 is returned.
    */
    static WIR_RegisterParameterDumper getRegisterParameterDumper( unsigned int );

    /*!
      @brief registerSystemDumper registers a function pointer for a
             processor-specific WIR_System I/O function under the numerical ID
             of some processor architecture.

      @param[in] id A processor architecture's unique numerical identifier.
      @param[in] f A pointer to a processor-specific I/O function for %WIR
                   systems.

      registerSystemDumper shall be called by the init methods of class
      WIR_Processor in order to setup processor-specific I/O routines.
    */
    static void registerSystemDumper( unsigned int, WIR_SystemDumper );

    /*!
      @brief getSystemDumper returns a pointer to a processor-specific
             WIR_System I/O function for the given processor architecture ID.

      @param[in] id A processor architecture's unique numerical identifier.
      @return A pointer to a processor-specific I/O function for %WIR
              systems.

      If some unknown/unregistered processor ID is passed to getSystemDumper,
      the function pointer registered for the default ID 0 is returned.
    */
    static WIR_SystemDumper getSystemDumper( unsigned int );

    /*!
      @brief isLegalOperationFormat returns whether the given operation format
             is legal for the specified opcode.

      @param[in] o A const reference to an opcode.
      @param[in] f A const reference to an operation format's identifier.
      @return true if the operation format and opcode match, false otherwise.

      If the given opcode has not been registered before, isLegalOperationFormat
      fails with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isLegalOperationFormat( const WIR_BaseProcessor::OpCode &,
                                        const WIR_BaseProcessor::OperationFormat & );


  private:

    /*!
      @brief isProcessorRegistered checks whether a processor model with the
             given ISA name has been registered using registerProcessor before.

      @param[in] n A processor's ISA name.
      @return true iff a processor with the given ISA name has been registered,
              false otherwise.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static bool isProcessorRegistered( const std::string & );

    /*!
      @brief getNewProcessor looks up a processor model with the given ISA name
             in map mProcessorMap and generates a new instance of this
             processor.

      @param[in] n A registered processor's ISA name.
      @return A pointer to the newly created processor instance. Note that the
              caller of getNewProcessor (i.e., class WIR_System) is responsible
              for deleting all these processor instances.

      If the given ISA name has not yet been registered before using
      registerProcessor, getNewProcessor fails with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static WIR_BaseProcessor *getNewProcessor( const std::string & );

    /*!
      @brief getOperationFormat looks up a machine operation format from map
             mOperationFormatMap.

      @param[in] id A const reference to an operation format's identifier to be
                    looked up.
      @return A const reference to the found %WIR operation format.

      If the given ID does not refer to a machine operation format or does not
      exist at all, getOperationFormat fails with an assertion.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    static const WIR_OperationFormat &getOperationFormat( const WIR_BaseProcessor::OperationFormat & );

    /*!
      @brief mProcessorMap processor names to an actual instance of a processor.
    */
    static std::map<std::string, std::unique_ptr<WIR_BaseProcessor>> mProcessorMap;

    /*!
      @brief mOperationFormatMap maps the numerical ID of an operation format to
             an actual operation format.
    */
    static std::map<unsigned int, WIR_OperationFormat> mOperationFormatMap;

    /*!
      @brief operationFormatRegisters is a helper class collecting all registers
             occurring in registered operation formats.

      The only purpose of this class is to delete these registers properly.

      @author Heiko Falk <Heiko.Falk@tuhh.de>
    */
    class operationFormatRegisters
    {
      public:

        //! Destructor taking care of the registers' deletion.
        ~operationFormatRegisters( void );

        std::set<WIR_BaseRegister *> mRegisters;
    };

    //! mOperationFormatRegisters collects all registers of operation formats.
    static operationFormatRegisters mOperationFormatRegisters;

    /*!
      @brief mOpCodeMap maps the numerical ID of an opcode to the IDs of all
             operation formats that are supported by this particular opcode.
    */
    static std::map<unsigned int, std::set<unsigned int>> mOpCodeMap;

    /*!
      @brief mBasicBlockDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_BasicBlock I/O function.
    */
    static std::map<unsigned int, WIR_BasicBlockDumper> mBasicBlockDumperMap;

    /*!
      @brief mBlockLabelDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific I/O
             function for basic block labels.
    */
    static std::map<unsigned int, WIR_BlockLabelDumper> mBlockLabelDumperMap;

    /*!
      @brief mCommentDumperMap maps the numerical ID of a processor architecture
             to a function pointer for a processor-specific WIR_Comment I/O
             function.
    */
    static std::map<unsigned int, WIR_CommentDumper> mCommentDumperMap;

    /*!
      @brief mCompilationUnitDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_CompilationUnit I/O function.
    */
    static std::map<unsigned int, WIR_CompilationUnitDumper> mCompilationUnitDumperMap;

    /*!
      @brief mDataDumperMap maps the numerical ID of a processor architecture to
             a function pointer for a processor-specific WIR_Data I/O function.
    */
    static std::map<unsigned int, WIR_DataDumper> mDataDumperMap;

    /*!
      @brief mDataSectionDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             I/O function for data sections.
    */
    static std::map<unsigned int, WIR_DataSectionDumper> mDataSectionDumperMap;

    /*!
      @brief mFileInfoDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_FileInfo I/O function.
    */
    static std::map<unsigned int, WIR_FileInfoDumper> mFileInfoDumperMap;

    /*!
      @brief mFunctionDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_Function I/O function.
    */
    static std::map<unsigned int, WIR_FunctionDumper> mFunctionDumperMap;

    /*!
      @brief mInstructionDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_Instruction I/O function.
    */
    static std::map<unsigned int, WIR_InstructionDumper> mInstructionDumperMap;

    /*!
      @brief mOperationDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_Operation I/O function.
    */
    static std::map<unsigned int, WIR_OperationDumper> mOperationDumperMap;

    /*!
      @brief mAddressingModeParameterDumperMap maps the numerical ID of a
             processor architecture to a function pointer for a
             processor-specific WIR_AddressingModeParameter I/O function.
    */
    static std::map<unsigned int, WIR_AddressingModeParameterDumper> mAddressingModeParameterDumperMap;

    /*!
      @brief mConditionFieldParameterDumperMap maps the numerical ID of a
             processor architecture to a function pointer for a
             processor-specific WIR_ConditionFieldParameter I/O function.
    */
    static std::map<unsigned int, WIR_ConditionFieldParameterDumper> mConditionFieldParameterDumperMap;

    /*!
      @brief mImmediateParameterDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_BaseImmediateParameter I/O function.
    */
    static std::map<unsigned int, WIR_ImmediateParameterDumper> mImmediateParameterDumperMap;

    /*!
      @brief mLabelParameterDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_LabelParameter I/O function.
    */
    static std::map<unsigned int, WIR_LabelParameterDumper> mLabelParameterDumperMap;

    /*!
      @brief mLdScriptDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific linker
             script I/O function.
    */
    static std::map<unsigned int, WIR_LdScriptDumper> mLdScriptDumperMap;

    /*!
      @brief mLdScriptSectionDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific section
             linker script I/O function.
    */
    static std::map<unsigned int, WIR_LdScriptSectionDumper> mLdScriptSectionDumperMap;

    /*!
      @brief mRegisterParameterDumperMap maps the numerical ID of a processor
             architecture to a function pointer for a processor-specific
             WIR_RegisterParameter I/O function.
    */
    static std::map<unsigned int, WIR_RegisterParameterDumper> mRegisterParameterDumperMap;

    /*!
      @brief mSystemDumperMap maps the numerical ID of a processor architecture
             to a function pointer for a processor-specific WIR_System I/O
             function.
    */
    static std::map<unsigned int, WIR_SystemDumper> mSystemDumperMap;

};

}       // namespace WIR

#endif  // _WIR_REGISTRY_H
