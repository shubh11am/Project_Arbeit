/*

   This source file belongs to the

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
  @file wirregistry.cc
  @brief This file implements the central %WIR registry.

  @author Heiko Falk <Heiko.Falk@tuhh.de>
*/


//
// Include section
//

#ifdef HAVE_CONFIG_H
#include <config_wir.h>
#endif

// Include libuseful headers
#include <libuseful/debugmacros.h>
#include <libuseful/io.h>

// Include WIR headers
#include <wir/wir.h>


//
// Code section
//

namespace WIR {


using namespace std;


// mProcessorMap maps processor names to an actual instance of a processor.
map<string, unique_ptr<WIR_BaseProcessor>> WIR_Registry::mProcessorMap;

// mOperationFormatMap maps the numerical ID of an operation format to an actual
// operation format.
map<unsigned int, WIR_OperationFormat> WIR_Registry::mOperationFormatMap;

// mOperationFormatRegisters collects all registers of operation formats.
WIR_Registry::operationFormatRegisters WIR_Registry::mOperationFormatRegisters;

// mOpCodeMap maps the numerical ID of an opcode to the IDs of all operation
// formats that are supported by this particular opcode.
map<unsigned int, set<unsigned int>> WIR_Registry::mOpCodeMap;

// mBasicBlockDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_BasicBlock I/O function.
map<unsigned int, WIR_BasicBlockDumper> WIR_Registry::mBasicBlockDumperMap;

// mBlockLabelDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific I/O function for basic block
// labels.
map<unsigned int, WIR_BlockLabelDumper> WIR_Registry::mBlockLabelDumperMap;

// mCommentDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_Commment I/O function.
map<unsigned int, WIR_CommentDumper> WIR_Registry::mCommentDumperMap;

// mCompilationUnitDumperMap maps the numerical ID of a processor architecture
// to a function pointer for a processor-specific WIR_CompilationUnit I/O
// function.
map<unsigned int, WIR_CompilationUnitDumper> WIR_Registry::mCompilationUnitDumperMap;

// mDataDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_Data I/O function.
map<unsigned int, WIR_DataDumper> WIR_Registry::mDataDumperMap;

// mDataSectionDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific I/O function for data sections.
map<unsigned int, WIR_DataSectionDumper> WIR_Registry::mDataSectionDumperMap;

// mFileInfoDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_FileInfo I/O function.
map<unsigned int, WIR_FileInfoDumper> WIR_Registry::mFileInfoDumperMap;

// mFunctionDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_Function I/O function.
map<unsigned int, WIR_FunctionDumper> WIR_Registry::mFunctionDumperMap;

// mInstructionDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_Instruction I/O function.
map<unsigned int, WIR_InstructionDumper> WIR_Registry::mInstructionDumperMap;

// mOperationDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_Operation I/O function.
map<unsigned int, WIR_OperationDumper> WIR_Registry::mOperationDumperMap;

// mAddressingModeParameterDumperMap maps the numerical ID of a processor
// architecture to a function pointer for a processor-specific
// WIR_AddressingModeParameter I/O function.
map<unsigned int, WIR_AddressingModeParameterDumper> WIR_Registry::mAddressingModeParameterDumperMap;

// mConditionFieldParameterDumperMap maps the numerical ID of a processor
// architecture to a function pointer for a processor-specific
// WIR_ConditionFieldParameter I/O function.
map<unsigned int, WIR_ConditionFieldParameterDumper> WIR_Registry::mConditionFieldParameterDumperMap;

// mImmediateParameterDumperMap maps the numerical ID of a processor
// architecture to a function pointer for a processor-specific
// WIR_BaseImmediateParameter I/O function.
map<unsigned int, WIR_ImmediateParameterDumper> WIR_Registry::mImmediateParameterDumperMap;

// mLabelParameterDumperMap maps the numerical ID of a processor architecture to
// a function pointer for a processor-specific WIR_LabelParameter I/O function.
map<unsigned int, WIR_LabelParameterDumper> WIR_Registry::mLabelParameterDumperMap;

// mLdScriptDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific linker script I/O function.
map<unsigned int, WIR_LdScriptDumper> WIR_Registry::mLdScriptDumperMap;

// mLdScriptSectionDumperMap maps the numerical ID of a processor architecture
// to a function pointer for a processor-specific section linker script I/O
// function.
map<unsigned int, WIR_LdScriptSectionDumper> WIR_Registry::mLdScriptSectionDumperMap;

// mRegisterParameterDumperMap maps the numerical ID of a processor architecture
// to a function pointer for a processor-specific WIR_RegisterParameter I/O
// function.
map<unsigned int, WIR_RegisterParameterDumper> WIR_Registry::mRegisterParameterDumperMap;

// mSystemDumperMap maps the numerical ID of a processor architecture to a
// function pointer for a processor-specific WIR_System I/O function.
map<unsigned int, WIR_SystemDumper> WIR_Registry::mSystemDumperMap;


//
// Public class methods
//

/*
  registerProcessor registers the given WIR processor model under its ISA name.
*/
void WIR_Registry::registerProcessor( WIR_BaseProcessor &&p )
{
  DSTART( "static void WIR_Registry::registerProcessor(WIR_BaseProcessor&&)" );

  WIR_BaseProcessor *c = p.clone();

  DOUT( "Registering processor ISA '" << c->getISAName() << "'." << endl );

  mProcessorMap[ c->getISAName() ] = unique_ptr<WIR_BaseProcessor>( c );
};


/*
  registerOperationFormat registers the given %WIR operation format under the
  given ID.
*/
void WIR_Registry::registerOperationFormat( const WIR_BaseProcessor::OperationFormat &id,
                                            WIR_OperationFormat &&f )
{
  DSTART(
    "static void WIR_Registry::registerOperationFormat(const WIR_BaseProcessor::OperationFormat&, WIR_OperationFormat&&)" );

  for ( WIR_Parameter &p : f )
    if ( p.getType() == WIR_ParameterType::reg ) {
      // Take over control over all registers occuring in register parameters of
      // an operation format.
      auto &regP = dynamic_cast<WIR_RegisterParameter &>( p );
      WIR_BaseRegister &reg = regP.getRegister();
      WIR_BaseRegister *r = &reg;

      // Determine root of register hierarchy.
      if ( r->isVirtual() )
        r = &(dynamic_cast<WIR_VirtualRegister *>( r )->getRoot());
      else
        r = &(dynamic_cast<WIR_PhysicalRegister *>( r )->getRoot());

      if ( mOperationFormatRegisters.mRegisters.count( r ) == 0 ) {
        mOperationFormatRegisters.mRegisters.insert( r );
      }
    }

  mOperationFormatMap[ id.mID ] = move( f );
};


/*
  registerOpCode registers the given operation format as a legal format for
  operations with the given opcode.
*/
void WIR_Registry::registerOpCode( const WIR_BaseProcessor::OpCode &o,
                                   const WIR_BaseProcessor::OperationFormat &f )
{
  DSTART(
    "static void WIR_Registry::registerOpCode(const WIR_BaseProcessor::OpCode&, const WIR_BaseProcessor::OperationFormat&)" );

  ufAssertT(
    mOperationFormatMap.find( f.mID ) != mOperationFormatMap.end(),
    "Illegal attempt to register opcode '" << o.getName() <<
    "' under unknown operation format '" << f.getName() << "'." );

  DOUT(
    "Registering operation format '" << f.getName() << "' for opcode '" <<
    o.getName() << "'." << endl );

  mOpCodeMap[ o.mID ].insert( f.mID );
};


/*
  registerBasicBlockDumper registers a function pointer for a
  processor-specific WIR_BasicBlock I/O function under the numerical ID of some
  processor architecture.

  registerBasicBlockDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerBasicBlockDumper( unsigned int id,
                                             WIR_BasicBlockDumper f )
{
  DSTART(
    "static void WIR_Registry::registerBasicBlockDumper(unsigned int, WIR_BasicBlockDumper)" );

  ufAssert( mBasicBlockDumperMap.find( id ) == mBasicBlockDumperMap.end() );

  mBasicBlockDumperMap[ id ] = f;
};


/*
  getBasicBlockDumper returns a pointer to a processor-specific WIR_BasicBlock
  I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getBasicBlockDumper,
  the function pointer registered for the default ID 0 is returned.
*/
WIR_BasicBlockDumper WIR_Registry::getBasicBlockDumper( unsigned int id )
{
  DSTART(
    "static WIR_BasicBlockDumper WIR_Registry::getBasicBlockDumper(unsigned int)" );

  auto it = mBasicBlockDumperMap.find( id );

  if ( it == mBasicBlockDumperMap.end() )
    return( mBasicBlockDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerBlockLabelDumper registers a function pointer for a processor-specific
  WIR_BasicBlock label I/O function under the numerical ID of some processor
  architecture.

  registerBlockLabelDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerBlockLabelDumper( unsigned int id,
                                             WIR_BlockLabelDumper f )
{
  DSTART(
    "static void WIR_Registry::registerBlockLabelDumper(unsigned int, WIR_BlockLabelDumper)" );

  ufAssert( mBlockLabelDumperMap.find( id ) == mBlockLabelDumperMap.end() );

  mBlockLabelDumperMap[ id ] = f;
};


/*
  getBlockLabelDumper returns a pointer to a processor-specific WIR_BasicBlock
  label I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getBlockLabelDumper,
  the function pointer registered for the default ID 0 is returned.
*/
WIR_BlockLabelDumper WIR_Registry::getBlockLabelDumper( unsigned int id )
{
  DSTART(
    "static WIR_BlockLabelDumper WIR_Registry::getBlockLabelDumper(unsigned int)" );

  auto it = mBlockLabelDumperMap.find( id );

  if ( it == mBlockLabelDumperMap.end() )
    return( mBlockLabelDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerCommentDumper registers a function pointer for a processor-specific
  WIR_Comment I/O function under the numerical ID of some processor
  architecture.

  registerCommentDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerCommentDumper( unsigned int id, WIR_CommentDumper f )
{
  DSTART(
    "static void WIR_Registry::registerCommentDumper(unsigned int, WIR_CommentDumper)" );

  ufAssert( mCommentDumperMap.find( id ) == mCommentDumperMap.end() );

  mCommentDumperMap[ id ] = f;
};


/*
  getCommentDumper returns a pointer to a processor-specific WIR_Comment I/O
  function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getCommentDumper,
  the function pointer registered for the default ID 0 is returned.
*/
WIR_CommentDumper WIR_Registry::getCommentDumper( unsigned int id )
{
  DSTART(
    "static WIR_CommentDumper WIR_Registry::getCommentDumper(unsigned int)" );

  auto it = mCommentDumperMap.find( id );

  if ( it == mCommentDumperMap.end() )
    return( mCommentDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerCompilationUnitDumper registers a function pointer for a
  processor-specific WIR_CompilationUnit I/O function under the numerical ID of
  some processor architecture.

  registerCompilationUnitDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerCompilationUnitDumper( unsigned int id,
                                                  WIR_CompilationUnitDumper f )
{
  DSTART(
    "static void WIR_Registry::registerCompilationUnitDumper(unsigned int, WIR_CompilationUnitDumper)" );

  ufAssert(
    mCompilationUnitDumperMap.find( id ) == mCompilationUnitDumperMap.end() );

  mCompilationUnitDumperMap[ id ] = f;
};


/*
  getCompilationUnitDumper returns a pointer to a processor-specific
  WIR_CompilationUnit I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to
  getCompilationUnitDumper, the function pointer registered for the default ID 0
  is returned.
*/
WIR_CompilationUnitDumper WIR_Registry::getCompilationUnitDumper( unsigned int id )
{
  DSTART(
    "static WIR_CompilationUnitDumper WIR_Registry::getCompilationUnitDumper(unsigned int)" );

  auto it = mCompilationUnitDumperMap.find( id );

  if ( it == mCompilationUnitDumperMap.end() )
    return( mCompilationUnitDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerDataDumper registers a function pointer for a processor-specific
  WIR_Data I/O function under the numerical ID of some processor architecture.

  registerDataDumper shall be called by the init methods of class WIR_Processor
  in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerDataDumper( unsigned int id, WIR_DataDumper f )
{
  DSTART(
    "static void WIR_Registry::registerDataDumper(unsigned int, WIR_DataDumper)" );

  ufAssert( mDataDumperMap.find( id ) == mDataDumperMap.end() );

  mDataDumperMap[ id ] = f;
};


/*
  getDataDumper returns a pointer to a processor-specific WIR_Data I/O function
  for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getDataDumper, the
  function pointer registered for the default ID 0 is returned.
*/
WIR_DataDumper WIR_Registry::getDataDumper( unsigned int id )
{
  DSTART(
    "static WIR_DataDumper WIR_Registry::getDataDumper(unsigned int)" );

  auto it = mDataDumperMap.find( id );

  if ( it == mDataDumperMap.end() )
    return( mDataDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerDataSectionDumper registers a function pointer for a
  processor-specific I/O function for data sections under the numerical ID of
  some processor architecture.

  registerDataDumper shall be called by the init methods of class WIR_Processor
  in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerDataSectionDumper( unsigned int id,
                                              WIR_DataSectionDumper f )
{
  DSTART(
    "static void WIR_Registry::registerDataSectionDumper(unsigned int, WIR_DataSectionDumper)" );

  ufAssert( mDataSectionDumperMap.find( id ) == mDataSectionDumperMap.end() );

  mDataSectionDumperMap[ id ] = f;
};


/*
  getDataSectionDumper returns a pointer to a processor-specific data section
  I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getDataSectionDumper,
  the function pointer registered for the default ID 0 is returned.
*/
WIR_DataSectionDumper WIR_Registry::getDataSectionDumper( unsigned int id )
{
  DSTART(
    "static WIR_DataSectionDumper WIR_Registry::getDataSectionDumper(unsigned int)" );

  auto it = mDataSectionDumperMap.find( id );

  if ( it == mDataSectionDumperMap.end() )
    return( mDataSectionDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerFileInfoDumper registers a function pointer for a processor-specific
  WIR_FileInfo I/O function under the numerical ID of some processor
  architecture.

  registerFileInfoDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerFileInfoDumper( unsigned int id,
                                           WIR_FileInfoDumper f )
{
  DSTART(
    "static void WIR_Registry::registerFileInfoDumper(unsigned int, WIR_FileInfoDumper)" );

  ufAssert( mFileInfoDumperMap.find( id ) == mFileInfoDumperMap.end() );

  mFileInfoDumperMap[ id ] = f;
};


/*
  getFileInfoDumper returns a pointer to a processor-specific WIR_FileInfo I/O
  function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getFileInfoDumper, the
  function pointer registered for the default ID 0 is returned.
*/
WIR_FileInfoDumper WIR_Registry::getFileInfoDumper( unsigned int id )
{
  DSTART(
    "static WIR_FileInfoDumper WIR_Registry::getFileInfoDumper(unsigned int)" );

  auto it = mFileInfoDumperMap.find( id );

  if ( it == mFileInfoDumperMap.end() )
    return( mFileInfoDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerFunctionDumper registers a function pointer for a processor-specific
  WIR_Function I/O function under the numerical ID of some processor
  architecture.

  registerFunctionDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerFunctionDumper( unsigned int id,
                                           WIR_FunctionDumper f )
{
  DSTART(
    "static void WIR_Registry::registerFunctionDumper(unsigned int, WIR_FunctionDumper)" );

  ufAssert( mFunctionDumperMap.find( id ) == mFunctionDumperMap.end() );

  mFunctionDumperMap[ id ] = f;
};


/*
  getFunctionDumper returns a pointer to a processor-specific WIR_Function I/O
  function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getFunctionDumper, the
  function pointer registered for the default ID 0 is returned.
*/
WIR_FunctionDumper WIR_Registry::getFunctionDumper( unsigned int id )
{
  DSTART(
    "static WIR_FunctionDumper WIR_Registry::getFunctionDumper(unsigned int)" );

  auto it = mFunctionDumperMap.find( id );

  if ( it == mFunctionDumperMap.end() )
    return( mFunctionDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerInstructionDumper registers a function pointer for a
  processor-specific WIR_Instruction I/O function under the numerical ID of some
  processor architecture.

  registerInstructionDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerInstructionDumper( unsigned int id,
                                              WIR_InstructionDumper f )
{
  DSTART(
    "static void WIR_Registry::registerInstructionDumper(unsigned int, WIR_InstructionDumper)" );

  ufAssert( mInstructionDumperMap.find( id ) == mInstructionDumperMap.end() );

  mInstructionDumperMap[ id ] = f;
};


/*
  getInstructionDumper returns a pointer to a processor-specific WIR_Instruction
  I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getInstructionDumper,
  the function pointer registered for the default ID 0 is returned.
*/
WIR_InstructionDumper WIR_Registry::getInstructionDumper( unsigned int id )
{
  DSTART(
    "static WIR_InstructionDumper WIR_Registry::getInstructionDumper(unsigned int)" );

  auto it = mInstructionDumperMap.find( id );

  if ( it == mInstructionDumperMap.end() )
    return( mInstructionDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerOperationDumper registers a function pointer for a processor-specific
  WIR_Operation I/O function under the numerical ID of some processor
  architecture.

  registerOperationDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerOperationDumper( unsigned int id,
                                            WIR_OperationDumper f )
{
  DSTART(
    "static void WIR_Registry::registerOperationDumper(unsigned int, WIR_OperationDumper)" );

  ufAssert( mOperationDumperMap.find( id ) == mOperationDumperMap.end() );

  mOperationDumperMap[ id ] = f;
};


/*
  getOperationDumper returns a pointer to a processor-specific WIR_Operation I/O
  function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getOperationDumper, the
  function pointer registered for the default ID 0 is returned.
*/
WIR_OperationDumper WIR_Registry::getOperationDumper( unsigned int id )
{
  DSTART(
    "static WIR_OperationDumper WIR_Registry::getOperationDumper(unsigned int)" );

  auto it = mOperationDumperMap.find( id );

  if ( it == mOperationDumperMap.end() )
    return( mOperationDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerAddressingModeParameterDumper registers a function pointer for a
  processor-specific WIR_AddressingModeParameter I/O function under the
  numerical ID of some processor architecture.

  registerAddressingModeParameterDumper shall be called by the init methods of
  class WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerAddressingModeParameterDumper( unsigned int id,
                                                          WIR_AddressingModeParameterDumper f )
{
  DSTART(
    "static void WIR_Registry::registerAddressingModeParameterDumper(unsigned int, WIR_AddressingModeParameterDumper)" );

  ufAssert(
    mAddressingModeParameterDumperMap.find( id ) ==
      mAddressingModeParameterDumperMap.end() );

  mAddressingModeParameterDumperMap[ id ] = f;
};


/*
  getAddressingModeParameterDumper returns a pointer to a processor-specific
  WIR_AddressingModeParameter I/O function for the given processor architecture
  ID.

  If some unknown/unregistered processor ID is passed to
  getAddressingModeParameterDumper, the function pointer registered for the
  default ID 0 is returned.
*/
WIR_AddressingModeParameterDumper WIR_Registry::getAddressingModeParameterDumper( unsigned int id )
{
  DSTART(
    "static WIR_AddressingModeParameterDumper WIR_Registry::getAddressingModeParameterDumper(unsigned int)" );

  auto it = mAddressingModeParameterDumperMap.find( id );

  if ( it == mAddressingModeParameterDumperMap.end() )
    return( mAddressingModeParameterDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerConditionFieldParameterDumper registers a function pointer for a
  processor-specific WIR_ConditionFieldParameter I/O function under the
  numerical ID of some processor architecture.

  registerConditionFieldParameterDumper shall be called by the init methods of
  class WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerConditionFieldParameterDumper( unsigned int id,
                                                          WIR_ConditionFieldParameterDumper f )
{
  DSTART(
    "static void WIR_Registry::registerConditionFieldParameterDumper(unsigned int, WIR_ConditionFieldParameterDumper)" );

  ufAssert(
    mConditionFieldParameterDumperMap.find( id ) ==
      mConditionFieldParameterDumperMap.end() );

  mConditionFieldParameterDumperMap[ id ] = f;
};


/*
  getConditionFieldParameterDumper returns a pointer to a processor-specific
  WIR_ConditionFieldParameter I/O function for the given processor architecture
  ID.

  If some unknown/unregistered processor ID is passed to
  getConditionFieldParameterDumper, the function pointer registered for the
  default ID 0 is returned.
*/
WIR_ConditionFieldParameterDumper WIR_Registry::getConditionFieldParameterDumper( unsigned int id )
{
  DSTART(
    "static WIR_ConditionFieldParameterDumper WIR_Registry::getConditionFieldParameterDumper(unsigned int)" );

  auto it = mConditionFieldParameterDumperMap.find( id );

  if ( it == mConditionFieldParameterDumperMap.end() )
    return( mConditionFieldParameterDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerImmediateParameterDumper registers a function pointer for a
  processor-specific WIR_BaseImmediateParameter I/O function under the numerical
  ID of some processor architecture.

  registerImmediateParameterDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerImmediateParameterDumper( unsigned int id,
                                                     WIR_ImmediateParameterDumper f )
{
  DSTART(
    "static void WIR_Registry::registerImmediateParameterDumper(unsigned int, WIR_ImmediateParameterDumper)" );

  ufAssert(
    mImmediateParameterDumperMap.find( id ) ==
      mImmediateParameterDumperMap.end() );

  mImmediateParameterDumperMap[ id ] = f;
};


/*
  getImmediateParameterDumper returns a pointer to a processor-specific
  WIR_BaseImmediateParameter I/O function for the given processor architecture
  ID.

  If some unknown/unregistered processor ID is passed to
  getImmediateParameterDumper, the function pointer registered for the default
  ID 0 is returned.
*/
WIR_ImmediateParameterDumper WIR_Registry::getImmediateParameterDumper( unsigned int id )
{
  DSTART(
    "static WIR_ImmediateParameterDumper WIR_Registry::getImmediateParameterDumper(unsigned int)" );

  auto it = mImmediateParameterDumperMap.find( id );

  if ( it == mImmediateParameterDumperMap.end() )
    return( mImmediateParameterDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerLabelParameterDumper registers a function pointer for a
  processor-specific WIR_LabelParameter I/O function under the numerical ID of
  some processor architecture.

  registerLabelParameterDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerLabelParameterDumper( unsigned int id,
                                                 WIR_LabelParameterDumper f )
{
  DSTART(
    "static void WIR_Registry::registerLabelParameterDumper(unsigned int, WIR_LabelParameterDumper)" );

  ufAssert(
    mLabelParameterDumperMap.find( id ) == mLabelParameterDumperMap.end() );

  mLabelParameterDumperMap[ id ] = f;
};


/*
  getLabelParameterDumper returns a pointer to a processor-specific
  WIR_LabelParameter I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to
  getLabelParameterDumper, the function pointer registered for the default ID
  0 is returned.
*/
WIR_LabelParameterDumper WIR_Registry::getLabelParameterDumper( unsigned int id )
{
  DSTART(
    "static WIR_LabelParameterDumper WIR_Registry::getLabelParameterDumper(unsigned int)" );

  auto it = mLabelParameterDumperMap.find( id );

  if ( it == mLabelParameterDumperMap.end() )
    return( mLabelParameterDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerLdScriptDumper registers a function pointer for a processor-specific
  linker script I/O function under the numerical ID of some processor
  architecture.

  registerLdScriptDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerLdScriptDumper( unsigned int id,
                                           WIR_LdScriptDumper f )
{
  DSTART(
    "static void WIR_Registry::registerLdScriptDumper(unsigned int, WIR_LdScriptDumper)" );

  ufAssert( mLdScriptDumperMap.find( id ) == mLdScriptDumperMap.end() );

  mLdScriptDumperMap[ id ] = f;
};


/*!
  @brief getLdScriptDumper returns a pointer to a processor-specific linker
          script I/O function for the given processor architecture ID.

  @param[in] id A processor architecture's unique numerical identifier.
  @return A pointer to a processor-specific I/O function for linker scripts.

  If some unknown/unregistered processor ID is passed to getLdScriptDumper,
  the function pointer registered for the default ID 0 is returned.
*/
WIR_LdScriptDumper WIR_Registry::getLdScriptDumper( unsigned int id )
{
  DSTART(
    "static WIR_LdScriptDumper WIR_Registry::getLdScriptDumper(unsigned int)" );

  auto it = mLdScriptDumperMap.find( id );

  if ( it == mLdScriptDumperMap.end() )
    return( mLdScriptDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerLdScriptSectionDumper registers a function pointer for a
  processor-specific section linker script I/O function under the numerical ID
  of some processor architecture.

  registerLdScriptSectionDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerLdScriptSectionDumper( unsigned int id,
                                                  WIR_LdScriptSectionDumper f )
{
  DSTART(
    "static void WIR_Registry::registerLdScriptSectionDumper(unsigned int, WIR_LdScriptSectionDumper)" );

  ufAssert(
    mLdScriptSectionDumperMap.find( id ) == mLdScriptSectionDumperMap.end() );

  mLdScriptSectionDumperMap[ id ] = f;
};


/*
  getLdScriptSectionDumper returns a pointer to a processor-specific section
  linker script I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to
  getLdScriptSectionDumper, the function pointer registered for the default ID 0
  is returned.
*/
WIR_LdScriptSectionDumper WIR_Registry::getLdScriptSectionDumper( unsigned int id )
{
  DSTART(
    "static WIR_LdScriptSectionDumper WIR_Registry::getLdScriptSectionDumper(unsigned int)" );

  auto it = mLdScriptSectionDumperMap.find( id );

  if ( it == mLdScriptSectionDumperMap.end() )
    return( mLdScriptSectionDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerRegisterParameterDumper registers a function pointer for a
  processor-specific WIR_RegisterParameter I/O function under the numerical ID
  of some processor architecture.

  registerRegisterParameterDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerRegisterParameterDumper( unsigned int id,
                                                    WIR_RegisterParameterDumper f )
{
  DSTART(
    "static void WIR_Registry::registerRegisterParameterDumper(unsigned int, WIR_RegisterParameterDumper)" );

  ufAssert(
    mRegisterParameterDumperMap.find( id ) ==
      mRegisterParameterDumperMap.end() );

  mRegisterParameterDumperMap[ id ] = f;
};


/*
  getRegisterParameterDumper returns a pointer to a processor-specific
  WIR_RegisterParameter I/O function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to
  getRegisterParameterDumper, the function pointer registered for the default ID
  0 is returned.
*/
WIR_RegisterParameterDumper WIR_Registry::getRegisterParameterDumper( unsigned int id )
{
  DSTART(
    "static WIR_RegisterParameterDumper WIR_Registry::getRegisterParameterDumper(unsigned int)" );

  auto it = mRegisterParameterDumperMap.find( id );

  if ( it == mRegisterParameterDumperMap.end() )
    return( mRegisterParameterDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  registerSystemDumper registers a function pointer for a processor-specific
  WIR_System I/O function under the numerical ID of some processor architecture.

  registerSystemDumper shall be called by the init methods of class
  WIR_Processor in order to setup processor-specific I/O routines.
*/
void WIR_Registry::registerSystemDumper( unsigned int id,
                                         WIR_SystemDumper f )
{
  DSTART(
    "static void WIR_Registry::registerSystemDumper(unsigned int, WIR_SystemDumper)" );

  ufAssert( mSystemDumperMap.find( id ) == mSystemDumperMap.end() );

  mSystemDumperMap[ id ] = f;
};


/*
  getSystemDumper returns a pointer to a processor-specific WIR_System I/O
  function for the given processor architecture ID.

  If some unknown/unregistered processor ID is passed to getSystemDumper, the
  function pointer registered for the default ID 0 is returned.
*/
WIR_SystemDumper WIR_Registry::getSystemDumper( unsigned int id )
{
  DSTART(
    "static WIR_SystemDumper WIR_Registry::getSystemDumper(unsigned int)" );

  auto it = mSystemDumperMap.find( id );

  if ( it == mSystemDumperMap.end() )
    return( mSystemDumperMap[ 0 ] );
  else
    return( (*it).second );
};


/*
  isLegalOperationFormat returns whether the given operation format is legal for
  the specified opcode.

  If the given opcode has not been registered before, isLegalOperationFormat
  fails with an assertion.
*/
bool WIR_Registry::isLegalOperationFormat( const WIR_BaseProcessor::OpCode &o,
                                           const WIR_BaseProcessor::OperationFormat &f )
{
  DSTART(
    "static bool WIR_Registry::isLegalOperationFormat(const WIR_BaseProcessor::OpCode&, const WIR_BaseProcessor::OperationFormat&)" );

  ufAssert( mOpCodeMap.find( o.mID ) != mOpCodeMap.end() );
  return( mOpCodeMap[ o.mID ].count( f.mID ) == 1 );
};


//
// Private class methods
//

// Destructor taking care of the registers' deletion.
WIR_Registry::operationFormatRegisters::~operationFormatRegisters( void )
{
  DSTART(
    "WIR_Registry::operationFormatRegisters::~operationFormatRegisters()" );

  for ( WIR_BaseRegister *p : mRegisters )
    delete( p );
};


/*
  isProcessorRegistered checks whether a processor model with the given ISA name
  has been registered using registerProcessor before.
*/
bool WIR_Registry::isProcessorRegistered( const std::string &n )
{
  DSTART( "static bool WIR_Registry::isProcessorRegistered(const string&)" );

  return( mProcessorMap.find( n ) != mProcessorMap.end() );
};


/*
  getNewProcessor looks up a processor model with the given ISA name in map
  mProcessorMap and generates a new instance of this processor.

  If the given ISA name has not yet been registered before using
  registerProcessor, getNewProcessor fails with an assertion.
*/
WIR_BaseProcessor *WIR_Registry::getNewProcessor( const std::string &n )
{
  DSTART(
    "static WIR_BaseProcessor* WIR_Registry::getNewProcessor(const string&)" );

  ufAssert( mProcessorMap.find( n ) != mProcessorMap.end() );
  return( mProcessorMap[ n ]->clone() );
};


/*
  getOperationFormat looks up a machine operation format from map
  mOperationFormatMap.

  If the given ID does not refer to a machine operation format or does not exist
  at all, getOperationFormat fails with an assertion.
*/
const WIR_OperationFormat &WIR_Registry::getOperationFormat( const WIR_BaseProcessor::OperationFormat &id )
{
  DSTART(
    "static const WIR_OperationFormat& WIR_Registry::getOperationFormat(const WIR_BaseProcessor::OperationFormat&)" );

  auto it = mOperationFormatMap.find( id.mID );
  ufAssert( it != mOperationFormatMap.end() );
  return( it->second );
};

}       // namespace WIR
