#
#
#  This source file belongs to the
#
#           Hamburg University of Technology (TUHH)
#             WCC Compiler Framework
#
#  and is property of its respective copyright holder. It must neither be used
#  nor published even in parts without explicit written permission.
#
#  Copyright 2010 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# A Statement.
%declare<void> stmt;

# An address register which contains pointers, function pointers or array bases.
%declare<ARegPair><{ nullptr, ref( dummyARegV ) }> areg<void>;

# A data register which may contain 'char', 'short', 'int' or 'float' or a
# bitfield that was extracted from a struct.
%declare<DRegPair><{ nullptr, ref( dummyDRegV ) }> dreg<void>;

# An extended register that contains a double.
%declare<ERegPair><{ nullptr, ref( dummyERegV ) }> ereg<void>;

# A data register which may contain 'char', 'short', 'int' or 'float' together
# with the memory location from which it was read. This should only be used by
# LHS sides of assignments which must write back the result to memory. Parameter
# loadResult indicates whether the result should really be loaded from memory
# (true) or whether just the address should be computed (false).
%declare<TC_LValue> deref_dreg<bool loadResult>;

# An address register which contains pointers, function pointers or array bases
# together with the memory location from which it was read. This should only be
# used by LHS sides of assignments which must write back the result to memory.
# Parameter loadResult indicates whether the result should really be loaded into
# memory (true) or whether just the address should be computed (false).
%declare<TC_LValue> deref_areg<bool loadResult>;

# An extended register that contains a double together with the memory location
# from which it was read. This should only be used by LHS sides of assignments
# which must write back the result to memory. Parameter loadResult indicates
# whether the result should really be loaded into memory (true) or whether just
# the address should be computed (false).
%declare<TC_LValue> deref_ereg<bool loadResult>;

# Represents the called function:
# - lhsRegs are the LLIR registers that store the arguments that are passed by
#   register.
# - args are the WIR registers that store the arguments that are passed by
#   register.
# - returnBehaviour specifies what kind of result is returned.
# Returned is the pair of LLIR/WIR registers that contains the result
# (or nullptr).
%declare<RegisterPair><{ nullptr, nullptr }> called_function<regptrList &lhsRegs,
                                                             const argList &args,
                                                             enum RegType returnBehaviour>;

# A special nonterminal for handling function call arguments.
%declare<void> arg<bool dryrun, int index, int offset, IR_CallExp *theCall,
                   regptrList *lhsRegs, regptrList *rhsRegs, argList &args,
                   argList &dryArgs>;

# A modified address register, always an rvalue.
# This nonterminal introduces a "delay" in the evaluation of pointer
# arithmetics. This allows for better performance, because the pointer
# arithmetics can be performed as part of a LD/ST instructions, if applicable.
%declare<TC_AddressModification> modified_areg<void>;

# The C standard dictates that the unary dereference operator immediately
# followed by the address operator shall not emit any code, i.e. &*e and e
# shall emit the same code. Note, however, that e could be an lvalue, whereas
# &*e is never an lvalue.
# This is realized with the zero_op_* nonterminals, for both the
# AdressModification and the Register* that could represent an address.
%declare<ARegPair><{ nullptr, ref( dummyARegV ) }> zero_op_areg<void>;
%declare<TC_AddressModification> zero_op_modified_areg<void>;

# This is only generated for constant addresses in memory.
%declare<IR_Integer> constAddress;

# Non-terminals for constants that may appear as immediates in instructions.
%declare<IR_Integer> addrOffset;

# A signed 4-bit constant.
%declare<IR_Integer> const4;

# An unsigned 4-bit constant.
%declare<IR_Integer> uconst4;

# A signed 9-bit constant.
%declare<IR_Integer> const9;

# An unsigned 9-bit constant.
%declare<IR_Integer> uconst9;

# A signed 16-bit constant.
%declare<IR_Integer> const16;

# The constant value 0.
%declare<IR_Integer> constant0;

# The constant value 8.
%declare<IR_Integer> constant8;

# The constant value 256.
%declare<IR_Integer> constant256;

# A constant that is a power of 2.
%declare<IR_Integer> powerOfTwo;

# A constant that is a (power of 2) * -1.
%declare<IR_Integer> negPowerOfTwo;

# A constant in the range [2^32 - 2^8, 2^32 - 1] (negated const9).
%declare<IR_Integer> const9_neg;

# constantf represents 32bit Float.
%declare<Float> constantf;

# constantd represents 64bit Double.
%declare<Double> constantd;
