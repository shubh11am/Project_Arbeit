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
#  Copyright 2005 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# A Statement
%declare<void> stmt;

# A data register which may contain 'char', 'short', 'int' or 'float' or a
# bitfield that was extracted from a struct.
%declare<LLIR_Register *><nullptr> dreg<void>;

# An extended register that contains a value of unsigned long long integer,
# signed long long integer or double type. The ARM7 does not natively support
# extended registers, we simply use them for better organization.
%declare<LLIR_Register*><nullptr> ereg<void>;

# A data register which may contain 'char', 'short', 'int' or 'float' together
# with the memory location from which it was read. This should only be used by
# LHS sides of assignments which must write back the result to memory. The
# first parameter indicates whether the result should really be loaded into
# memory (true) or whether just the address should be computed (false). The
# second parameter indicates whether only a 'dry run' of the action part is to
# be performed. The returned LValue can then be used for cost calculations.
%declare<ARM_LValue> deref_dreg<bool loadResult, bool dryRun>;

# The same as above, but for 64 bit types.
%declare<ARM_LValue> deref_ereg<bool loadResult, bool dryRun>;

# Represents the called function:
# - target is the register where the rules should write the result to. If target
#   is the empty string, then the result is not copied anywhere.
# - lhsRegs are the registers that store the arguments that are passed by register
# - returnBehaviour specifies what kind of result is returned
# Returned is the register that contains the result (this is target if != "")
%declare<LLIR_Register *><nullptr> called_function<regptr_list &lhsRegs,
                                                   enum RegType returnBehaviour>;

# A special nonterminal for handling function call arguments
%declare<void> arg<bool dryrun, int index, int offset, IR_CallExp *theCall,
                   RegisterQueue *lhsRegs, RegisterQueue *rhsRegs>;

# This is only generated for constant addresses in memory.
%declare<IR_Integer> constAddress;

# Non-terminals for constants that may appear as immediates in instructions.
%declare<IR_Integer> addrOffset;

# A signed 4-bit constant
%declare<IR_Integer> const4;
# An unsigned 4-bit constant
%declare<IR_Integer> uconst4;
# A signed 8-bit constant
%declare<IR_Integer> const8;
# An unsigned 8-bit constant
%declare<IR_Integer> uconst8;
# A signed 16-bit constant
%declare<IR_Integer> const16;

# The constant value 0
%declare<IR_Integer> constant0;
# The constant value 8
%declare<IR_Integer> constant8;
# The constant value 256
%declare<IR_Integer> constant256;

# A constant that is a power of 2
%declare<IR_Integer> powerOfTwo;
# A constant that is a (power of 2) * -1
%declare<IR_Integer> negPowerOfTwo;

# A constant in the range [2^32 - 2^8, 2^32 - 1] (negated const9)
%declare<IR_Integer> const9_neg;

# constantf represents 32bit Float constant
%declare<Float> constantf;
# constantd represents 64bit Double constant
%declare<Double> constantd;
# constantll represents 64bit Long long integer constant
%declare<IR_Integer> constantll;

# For conditional jumps with a positive condition.
%declare<IR_Type *><nullptr> rel;
