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
#  Copyright 2021 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# Non-terminal 'rel' denotes a Boolean relation to be used in conditional
# statements like, e.g., if-then statements, loops or selection operators.
# @param[in] b A const reference to a basic block being target of a conditional
#              jump that depends on the Boolean relation.
# @param[in] markLoopExit A Boolean flag denoting whether conditional jumps
#                         branching out of a loop shall be marked as such, in
#                         order to provide hints for subsequent control flow
#                         analyses.
%declare<void> rel<const WIR::WIR_BasicBlock &b, bool markLoopExit>;

# Non-terminal 'rel' denotes a negated Boolean relation to be used in
# conditional statements like, e.g., if-then statements, loops or selection
# operators.
# @param[in] b A const reference to a basic block being target of a conditional
#              jump that depends on the negated Boolean relation.
# @param[in] markLoopExit A Boolean flag denoting whether conditional jumps
#                         branching out of a loop shall be marked as such, in
#                         order to provide hints for subsequent control flow
#                         analyses.
%declare<void> nrel<const WIR::WIR_BasicBlock &b, bool markLoopExit>;
