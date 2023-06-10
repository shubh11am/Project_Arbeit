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


# For conditional jumps with a positive condition.
%declare<void> rel<const string &block, const WIR::WIR_BasicBlock &b,
                   bool markLoopExit>;

# For conditional jumps with a negated condition.
%declare<void> nrel<const string &block, const WIR::WIR_BasicBlock &b,
                    bool markLoopExit>;
