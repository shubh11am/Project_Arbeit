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
#  Copyright 2009 - 2022
#
#  Hamburg University of Technology (TUHH)
#  Institute of Embedded Systems
#  21071 Hamburg
#  Germany
#
#  http://www.tuhh.de/es/esd/research/wcc
#
#


# Represents a register that still must be casted to its target type.
%declare<WIR::RV_RegV &><RV32::dummyRegV> implicit_castable_reg;

# Represents a register that still must be casted to its target type
%declare<WIR::RV_RegV &><RV32::dummyRegV> explicit_castable_reg;