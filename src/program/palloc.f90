! Id:$
logical function palloc(num,name,length,precis)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Define, delete, or resize a dictionary entry.
  !               Pointer defined for integer (single) and real
  !               (double precision arrays.

  !      Inputs:
  !         num        - Entry number for array (see below)          (int*4)
  !         name       - Name of array          (see below)          (char*
  !         length     - Length of array defined: =0 for delete      (int*8)
  !         precis     - Precision of array: 1 = integers; 2 = reals (int*4)

  !      Outputs:
  !         np(num)    - Pointer to first word of array in blank common
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  integer ::    list
  parameter (list = 91)

  include  'allotd.h'
  include  'allotn.h'
  include  'pointer.h'

  logical ::   ualloc
  character name*(*)
  integer ::   i, num,precis
  integer ::   length

  save

  !     Set active arrays for FEAPpv into list

  data   (nlist(i),i=1,list)/ &
       
       'TANGS', 'OINUC', 'OINUO', 'OINUP', 'UTANG', 'OINLC', &
       'OINLO', 'OINLP', 'CMASS', 'OINMC', 'OINMO', 'OINMP', &
       
       'LMASS', 'OINDC', 'OINDO', 'OINDP', 'DAMPS', 'OINCC', &
       'OINCO', 'OINCP', 'JPROF', 'PROP0', 'PROP1', 'PROP2', &
       
                                !     Solution arrays
       
                                !              'TANGS',     !     1: Symmetric tangent
                                !              'OINUC',     !     2: Compact tangent end pointers
                                !              'OINUO',     !     3: Compact tangent entries/equation
                                !              'OINUP',     !     4: Compact tangent pointer
       
                                !              'UTANG',     !     5: Unsymmetric tangent
                                !              'OINLC',     !     6: Compact tangent end pointers
                                !              'OINLO',     !     7: Compact tangent entries/equation
                                !              'OINLP',     !     8: Compact tangent pointer
       
                                !              'CMASS',     !     9: Consistent mass
                                !              'OINMC',     !    10: Compact upper mass end pointers
                                !              'OINMO',     !    11: Compact upper mass entries/equation
                                !              'OINMP',     !    12: Compact upper mass pointer
       
                                !              'LMASS',     !    13: Diagonal mass
                                !              'OINDC',     !    14: Compact lower mass end pointers
                                !              'OINDO',     !    15: Compact lower mass entries/equation
                                !              'OINDP',     !    16: Compact lower mass pointer
       
                                !              'DAMPS',     !    17: Symmetric damping
                                !              'OINCC',     !    18: Compact damping end pointers
                                !              'OINCO',     !    19: Compact damping entries/equation
                                !              'OINCP',     !    20: Compact damping pointer
       
                                !              'JPROF',     !    21: Profile pointer
                                !              'PROP0',     !    22: Prop. load offset table
                                !              'PROP1',     !    23: Prop. load values table
                                !              'PROP2',     !    24: Prop. load temporary use
       
                                !     Mesh arrays
       
       'D    ', 'DR   ', 'F    ', 'F0   ', 'FPRO ', 'FTN  ', &
       'ID   ', 'IE   ', 'IX   ', 'LD   ', 'P    ', 'S    ', &
       'NORMV', 'T    ', 'TL   ', 'U    ', 'UL   ', 'VEL  ', &
       'X    ', 'XL   ', 'ANG  ', 'ANGL ', 'NREN ', 'IPOS ', &
       'NDTYP', &
       
                                !              'D    ',     !    25: Material parameters
                                !              'DR   ',     !    26: Residual/reactions
       
                                !              'F    ',     !    27: Nodal load/displacement, current
                                !              'F0   ',     !    28: Nodal load/displace pattern, base
                                !              'FPRO ',     !    29: DOF proportional load numbers
                                !              'FTN  ',     !    30: Nodal load/displacement, current
       
                                !              'ID   ',     !    31: Equation numbers/boundary conds
                                !              'IE   ',     !    32: Element assembly information
                                !              'IX   ',     !    33: Element connection data
       
                                !              'LD   ',     !    34: Element local/global eq numbers
       
                                !              'P    ',     !    35: Element vector
       
                                !              'S    ',     !    36: Element array
                                !              'NORMV'      !    37: Normal vector (shell use)
       
                                !              'T    ',     !    38: Nodal temperatures
                                !              'TL   ',     !    39: Nodal temperaturese, element
       
                                !              'U    ',     !    40: Nodal solutions/increments
                                !              'UL   ',     !    41: Nodal solutions/increments,element
       
                                !              'VEL'  ,     !    42: Nodal transient solution values
       
                                !              'X    ',     !    43: Nodal coordinates
                                !              'XL   ',     !    44: Nodal coordinates, element
       
                                !              'ANG  ',     !    45: Nodal angles
                                !              'ANGL ',     !    46: Nodal angles, element
                                !              'NREN ',     !    47: Node renumber list
                                !              'IPOS ',     !    48: Tie node list
       
                                !              'NDTYP',     !    49: Node coordinate type
       
                                !     History data
       
       'H    ', 'NH1  ', 'NH2  ', 'NH3  ', &
       
                                !              'H    ',     !    50: Element history parameters
                                !              'NH1  ',     !    51: Element history data at t_n
                                !              'NH2  ',     !    52: Element history data at t_n+1
                                !              'NH3  ',     !    53: Element history data, time independ.
       
                                !     Plot data
       
       'CT   ', 'FCIX ', 'FCZM ', 'NDER ', 'NDNP ', 'VISN ', &
       'NSCR ', 'OUTL ', 'SYMM ', &
       
       
                                !              'CT   ',     !    54: Plot deformed coordinate storage.
                                !              'FCIX ',     !    55: Face nodal connection list.
                                !              'FCZM ',     !    56: Face z-sort coordinates.
                                !              'NDER ',     !    57: Error estimator projections.
                                !              'NDNP ',     !    58: Stress projection array.
                                !              'VISN ',     !    59: Visible face list
                                !              'NSCR ',     !    60: TRI2D size projection data.
                                !              'OUTL ',     !    61: Outline construction.
                                !              'SYMM ',     !    62: Symmetric reflections table.
       
                                !     Blending arrays
       
       'BNODE', 'BSIDE', 'BTRAN', 'BLEND', 'BFACE', 'BNILR', &
       
                                !              'BNODE',     !    63:  Super nodes for blending functions
                                !              'BSIDE',     !    64:  Sides for blending functions
                                !              'BTRAN',     !    65:  Transformations for blends
                                !              'BLEND',     !    66:  Blending function storage
                                !              'BFACE',     !    65:  Blending function storage
                                !              'BNILR',     !    66:  Blending layer storage
       
                                !     Solver support data
       
       'BFGD ', 'BFGO ', 'BFGS ', 'BFGT ', 'BFGV ', 'BFGW ', &
       'EIGE ', 'EVAL ', 'EVEC ', 'EXTND', 'MU1  ', 'MU2  ', &
       
                                !              'BFGD ',     !    69: BFGS working vector
                                !              'BFGO ',     !    70: BFGS working vector
                                !              'BFGS ',     !    71: BFGS working vector
                                !              'BFGT ',     !    72: BFGS working vector
                                !              'BFGV ',     !    73: BFGS vectors, U
                                !              'BFGW ',     !    74: BFGS vectors, W
       
                                !              'EIGE ',     !    75: Element eigenpairs
                                !              'EVAL ',     !    76: Subspace eigenvalues
                                !              'EVEC ',     !    77: Subspace eigenvectors
                                !              'EXTND',     !    78: External nodes
       
                                !              'MU1  ',     !    79:
                                !              'MU2  ',     !    80:
       
                                !     Temporay arrays
       
       'TEMP1', 'TEMP2', 'TEMP3', 'TEMP4', 'TEMP5', 'TEMP6', &
       'TEMP7', 'TEMP8', 'TEMP9', 'TEMP0', &
       
                                !              'TEMP1',     !    81:  Temporary array
                                !              'TEMP2',     !    82:  Temporary array
                                !              'TEMP3',     !    83:  Temporary array
                                !              'TEMP4',     !    84:  Temporary array
                                !              'TEMP5',     !    85:  Temporary array
                                !              'TEMP6',     !    86:  Temporary array
                                !              'TEMP7',     !    87:  Temporary array
                                !              'TEMP8',     !    88:  Temporary array
                                !              'TEMP9',     !    89:  Temporary array
                                !              'TEMP0',     !    90:  Temporary array
       
                                !     Follower nodal loads
       
       'APLOT'/

  !              'APLOT',     !    91:  Tag for active plot elements

  !     Set memory pointers in blank common

  if(num == 0) then

     do i = 1,list
        np(i)      = 0
        ilist(1,i) = 0
        ilist(2,i) = 0
     end do
     llist  =  list

  endif

  !     Check user allocations then do allocation operation

  palloc = ualloc(num-llist,name,length,precis)

end function palloc
