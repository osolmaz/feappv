! Id: plclos.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine plclos()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Close plot device

  !      Inputs:
  !         none

  !      Outputs:
  !         none      - Returns command outputs to text device
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  implicit  none

  include  'print.h'

  integer ::   status,vtxwin

  save

  !     Close plot device

  fopn = .false. 

  status = vtxwin()

end subroutine plclos
