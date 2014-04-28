! Id:$
subroutine umaclib(i,lct,ct)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Interface for user command language instructions

  !      Inputs:
  !         i      - Command number
  !         lct    - Character array describing option
  !         ct(3)  - Command parameters

  !      Outputs:
  !         N.B.  Users are responsible for generating command options
  !               See programmer manual for example.
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  character lct*(*)
  integer ::   i
  real*8 ::    ct(3)

  save

  if(    i == 1) then
    call umacr1(lct,ct)
  elseif(i == 2) then
    call umacr2(lct,ct)
  elseif(i == 3) then
    call umacr3(lct,ct)
  elseif(i == 4) then
    call umacr4(lct,ct)
  elseif(i == 5) then
    call umacr5(lct,ct)
  endif

end subroutine umaclib
