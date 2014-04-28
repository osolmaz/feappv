! Id:$
subroutine upltlib(i,ct)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Interface for user plot commands

  !      Inputs:
  !         i      - Command number
  !         ct(3)  - Parameters from input line

  !      Outputs:
  !         None   - Users are responsible for providing outputs in
  !                  uploti routines
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  integer ::   i
  real*8 ::    ct(3)

  save

  if(i == 1) then
     call uplot1(ct)
  elseif(i == 2) then
     call uplot2(ct)
  elseif(i == 3) then
     call uplot3(ct)
  elseif(i == 4) then
     call uplot4(ct)
  elseif(i == 5) then
     call uplot5(ct)
  endif

end subroutine upltlib
