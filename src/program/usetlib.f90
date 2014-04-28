! Id:$
subroutine usetlib(i)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Interface for user mesh manipulation set  commands

  !      Inputs:
  !         i      - Command number

  !      Outputs:
  !         None   - Users are responsible for providing outputs in
  !                  umanii routines
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  integer ::   i

  save

  if(i == 1) then
    call umani1
  elseif(i == 2) then
    call umani2
  elseif(i == 3) then
    call umani3
  elseif(i == 4) then
    call umani4
  elseif(i == 5) then
    call umani5
  endif

end subroutine usetlib
