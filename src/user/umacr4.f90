! Id:$
subroutine umacr4(lct,ctl)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose:  User interface for adding solution command language
  !                instructions.

  !      Inputs:
  !         lct       - Command character parameters
  !         ctl(3)    - Command numerical parameters

  !      Outputs:
  !         N.B.  Users are responsible for command actions.  See
  !               programmers manual for example.
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'umac1.h'

  logical ::   pcomp
  character lct*15
  real*8 ::    ctl(3)

  save

  !     Set command word

  if( .not. pcomp(uct,'mac4',4)) then
     !       uct = 'name'                    ! Specify 'name'
  elseif(urest == 1) then           ! Read  restart data

  elseif(urest == 2) then           ! Write restart data

  else                              ! Perform user operation

  endif

end subroutine umacr4
