! Id:$
subroutine umshlib(i,tx,prt)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Interface for user mesh commands

  !      Inputs:
  !         i      - Command number
  !         tx(*)  - Commnand line input data
  !         prt    - Flag, output if true

  !      Outputs:
  !         None   - Users are responsible for providing outputs in
  !                  umeshi routines
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  logical ::   prt
  character tx(*)*15
  integer ::   i

  save

  if(i == 1) then
     call umesh1(tx,prt)
  elseif(i == 2) then
     call umesh2(tx,prt)
  elseif(i == 3) then
     call umesh3(tx,prt)
  elseif(i == 4) then
     call umesh4(tx,prt)
  elseif(i == 5) then
     call umesh5(tx,prt)
  endif

end subroutine umshlib
