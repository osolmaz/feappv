! Id: plstop.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine plstop()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Close any open plot windows and stop execution

  !      Inputs:
  !         none

  !      Outputs:
  !         none
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  use       DFLIB

  implicit  none

  include  'pdatps.h'
  include  'plflag.h'

  integer ::   status,vclrwk,vclswk

  save

  !     Close PostScript file if open

  if (hdcpy) call fpplcl()

  if(everon) then
     status = vclrwk()
     status = vclswk()
  endif

  !     Clear last time history plot

  call ptimpl()

  status = setexitqq(QWIN$EXITNOPERSIST)


  stop

end subroutine plstop
