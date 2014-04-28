! Id: plstop.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
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

  implicit  none

  include  'pdata2.h'
  include  'pdatps.h'
  include  'plflag.h'
  include  'x11f.h'

  save

  !     Close PostScript file if open

  if (hdcpy) call fpplcl()

  !     X11 device

  if(everon) call gdx11(6,xx,yy)

  !     Clear last time history plot data set

  call ptimpl()

  stop

end subroutine plstop
