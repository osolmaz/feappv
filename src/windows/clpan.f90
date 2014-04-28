! Id: clpan.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine clpan()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Close panel plots

  !      Inputs:
  !         none

  !      Outputs:
  !         none
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  implicit  none

  !     include  'pdata2.h'
  include  'pdatap.h'
  include  'pdatps.h'
  include  'pdataq.h'
  include  'plflag.h'
  include  'psdat3.h'

  integer ::   status, vfarea

  save

  !     Close panel for filled plots

  if(screfl) status = vfarea(npf,ixy)
  npf    = 0

  !     Fill panel for PostScript

  if (hdcpy .and. ipan >= 3 ) then
     call fppspl(ipan,xp,yp)
  endif

  !     Reinitialize panel counter

  ipan = 0

end subroutine clpan
