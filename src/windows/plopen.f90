! Id: plopen.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine plopen

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !     Purpose: Open graphics screen to receive plot data

  !      Inputs:
  !         none

  !      Outputs:
  !         none
  !-----[--+---------+---------+---------+---------+---------+---------+-]
  implicit  none

  include  'pdata2.h'
  include  'pdatap.h'
  include  'pdatps.h'
  include  'plflag.h'
  include  'print.h'
  include  'wdata.h'

  logical ::   hdcpyo
  integer ::   ifrfl,icol,status,vclrwk,vgrwin

  save

  !     If never open start the plot

  if( .not. everon) call plstrt()

  !     Open plot device

  status = vgrwin()

  if(fopn) return
  fopn   = .true. 
  hdcpyo = hdcpy
  icol   = 1

  if(iclear == 0) then
     if(screfl) status = vclrwk()
  endif

  !     PostScript

  if(iclear == 0) then
     iclear = 1

     !       Put up border

     if ( hdlogo .and. hdcpy ) then
        hdcpy = .false. 
        ifrfl = 1
     endif

     if(iwindow == 1) then
        if(bordfl) call plbord(icol)

        !         Put up logo for feap

        call pfeap(0.983d0,0.017d0,0.250d0,3,3)
        call pfeap(0.980d0,0.020d0,0.250d0,2,1)
     endif

     if (ifrfl == 1) then
        hdcpy = hdcpyo
        ifrfl = 0
     endif

  endif

end subroutine plopen
