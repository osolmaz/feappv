! Id: pltime.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine pltime()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Place time on plot window

  !      Inputs:  None

  !      Outputs: To plot window
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  implicit  none

  include  'tdata.h'
  include  'pdatxt.h'

  character yy*15

  save

  data      yy / ' ' /

  !     Display time for current view

  dtext = 0.11d0
  call pppcol(-1,1)
  call tplot(1.02d0 , 0.135d0, yy, 15, 1)
  call pppcol(1,1)
  write(yy, '(6hTime =,1p,1e9.2)' ) ttim
  call tplot(1.02d0 , 0.135d0, yy, 15, 1)

end subroutine pltime
