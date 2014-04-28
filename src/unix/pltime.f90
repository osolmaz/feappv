! Id: pltime.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
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

  !     Display time for current view

  dtext = 0.00d0
  call pppcol(1,1)
  yy = ' '
  call tplot(1.13d0 , 0.135d0, yy, 15, 1)
  write(yy, '(6hTime =,1p,1e9.2)' ) ttim
  call tplot(1.13d0 , 0.135d0, yy, 15, 1)

end subroutine pltime
