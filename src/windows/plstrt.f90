! Id: plstrt.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine plstrt()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Open a graphics device to receive plot data

  !      Inputs:
  !         none

  !      Outputs:
  !         none      - Graphics window should appear after call
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  use       DFLIB

  implicit  none

  include  'pdata2.h'
  include  'plflag.h'
  include  'wdata.h'

  integer ::   status

  save

  !     Open devices for Graphics output

  everon = .true. 

  call clearscreen($GCLEARSCREEN)
  status = displaycursor($GCURSOROFF)

end subroutine plstrt
