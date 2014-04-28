! Id: plabl.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine plabl(m)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Place numerical labels on plot data
  !               N.B. Must be preceded by a move to location where
  !                    value is centered

  !      Inputs:
  !         m         - Number to place on plot

  !      Outputs:
  !         none      - Plot output to screen/file
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  implicit  none

  !     include  'pdata2.h'
  include  'pdatap.h'
  include  'pdatps.h'
  include  'pdatxt.h'
  include  'plflag.h'
  include  'psdat3.h'
  !     include  'x11f.h'

  character yyy*6
  integer ::   m,n,nchar
  real*8 ::    x1,y1

  save

  !     Set number of characters

  n = abs(m)

  if    (n >= 0   .and. n < 10   ) then
    write(yyy,'(i1)') n
    nchar = 1
  elseif(n >= 10  .and. n < 100  ) then
    write(yyy,'(i2)') n
    nchar = 2
  elseif(n >= 100 .and. n < 1000 ) then
    write(yyy,'(i3)') n
    nchar = 3
  elseif(n >= 1000 .and. n < 10000 ) then
    write(yyy,'(i4)') n
    nchar = 4
  elseif(n >= 10000 .and. n < 100000 ) then
    write(yyy,'(i5)') n
    nchar = 5
  else
    write(yyy,'(i6)') n
    nchar = 6
  endif

  !     MicroSoft graphics

  dtext = 0.0d0
  x1    = jx1/22000.d0
  y1    = jy1/22000.d0
  if(screfl) call tplot(x1,y1,yyy,nchar,0)

  end
