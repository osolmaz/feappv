! Id: pstart.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
subroutine pstart()
  !
  !      * * F E A P * * A Finite Element Analysis Program
  !                        -      -       -        -
  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  implicit      none

  include  'pdata2.h'

  !     Start for X11 graphics driver

  idev = 1

end subroutine pstart
