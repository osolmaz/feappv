c$Id:$
      subroutine uplot5(ctl)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Dummy user plot routine

c      Inputs:
c         ctl(3) - Parameters for plots

c      Outputs:
c         none   - Users are responsible for generating outputs
c                  through common blocks, etc.  See programmer
c                  manual for example.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include 'umac1.h'  ! uct

      logical  pcomp
      real*8   ctl(3)

c     Provide user plot name

      if (pcomp(uct,'plt5',4)) then
c         uct = 'name'

c     Perform user plot function

      else

      end if

      end
