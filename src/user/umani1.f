c$Id:$
      subroutine umani1

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Dummy user mesh manipulation set routine

c      Inputs:

c      Outputs:
c         none   - Users are responsible for generating outputs
c                  through common blocks, etc.  See programmer
c                  manual for example.
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include 'umac1.h'

      logical  pcomp

      if (pcomp(uct,'man1',4)) then
c         uct = 'name'
      else

      end if

      end
