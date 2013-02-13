c$Id:$
      subroutine getcon(epmac)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Perform installation parameter computations

c      Inputs:

c      Outputs:
c         epmac   - Smallest number that can be added to 1.0
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'psize.h'
      include  'comblk.h'

      real*8    epmac

      save

c     Compute machine epsilon estimate

      epmac = 1.0d0
100   epmac = epmac*0.5d0
      if(1.d0 .ne. 1.d0 + epmac) go to 100
      epmac = 2.0d0*epmac

      end
