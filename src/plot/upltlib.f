c$Id:$
      subroutine upltlib(i,ct)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Interface for user plot commands

c      Inputs:
c         i      - Command number
c         ct(3)  - Parameters from input line

c      Outputs:
c         None   - Users are responsible for providing outputs in
c                  uploti routines
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   i
      real*8    ct(3)

      save

      if(i.eq.1) then
        call uplot1(ct)
      elseif(i.eq.2) then
        call uplot2(ct)
      elseif(i.eq.3) then
        call uplot3(ct)
      elseif(i.eq.4) then
        call uplot4(ct)
      elseif(i.eq.5) then
        call uplot5(ct)
      endif

      end
