c$Id:$
      subroutine usetlib(i)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Interface for user mesh manipulation set  commands

c      Inputs:
c         i      - Command number

c      Outputs:
c         None   - Users are responsible for providing outputs in
c                  umanii routines
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      integer   i

      save

      if(i.eq.1) then
        call umani1
      elseif(i.eq.2) then
        call umani2
      elseif(i.eq.3) then
        call umani3
      elseif(i.eq.4) then
        call umani4
      elseif(i.eq.5) then
        call umani5
      endif

      end
