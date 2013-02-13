c$Id:$
      subroutine uglobl(vtype,td)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Dummy user global parameter routine

c      Inputs:
c         vtype(2)  - Character array describing user global command
c         td(5)     - Real array of data for global command

c      Outputs:
c         N.B. Users must provide output via common blocks, etc.
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      character vtype(2)*15
      real*8    td(5)

      end
