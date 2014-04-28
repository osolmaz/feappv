! Id: pltcur.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine pltcur()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Turn cursor on for text inputs

  !      Inputs:

  !      Outputs:
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  use      DFLIB

  implicit none

  integer ::  status

  status = displaycursor($GCURSORON)

  end
