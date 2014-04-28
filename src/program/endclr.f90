! Id:$
subroutine    endclr (subnam,chr)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: End-of-file clearing routine

  !      Inputs:
  !         subnam - Character array storing calling subroutine name

  !      Outputs:
  !         chr    - Blank character to clear error
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'iofile.h'

  character subnam*(*),chr*(*)

  if (ior > 0)  then
     write(iow,2000) subnam
     call pdelfl()
     call plstop()
  else
     write(*,2000) subnam
     chr = ' '
  endif

  !     Format

2000 format (' *ERROR in ',a,' ** end of file encountered')

end subroutine endclr
