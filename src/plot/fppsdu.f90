! Id:$
subroutine fppsdu()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose:  Output string of characters to PostScript file

  !      Inputs:
  !        none       - Input through common /plpost/

  !      Outputs:
  !        none       - Outputs are written to PostScript file
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  include  'iodata.h'
  include  'plpost.h'

  integer ::   i, first, last

  save

  if (nxtchr > 0) then

     !       Write to lun

     do first = 1,nxtchr
        if(buffer(first) /= ' ') go to 100
     end do
     return
100  do last = nxtchr,first,-1
        if(buffer(last) /= ' ') go to 200
     end do

200  write (lun,'(80a1)') (buffer(i), i=first,last)
     nxtchr = 0

     !       Clear buffer

     do i=1, 80
        buffer(i) = ' '
     end do

  end if

end subroutine fppsdu
