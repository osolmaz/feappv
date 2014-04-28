! Id:$
subroutine pgetd( name, point, lengt, prec , flag )

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Retrieve an array from dictionary

  !      Inputs:
  !         name     - Name of array to retrieve

  !      Outputs:
  !         point    - Pointer to array in blank common
  !         lengt    - Length of array
  !         prec     - Precision of array
  !         flag     - Flag, true if array found
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'allotn.h'
  include  'allotd.h'
  include  'cdata.h'
  include  'debugs.h'
  include  'iofile.h'

  include  'p_point.h'

  character name*(*),dname*5
  logical ::   pcomp,flag
  integer ::   lengt, prec, i, irp(2,2)

  save

  data      irp / 3*1, 2 /

  !     Search dictionary for name

  dname = name

  do i = 1,ndict

     !       Assign pointer, length, and precision

     if( pcomp(dname, dict(i), 5) ) then
        prec  =  irp(iprec(i),ipr)
        point = (ipoint(i) + prec - 1)/prec - ipr*(2 - iprec(i))
        lengt = (ipoint(i+1) - ipoint(i))/prec
        prec  =  iprec(i)
        flag  = .true. 
        return
     endif
  end do

  !     Output error message if not found

  if(debug) then
     if(ior < 0) write(*,2000) dname(1:5)
     write(iow,2000) dname(1:5)
     !       if(ior.gt.0) call plstop()
  end if
  flag = .false. 

  !     Format

2000 format(' *WARNING* Check for ',a5, &
       ': Array not allocated for this problem.')

end subroutine pgetd
