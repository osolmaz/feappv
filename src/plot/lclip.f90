! Id:$
logical function lclip( ix, nen, x, ndm )

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Clip plot at specified planes

  !      Inputs:
  !         ix(*)     - List of nodes on element
  !         nen       - Number of nodes on element
  !         x(ndm,*)  - Nodal coordinates of element
  !         ndm       - Spatial dimension of mesh

  !      Outputs:
  !         lclip     - Flag, true if element is within clip region
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  include  'plclip.h'

  integer ::   i, n, nen,ndm

  integer ::   ix(nen)
  real*8 ::    x(ndm,*),x0(3)

  save

  do i = 1, ndm
     x0(i) = 0.0d0
     do n = 1,nen
        if(ix(n) > 0) x0(i) = x0(i) + x(i,ix(n))
     end do
     x0(i) = x0(i)/nen
  end do
  if(ndm == 1) then
     lclip = (x0(1) >= cmin(1)) .and. (x0(1) <= cmax(1))
  elseif(ndm == 2) then
     lclip = (x0(1) >= cmin(1)) .and. (x0(1) <= cmax(1)) &
          .and. &
          (x0(2) >= cmin(2)) .and. (x0(2) <= cmax(2))
  elseif(ndm == 3) then
     lclip = (x0(1) >= cmin(1)) .and. (x0(1) <= cmax(1)) &
          .and. &
          (x0(2) >= cmin(2)) .and. (x0(2) <= cmax(2)) &
          .and. &
          (x0(3) >= cmin(3)) .and. (x0(3) <= cmax(3))
  else
     lclip = .false. 
  endif

end function lclip
