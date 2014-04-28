! Id:$
subroutine genclr(ndf, v, nty, numnp)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Remove initial conditions on nodes merged by tie command

  !      Inputs:
  !         ndf      - Number dof/node
  !         nty(*)   - Nodal type
  !         numnp    - Number of nodes

  !      Outputs:
  !         v(ndf,*) - Initial conditions with merged nodes removed
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  integer ::   ndf,numnp, i,n, nty(numnp)
  real*8 ::    v(ndf,numnp)

  do n = 1,numnp
  if(nty(n) < 0) then
    do i = 1,ndf
    v(i,n) = 0.0d0
    end do
  endif
  end do

end subroutine genclr
