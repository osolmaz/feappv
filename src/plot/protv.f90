! Id:$
subroutine protv(nty,u,angl,ndm,ndf,numnp, du)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Compute displacements in global Cartesian frame.

  !      Inputs:
  !         nty(*)    - Nodal type
  !         u(ndf,*)  - Solution vector at nodes
  !         angl(*)   - Value of angle for sloping b.c.
  !         ndm       - Spatial dimension of mesh
  !         ndf       - Number dof/node
  !         numnp     - Number of nodes in mesh

  !      Outputs:
  !         du(ndf,*) - Cartesian displacements at nodes
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  integer ::   ndm,ndf,numnp, i,n, nty(*)
  real*8 ::    ang,cn,sn
  real*8 ::    u(ndf,*),angl(*),du(ndf,*)

  save

  do n = 1,numnp
  if(nty(n) >= 0) then
    do i = 1,ndf
    du(i,n) = u(i,n)
    end do
    if(ndm > 1 .and. ndf > 1 .and. angl(n) /= 0.0d0) then
      ang = angl(n)*0.017453292d0
      cn  = cos(ang)
      sn  = sin(ang)
      du(1,n) = u(1,n)*cn - u(2,n)*sn
      du(2,n) = u(1,n)*sn + u(2,n)*cn
    endif
  else
    do i = 1,ndf
    du(i,n) = 0.0d0
    end do
  endif
  end do

end subroutine protv
