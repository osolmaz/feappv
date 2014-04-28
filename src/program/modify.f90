! Id:$
subroutine modify(b,ld,s,dul,nst)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Modify element residual for effects of specified
  !               boundary values.

  !               b(i) = b(i) - s(i,j)*dul(j)

  !      Inputs:
  !         ld(*)  - Array with negative entries where boundary
  !                  solution to be imposed
  !         s(*,*) - Element tangent array
  !         dul(*) - Value of specified solution increments
  !         nst    - Dimension of element arrays

  !      Outputs:
  !         b(*)   - Residual modified for effect of increments
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  integer ::   nst,i,j, ii
  integer ::   ld(nst)
  real*8 ::    b(nst),s(nst,nst),dul(nst)

  !     Loop over columns and search for boundary terms

  do j = 1,nst
  if(ld(j) < 0) then

    !         Loop over rows to modify active equations

    do i = 1,nst
    ii = ld(i)
    if(ii > 0) then
      b(ii) = b(ii) - s(i,j)*dul(j)
    endif
    end do

  endif
  end do

end subroutine modify
