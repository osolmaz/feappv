! Id:$
subroutine elmt04(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  implicit  none

  integer ::   ndf , ndm , nst , isw
  integer ::   ix(*)
  real*8 ::    d(*), ul(*), xl(*), tl(*), s(*), p(*)

  if(isw > 0) write(*,2000)
  2000 format('    Elmt 04: *WARNING* Dummy subroutine called')
end subroutine elmt04
