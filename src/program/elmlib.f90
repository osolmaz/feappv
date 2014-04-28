! Id:$
subroutine elmlib(d,u,x,ix,t,s,p,i,j,k,jel,isw)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Element library driver routine
  !               N.B. Must set library flags in Subroutine PELNUM
  !                    for new program modules

  !      Inputs:
  !         d(*)    - Material parameters
  !         u(*)    - Element solution parameters
  !         x(*)    - Element nodal coordinates
  !         ix(*)   - Element nodal numbers
  !         t(*)    - Element temperatures
  !         i       - Number dof/node           (ndf)
  !         j       - Spatial dimension of mesh (ndm)
  !         k       - Size of element arrays    (nst)
  !         jel     - Element type number

  !      Outputs:
  !         d(*)    - Material parameters (isw = 1 only)
  !         s(*,*)  - Element array
  !         p(*)    - Element vector
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'eldata.h'
  include  'iofile.h'

  integer ::   i,j,k,jel,isw
  integer ::   ix(*)
  real*8 ::    p(*),s(*),d(*),u(*),x(*),t(*)

  save

  if(isw >= 3 .and. k > 0) then
    call pzero(s,k*k)
    call pzero(p,k  )
  endif

  if(jel > 0) then

    go to ( 1, 2, 3, 4, 5), jel
    go to 400

    1 call elmt01(d,u,x,ix,t,s,p,i,j,k,isw)
    go to 100
    2 call elmt02(d,u,x,ix,t,s,p,i,j,k,isw)
    go to 100
    3 call elmt03(d,u,x,ix,t,s,p,i,j,k,isw)
    go to 100
    4 call elmt04(d,u,x,ix,t,s,p,i,j,k,isw)
    go to 100
    5 call elmt05(d,u,x,ix,t,s,p,i,j,k,isw)

  else

    if(j == 1) then

      go to(101,102,103,104,105,106,107,108,109), -jel

      go to 400

      !         1-D solid

      101 write(*,*) ' No 1-d solid element available.'
      call plstop()

      102 call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
      go to 100

      103 write(*,*) ' No 1-d frame element available.'
      call plstop()

      104 write(*,*) ' No 1-d plate element available.'
      call plstop()

      105 write(*,*) ' No 1-d shell element available.'
      call plstop()

      106 write(*,*) ' No 1-d membrane element available.'
      call plstop()

      107 write(*,*) ' No 1-d thermal element available.'
      call plstop()

      108 write(*,*) ' No 1-d thermal convection element available.'
      call plstop()

      109 call pointnd(d,u,s,p,i,k,isw)
      go to 100

    elseif(j == 2) then

      go to(201,202,203,204,205,206,207,208,209), -jel

      go to 400

      !         2-D solid

      201 call solid2d(d,u,x,ix,t,s,p,i,j,k,isw)
      go to 100

      202 call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
      go to 100

      203 call frame2d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      204 call plate2d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      205 call shell2d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      206 write(*,*) ' No 2-d membrane element available.'
      call plstop()

      207 call therm2d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      208 call convec2d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      209 call pointnd(d,u,s,p,i,k,isw)
      go to 100

    elseif(j == 3) then

      go to(301,302,303,304,305,306,307,308,309), -jel

      go to 400

      !         3-D solid

      301 call solid3d(d,u,x,ix,t,s,p,i,j,k,isw)
      go to 100

      302 call trussnd(d,u,x,ix,t,s,p,i,j,k,isw)
      go to 100

      303 call frame3d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      304 write(*,*) ' No 3-d plate element available: Use SHELL.'
      call plstop()

      305 call shell3d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      306 call membr3d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      307 call therm3d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      308 call convec3d(d,u,x,ix,s,p,i,j,k,isw)
      go to 100

      309 call pointnd(d,u,s,p,i,k,isw)
      go to 100

    endif
  endif

  100 return

  !     Error

  400 if(ior > 0) write(iow,4000) n,jel
  if(ior < 0) write(  *,4000) n,jel
  call plstop()

  !     Format

  4000 format('  *ERROR* Element:',i6,', type number',i3,' input')

end subroutine elmlib
