! Id:$
subroutine solid3d(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !     Three Dimensional Solid Element Driver
  ! ____________________________________________________________________c

  implicit  none

  include  'cdata.h'
  include  'eldata.h'
  include  'evdata.h'
  include  'hdata.h'
  include  'iofile.h'
  include  'pmod2d.h'
  include  'strnum.h'
  include  'comblk.h'

  integer ::   ndf,ndm,nst,isw
  integer ::   i,tdof

  integer ::   ix(*)
  real*8 ::    d(*),ul(ndf,*),xl(ndm,*),tl(*),s(nst,nst),p(nst)
  real*8 ::    shp(4,8),th(8)

  save

  !     Extract type data

  stype = nint(d(16))
  etype = nint(d(17))
  dtype = nint(d(18))
  hflag = d(30) == 1.d0

  !     Set nodal temperatures: Can be specified or computed

  if(isw > 1) then
     tdof = nint(d(19))
     if(tdof <= 0) then
        do i = 1,nel ! {
           th(i) = tl(i)
        end do ! i     }
     else
        do i = 1,nel ! {
           th(i) = ul(tdof,i)
        end do ! i     }
     endif
  endif

  !     Go to correct process

  go to(1,2,3,3,5,3,3,3,3,3,3,3), isw

  !     Output element type

  if(isw == 0 .and. ior < 0) then
     write(*,*) '   Elmt  1: 3-d Solid Linear/Finite Defm. Element.'
  endif

  return

  !     Input material properties

1 write(iow,2001)
  if(ior < 0) write(*,2001)
  call inmate(d,tdof,  0 ,1)
  nh1 = nh1 + 2
  if(etype == 3) then
     nh1 = nh1 + 12
  endif

  !     Set tdof to zero if 1, 2, 3, or larger than ndf

  if(tdof > ndf) then
     write(iow,3003)
     if(ior < 0) write(*,3003)
     tdof = 0
  elseif(tdof >= 1 .and. tdof <= 3) then
     write(iow,3004)
     if(ior < 0) write(*,3004)
     tdof = 0
  endif

  !     Deactivate dof in element for dof > 3

  do i = 4,ndf
     ix(i) = 0
  end do

  !     If temperature dof is specified activate dof

  if(tdof > 0) then
     ix(tdof) = 1
  endif

  !     Set plot sequence for 4-nod tet or 8-node brick

  if(nen == 4) then
     call pltet4(iel)
  else
     call plbrk8(iel)
  endif

  !     Set number of stress projections

  istv = 6

  return

  !     Check element for errors

2 call ckbrk8 ( n, ix, xl, ndm, nel, shp )

  return

  !     Compute stress-divergence vector (p) and stiffness matrix (s)

3 if(etype == 1) then

     !       Displacement Model

     if(dtype > 0) then
        call sld3d1(d,ul,xl,ix,th,s,p,ndf,ndm,nst,isw)
     else
        call fld3d1(d,ul,xl,s,p,ndf,ndm,nst,isw)
     endif

  endif

  return

  !     Compute mass or geometric striffness matrix

5 if(imtyp == 1) then
     call mass3d(d,xl,s,p,ndf,ndm,nst)
  else
     !       Geometric stiffness here
  endif
  return

  !     Formats for input-output

2001 format( &
       /5x,'T h r e e   D i m e n s i o n a l   S o l i d', &
       '   E l e m e n t'/)
3003 format(' *WARNING* Thermal d.o.f. > active d.o.f.s : Set to 0')
3004 format(' *WARNING* Thermal d.o.f. can not be 1 to 3: Set to 0')

end subroutine solid3d

subroutine fld3d1(d,ul,xl,s,r,ndf,ndm,nst,isw)

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: 3-D finite deformation displacement element
  !      Remark: This a completely standard mechanical element
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'bdata.h'
  include  'cdata.h'
  include  'eldata.h'
  include  'elengy.h'
  include  'elplot.h'
  include  'eltran.h'
  include  'hdata.h'
  include  'iofile.h'
  include  'ndata.h'
  include  'plast3f.h'
  include  'pmod2d.h'
  include  'prstrs.h'
  include  'rdata.h'
  include  'comblk.h'

  logical ::   tetfl
  integer ::   ndf,ndm,nst,isw, i,ii,i1, j,jj,j1,  k
  integer ::   l,lint, nhv,nn,istrt
  real*8 ::    bdb, ta
  real*8 ::    qfact
  real*8 ::    d(*),ul(ndf,nen,*),xl(ndm,*),s(nst,*),r(ndf,*)
  real*8 ::    sg(4,9),sv(5,16),xsj(9),xx(3)
  real*8 ::    f(9,2,9),finv(3,3,9),df(3,3,9),detfi(2,9)
  real*8 ::    shp(4,8,9),be(6), body(3)
  real*8 ::    aa(6,6,5),dd(6,6,9), xu(3,9)
  real*8 ::    sigv(13),sigl(6,9), sigp(6,9)
  real*8 ::    bbd(3,6),r1(3,9)
  real*8 ::    dvol(9),dvol0(9),weng(9),xr(3,9),ur(3,9)

  save

  !     MATERIAL DATA

  if(isw == 1) then

     ta    = 0.0d0

     !     N.B. Augmenting not allowed in displacement model

  elseif(isw == 10) then

     !     COMPUTE TANGENT STIFFNESS/RESIDUAL FORCE AND OUTPUTS

  else

     !       Get quadrature information

     if(nel == 4) then
        tetfl = .true. 
        l     =  1
        call tint3d (l,lint,sv)
     else
        tetfl = .false. 
        l     = nint(d(5))
        call int3d(l,lint,sg)
     endif

     !       Compute shape functions

     do l = 1,lint
        if(tetfl) then
           call tetshp(sv(1,l),xl,ndm,xsj(l),shp(1,1,l))
           dvol0(l) = xsj(l)*sv(5,l)
        else
           call shp3d(sg(1,l),xsj(l),shp(1,1,l),xl,ndm)
           dvol0(l) = xsj(l)*sg(4,l)
        endif
     end do ! l

     !       Compute coordinates

     do l = 1,lint
        do i = 1,3
           xr(i,l) = 0.0d0
           ur(i,l) = 0.0d0
           do j = 1,nel
              xr(i,l) = xr(i,l) + shp(4,j,l)*xl(i,j)
              ur(i,l) = ur(i,l) + shp(4,j,l)*ul(i,j,1)
           end do ! j
           ur(i,l) = ur(i,l) + xr(i,l)
        end do ! i
     end do ! l

     !       Set history type and size

     nhv   = nint(d(15))
     istrt = nint(d(84))

     !       Initialize history variables

     if(isw == 14) then

        do l = 1,lint
           do i = 1,9
              f(i,1,l)    = 0.0d0
              f(i,2,l)    = 0.0d0
              finv(i,1,l) = 0.0d0
           end do ! i
           detfi(1,l)  = 1.d0
           detfi(2,l)  = 1.d0
           do i = 1,9,4
              f(i,1,l)    = 1.0d0
              f(i,2,l)    = 1.0d0
           end do ! i
           finv(1,1,l) = 1.0d0
           finv(2,2,l) = 1.0d0
           finv(3,3,l) = 1.0d0
        end do ! l

     else

        !         Compute deformation gradient, inverse and determinant.

        do l = 1,lint
           call kine3df(shp(1,1,l),ul,f(1,1,l),finv(1,1,l),df(1,1,l), &
                detfi(1,l),ndf,nel,nen)
           dvol(l) = dvol0(l)*detfi(1,l)
        end do ! l

     endif

     !       Compute Cauchy stresses and spatial tangent tensor at t-n+1

     nn  = 0
     do l = 1,lint

        estore = 0.0d0
        call modlfd(d,f(1,1,l),df(1,1,l),detfi(1,l),ta, &
             hr(nh1+nn),hr(nh2+nn),nhv,istrt,aa,sigv,be, &
             .false. ,isw)
        weng(l) = estore

        !         Multiply tangent moduli and stresses by volume element.
        !         Store time history plot data for element

        k = 6*(l-1)
        do i = 1,6
           tt(i+k)   = sigv(i)
           sigp(i,l) = sigv(i)
           sigl(i,l) = sigv(i)*dvol(l)
           do j = 1,6
              dd(j,i,l) = aa(j,i,1)*dvol(l)*ctan(1)
           end do ! j
        end do ! i
        nn = nn + nhv

     end do ! l

     !       STIFFNESS AND RESIDUAL

     if(isw == 3 .or. isw == 6) then

        !         Compute body forces values

        call sbodyf(d, body)

        !         Compute inertia effects: shflg = .true. for eigen shifts

        if(ctan(3) /= 0.0 .or. shflg) then
           call iner3d(d,xl,ul(1,1,4),ul(1,1,5),s,r, nel,ndf,ndm,nst)
        endif ! ctan(3) test

        do l = 1,lint

           !           COMPUTE STRESS DIVERGENCE TERM

           !           Compatible internal force.

           do i = 1,nel
              r1(1,i) = shp(1,i,l)*sigl(1,l) &
                   + shp(2,i,l)*sigl(4,l) &
                   + shp(3,i,l)*sigl(6,l)
              r1(2,i) = shp(1,i,l)*sigl(4,l) &
                   + shp(2,i,l)*sigl(2,l) &
                   + shp(3,i,l)*sigl(5,l)
              r1(3,i) = shp(1,i,l)*sigl(6,l) &
                   + shp(2,i,l)*sigl(5,l) &
                   + shp(3,i,l)*sigl(3,l)

              do j = 1,3
                 r(j,i)  = r(j,i) + shp(4,i,l)*body(j)*dvol0(l) &
                      - r1(j,i)
              end do ! j
           end do ! i

           !           COMPUTE K11

           if(isw == 3) then

              i1 = 0
              do i = 1,nel

                 !               PART 1. - geometric tangenta

                 j1 = 0
                 if(gflag) then
                    do j = 1,nel

                       !                   Accumulate geometric factor with consistent mass

                       bdb = (r1(1,i)*shp(1,j,l) &
                            +  r1(2,i)*shp(2,j,l) &
                            +  r1(3,i)*shp(3,j,l))*ctan(1)

                       do jj = 1,3
                          s(i1+jj,j1+jj) = s(i1+jj,j1+jj) + bdb
                       end do ! jj
                       j1 = j1 + ndf
                    end do ! j
                 endif

                 !               PART 2. - tangent modulus part (based upon aa-array)

                 do jj = 1,6
                    bbd(1,jj) = shp(1,i,l)*dd(1,jj,l) &
                         + shp(2,i,l)*dd(4,jj,l) &
                         + shp(3,i,l)*dd(6,jj,l)
                    bbd(2,jj) = shp(1,i,l)*dd(4,jj,l) &
                         + shp(2,i,l)*dd(2,jj,l) &
                         + shp(3,i,l)*dd(5,jj,l)
                    bbd(3,jj) = shp(1,i,l)*dd(6,jj,l) &
                         + shp(2,i,l)*dd(5,jj,l) &
                         + shp(3,i,l)*dd(3,jj,l)
                 end do ! jj

                 !               Compute tangent stiffness

                 j1 = 0
                 do j  = 1,nel
                    s(i1+1,j1+1) = s(i1+1,j1+1) &
                         + bbd(1,1)*shp(1,j,l) &
                         + bbd(1,4)*shp(2,j,l) &
                         + bbd(1,6)*shp(3,j,l)
                    s(i1+1,j1+2) = s(i1+1,j1+2) &
                         + bbd(1,4)*shp(1,j,l) &
                         + bbd(1,2)*shp(2,j,l) &
                         + bbd(1,5)*shp(3,j,l)
                    s(i1+1,j1+3) = s(i1+1,j1+3) &
                         + bbd(1,6)*shp(1,j,l) &
                         + bbd(1,5)*shp(2,j,l) &
                         + bbd(1,3)*shp(3,j,l)
                    s(i1+2,j1+1) = s(i1+2,j1+1) &
                         + bbd(2,1)*shp(1,j,l) &
                         + bbd(2,4)*shp(2,j,l) &
                         + bbd(2,6)*shp(3,j,l)
                    s(i1+2,j1+2) = s(i1+2,j1+2) &
                         + bbd(2,4)*shp(1,j,l) &
                         + bbd(2,2)*shp(2,j,l) &
                         + bbd(2,5)*shp(3,j,l)
                    s(i1+2,j1+3) = s(i1+2,j1+3) &
                         + bbd(2,6)*shp(1,j,l) &
                         + bbd(2,5)*shp(2,j,l) &
                         + bbd(2,3)*shp(3,j,l)
                    s(i1+3,j1+1) = s(i1+3,j1+1) &
                         + bbd(3,1)*shp(1,j,l) &
                         + bbd(3,4)*shp(2,j,l) &
                         + bbd(3,6)*shp(3,j,l)
                    s(i1+3,j1+2) = s(i1+3,j1+2) &
                         + bbd(3,4)*shp(1,j,l) &
                         + bbd(3,2)*shp(2,j,l) &
                         + bbd(3,5)*shp(3,j,l)
                    s(i1+3,j1+3) = s(i1+3,j1+3) &
                         + bbd(3,6)*shp(1,j,l) &
                         + bbd(3,5)*shp(2,j,l) &
                         + bbd(3,3)*shp(3,j,l)
                    j1 = j1 + ndf
                 end do ! j
                 i1 = i1 + ndf
              end  do ! i

           endif

        end do ! l

        !       OUTPUT STRESSES

     elseif(isw == 4 .or. isw == 8 .or. isw == 16) then

        do i = 1,3
           xx(i) = 0.0d0
        end do ! i
        do i = 1,13
           sigv(i) = 0.0d0
        end do ! i
        qfact = 1.d0/dble(lint)

        do l = 1,lint

           do j = 1,3
              do i=1,nel
                 xx(j)   = xx(j) + qfact*shp(4,i,l)*xl(j,i)
              end do ! i
           end do ! j

           !           Move stresses and jacobian for printing

           do i = 1,6
              sigv(i) = sigv(i) + qfact*sigp(i,l)
           end do ! i

        end do ! l

        !         OUTPUT STRESSES

        if (isw == 4) then

           call pstr3d(sigv,sigv(7))

           mct = mct - 2
           if(mct <= 0) then
              write(iow,2001) o,head
              if(ior < 0) write(*,2001) o,head
              mct = 50
           endif

           write(iow,2002) n,(sigv(ii),ii=1,6), &
                ma,(sigv(ii),ii=7,9),xx
           if(ior < 0) then
              write(*,2002) n,(sigv(ii),ii=1,6), &
                   ma,(sigv(ii),ii=7,9),xx
           end if

           !         PROJECT STRESSES ONTO THE NODES FOR PLOTTING

        elseif(isw == 8) then

           !           Compute current geometry

           do i = 1,ndm
              do j = 1,nel
                 xu(i,j) = xl(i,j) + ul(i,j,1)
              end do ! j
           end do ! i

           call slcn3d(sigp,shp,dvol0, r,s, lint,nel,8)

        endif
     endif
  endif

  !     FORMAT STATEMENTS

2001 format(a1,20a4//5x,'Element Stresses'// &
       '   Elem.   11-Stress   22-Stress   33-Stress   12-Stress', &
       '   23-Stress   13-Stress'/ &
       '   Matl.    1-Stress    2-Stress    3-Stress', &
       '     1-Coord     2-Coord     3-Coord ')

2002 format(i8,1p,6e12.4/i8,1p,6e12.4/1x)

end subroutine fld3d1

subroutine iner3d(d,xl,vl,al,s,r, nel,ndf,ndm,nst)

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Compute inertial effects for 3-d elements
  !               Includes effects of Rayleigh mass damping.

  !      Inputs:
  !         d(*)      - Material set parameters
  !         xl(ndm,*) - Nodal coordinates for element
  !         vl(ndf,*) - Velocity for element
  !         al(ndf,*) - Acceleration for element
  !         ctan3     - Mass tangent factor
  !         nel       - Number of element nodes
  !         ndf       - Number dof/node
  !         ndm       - Spatial dimension of mesh
  !         nst       - Size of element arrays

  !      Outputs:
  !         s(nst,*)  - Consistent or interpolated mass
  !         r(ndf,*)  - Element inertial force
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'eltran.h'   ! ctan(3)

  logical ::   tetfl
  integer ::   nel,ndf,ndm,nst, i,ii,i1, jj,j1, l,lint
  real*8 ::    xsj,dv,dvm, aj1,aj2,lfac,cfac
  real*8 ::    d(*),xl(ndm,nel),vl(ndf,nel),al(ndf,nel)
  real*8 ::    s(nst,nst),r(ndf,nel)
  real*8 ::    shp(4,8),sg(4,9),sv(5,16), cmass(8,8)

  save

  !     Compute mass quadrature

  if(nel == 4) then
     tetfl = .true. 
     l     =  2
     call tint3d (l,lint,sv)
  else
     tetfl = .false. 
     l     = nint(d(5))
     call int3d(l,lint,sg)
  endif

  !     Set mass interpolation factor between consistent (1) and lumped (0)

  cfac = d(7)
  lfac = 1.d0 - cfac
  dvm  = ctan(3) + d(77)*ctan(2)

  !     Initialize mass

  do jj = 1,nel
     do ii = 1,nel
        cmass(ii,jj) = 0.0d0
     end do ! ii
  end do ! jj

  do l = 1,lint

     !       Compute shape functions

     if(tetfl) then
        call tetshp(sv(1,l),xl,ndm,xsj,shp)
        dv = sv(5,l)*xsj*d(4)
     else
        call shp3d(sg(1,l),xsj,shp,xl,ndm)
        dv = sg(4,l)*xsj*d(4)
     endif

     !       Compute mass

     do jj = 1,nel

        !         Compute db = rho*shape*dv

        aj1 = shp(4,jj)*dv
        aj2 = cfac*aj1
        aj1 = lfac*aj1
        cmass(jj,jj) = cmass(jj,jj) + aj1
        do ii = 1,nel
           cmass(ii,jj) = cmass(ii,jj) + shp(4,ii)*aj2
        end do ! ii
     end do ! jj

  end do ! l

  !     Compute inertial effect

  do ii = 1,nel
     do jj = 1,nel
        do i = 1,3
           r(i,ii) = r(i,ii) - (al(i,jj) + d(77)*vl(i,jj))*cmass(ii,jj)
        end do ! i
     end do ! jj
  end do ! ii

  !     Expand mass into element array

  j1 = 0
  do jj = 1,nel
     i1 = 0
     do ii = 1,nel
        cmass(ii,jj) = cmass(ii,jj)*dvm
        do i = 1,ndm
           s(i1+i,j1+i) = s(i1+i,j1+i) + cmass(ii,jj)
        end do ! i
        i1 = i1 + ndf
     end do ! ii
     j1 = j1 + ndf
  end do ! jj

end subroutine iner3d

subroutine kine3df(shp,ul,f,fi,df,detfi,ndf,nel,nen)

  !-----[--.----+----.----+----.-----------------------------------------]
  !     Purpose: Compute deformation gradient and its inverse at tn+1

  !     Inputs:
  !        shp(4,*)  - Shape functions
  !        ul(ndf,*) - Nodal solution values
  !        ndf       - Degrees of freedom / node
  !        nel       - Number of element nodes
  !        nen       - Dimension for ul

  !     Outputs:
  !        f(3,3,*)  - Deformation gradients
  !        fi(3,3)   - Inverse deformation gradient
  !        df(3,3)   - Incremental deformation gradient
  !        detfi(*)  - Determinant of deformation gradient
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit none

  integer ::  ndf,nel,nen, i,j,k
  real*8 ::   shp(4,*),ul(ndf,nen,*)
  real*8 ::   f(3,3,*),fi(3,3),df(3,3),detfi(*), deti

  !     Compute compatible deformation gradient at t-n+1: F = I + GRAD u

  do i = 1,3
     do j = 1,3
        f(i,j,1)  = 0.0d0
        f(i,j,2)  = 0.0d0
        df(i,j)   = 0.0d0
        do k = 1,nel
           f(i,j,1) = f(i,j,1) + ul(i,k,1)*shp(j,k)
           df(i,j)  = df(i,j)  + ul(i,k,2)*shp(j,k)
        end do ! k
        f(i,j,2) = f(i,j,1) - df(i,j)
     end do ! j
     f(i,i,1) = f(i,i,1) + 1.0d0
     f(i,i,2) = f(i,i,2) + 1.0d0
  end do ! i

  !     Invert F_n

  detfi(2) = f(1,1,2)*f(2,2,2)*f(3,3,2) + f(1,2,2)*f(2,3,2)*f(3,1,2) &
       + f(1,3,2)*f(2,1,2)*f(3,2,2) - f(3,1,2)*f(2,2,2)*f(1,3,2) &
       - f(3,2,2)*f(2,3,2)*f(1,1,2) - f(3,3,2)*f(2,1,2)*f(1,2,2)

  !     Invert F_n+1

  detfi(1) = f(1,1,1)*f(2,2,1)*f(3,3,1) + f(1,2,1)*f(2,3,1)*f(3,1,1) &
       + f(1,3,1)*f(2,1,1)*f(3,2,1) - f(3,1,1)*f(2,2,1)*f(1,3,1) &
       - f(3,2,1)*f(2,3,1)*f(1,1,1) - f(3,3,1)*f(2,1,1)*f(1,2,1)

  deti    = 1.d0/detfi(1)
  fi(1,1) = (f(2,2,1)*f(3,3,1) - f(3,2,1)*f(2,3,1))*deti
  fi(1,2) =-(f(1,2,1)*f(3,3,1) - f(3,2,1)*f(1,3,1))*deti
  fi(1,3) = (f(1,2,1)*f(2,3,1) - f(2,2,1)*f(1,3,1))*deti
  fi(2,1) =-(f(2,1,1)*f(3,3,1) - f(3,1,1)*f(2,3,1))*deti
  fi(2,2) = (f(1,1,1)*f(3,3,1) - f(3,1,1)*f(1,3,1))*deti
  fi(2,3) =-(f(1,1,1)*f(2,3,1) - f(2,1,1)*f(1,3,1))*deti
  fi(3,1) = (f(2,1,1)*f(3,2,1) - f(3,1,1)*f(2,2,1))*deti
  fi(3,2) =-(f(1,1,1)*f(3,2,1) - f(3,1,1)*f(1,2,1))*deti
  fi(3,3) = (f(1,1,1)*f(2,2,1) - f(2,1,1)*f(1,2,1))*deti

  !     Push forward standard shape functions

  do i = 1,nel
     call pushv3f(fi,shp(1,i))
  end do ! i

end subroutine kine3df

subroutine mass3d(d,xl,s,p,ndf,ndm,nst)

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Compute mass matrix for 3-d tet and brick elements

  !      Inputs:
  !         d(*)      - Material set parameters
  !         xl(ndm,*) - Nodal coordinates for element
  !         ndf       - Number dof/node
  !         ndm       - Spatial dimension of mesh
  !         nst       - Size of element arrays

  !      Outputs:
  !         s(nst,*)  - Consistent or interpolated mass
  !         p(nst)    - Diagonal (lumped) mass
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  include  'eldata.h'
  include  'pmod2d.h'

  logical ::   quad
  integer ::   ndf,ndm,nst
  integer ::   j,k,l,j1,k1,lint

  real*8 ::    xsj,dv, aj1,lfac,cfac

  real*8 ::    d(*),xl(ndm,*),s(nst,nst),p(nst)
  real*8 ::    shp(4,8),sg(4,8),sv(5,4)

  save

  !     Compute mass matrix

  if(nel == 4) then
     l    =  2
     quad = .false. 
     call tint3d(l,lint,sv)
  else
     l    = nint(d(5))
     quad = .true. 
     call int3d(l,lint,sg)
  endif

  !     Set mass interpolation factor between consistent (1) and lumped (0)

  cfac = d(7)
  lfac = 1.d0 - cfac

  do l = 1,lint

     !       Compute shape functions

     if(quad) then
        call shp3d(sg(1,l),xsj,shp,xl,ndm)
        dv = sg(4,l)*xsj*d(4)
     else
        call tetshp(sv(1,l),xl,ndm,xsj,shp)
        dv = sv(5,l)*xsj*d(4)
     endif

     !       For each node j compute db = rho*shape*dv

     j1 = 1
     do j = 1,nel
        aj1 = shp(4,j)*dv

        !         Compute a lumped mass

        p(j1)    = p(j1)    + aj1
        s(j1,j1) = s(j1,j1) + aj1*lfac
        aj1      = aj1*cfac

        !         For each node k compute mass matrix (upper triangular part)

        k1 = 1
        do k = 1,nel
           s(j1,k1) = s(j1,k1) + shp(4,k)*aj1
           k1 = k1 + ndf
        end do
        j1 = j1 + ndf
     end do
  end do

  !     Compute missing parts and lower part by symmetries

  do j = 1,ndf*nel,ndf
     p(j+1) = p(j)
     p(j+2) = p(j)
     do k = 1,ndf*nel,ndf
        s(j+1,k+1) = s(j,k)
        s(j+2,k+2) = s(j,k)
     end do
  end do

end subroutine mass3d


subroutine rays3d(d,shp,shpbar,sig,dr,vl,ndf,nel,mixed)

  !-----[--.----+----.----+----.-----------------------------------------]
  !     Purpose: Stiffness proportional Rayleigh damping residual

  implicit  none

  logical ::   mixed
  integer ::   ndf,nel, i,j
  real*8 ::    theta,dtheta

  real*8 ::    d(*),shp(4,*),shpbar(3,*),sig(*),dr(6,6),vl(ndf,*)
  real*8 ::    eps(6)

  !     Compute strain rate terms

  do j = 1,6
     eps(j) = 0.0d0
  end do
  do j = 1,nel
     eps(1) = eps(1) + shp(1,j)*vl(1,j)
     eps(2) = eps(2) + shp(2,j)*vl(2,j)
     eps(3) = eps(3) + shp(3,j)*vl(3,j)
     eps(4) = eps(4) + shp(2,j)*vl(1,j) + shp(1,j)*vl(2,j)
     eps(5) = eps(5) + shp(3,j)*vl(2,j) + shp(2,j)*vl(3,j)
     eps(6) = eps(6) + shp(1,j)*vl(3,j) + shp(3,j)*vl(1,j)
  end do ! j

  !     Modify if mixed

  if(mixed) then

     !       Mixed volume change

     theta  = 0.0d0
     do j = 1,nel
        theta  = theta  + shpbar(1,j)*vl(1,j) &
             + shpbar(2,j)*vl(2,j) &
             + shpbar(3,j)*vl(3,j)
     end do

     !       Mixed strains

     dtheta = 0.3333333333333333d0*(theta - eps(1) - eps(2) - eps(3))
     eps(1) = eps(1) + dtheta
     eps(2) = eps(2) + dtheta
     eps(3) = eps(3) + dtheta

  endif

  !     Compute stress modification due to Rayleigh damping

  do j = 1,6
     eps(j) = eps(j)*d(78)
     do i = 1,6
        sig(i) = sig(i) + dr(i,j)*eps(j)
     end do ! i
  end do ! j

end subroutine rays3d

subroutine resid3d(xsj,shp,sig,d,vl,al,p,ndf,l)

  !     Purpose: 3-D residual routine

  implicit  none

  include  'eldata.h'
  include  'elplot.h'
  include  'eltran.h'
  include  'prld1.h'

  integer ::   ndf,l, j,k
  real*8 ::    xsj

  real*8 ::    b1,b2,b3,rr
  real*8 ::    aj1,aj2,aj3,aj0,lfac,cfac

  real*8 ::    d(*),vl(ndf,*),al(ndf,*),p(ndf,*)
  real*8 ::    shp(4,*),sig(*),ac(3),vc(3)

  save

  !     Compute stress-divergence vector (p)

  if(int(d(74)) > 0) then
     b1 = d(11) + prldv(int(d(74)))*d(71)
  else
     b1 = d(11)*dm
  endif

  if(int(d(75)) > 0) then
     b2 = d(12) + prldv(int(d(75)))*d(72)
  else
     b2 = d(12)*dm
  endif

  if(int(d(76)) > 0) then
     b3 = d(13) + prldv(int(d(76)))*d(73)
  else
     b3 = d(13)*dm
  endif

  rr   = d(4)
  if(d(7) >= 0.0d0) then
     cfac = d(7)
     lfac = 1.d0 - cfac
  else
     cfac = 0.0d0
     lfac = 0.0d0
  endif

  !     Store time history plot data for element

  k = 6*(l-1)
  do j = 1,6
     tt(j+k) = sig(j)
  end do ! j

  !     Compute accelerations

  ac(1) = 0.0d0
  ac(2) = 0.0d0
  ac(3) = 0.0d0
  do j = 1,nel
     ac(1) = ac(1) + shp(4,j)*al(1,j)
     ac(2) = ac(2) + shp(4,j)*al(2,j)
     ac(3) = ac(3) + shp(4,j)*al(3,j)
  end do ! j
  ac(1)   = rr*ac(1)*cfac
  ac(2)   = rr*ac(2)*cfac
  ac(3)   = rr*ac(3)*cfac

  !     For Rayleigh Mass Damping: Compute velocity

  if(d(77) /= 0.0d0) then
     vc(1) = 0.0d0
     vc(2) = 0.0d0
     vc(3) = 0.0d0
     do j = 1,nel
        vc(1) = vc(1) + shp(4,j)*vl(1,j)
        vc(2) = vc(2) + shp(4,j)*vl(2,j)
        vc(3) = vc(3) + shp(4,j)*vl(3,j)
     end do ! j
     vc(1)   = vc(1)*cfac
     vc(2)   = vc(2)*cfac
     vc(3)   = vc(3)*cfac

     do j = 1,nel
        aj0    = shp(4,j)*xsj*rr*d(77)
        p(1,j) = p(1,j) - (vc(1) + lfac*vl(1,j))*aj0
        p(2,j) = p(2,j) - (vc(2) + lfac*vl(2,j))*aj0
        p(3,j) = p(3,j) - (vc(3) + lfac*vl(3,j))*aj0
     end do ! j

  endif

  !     Loop over rows

  do j = 1,nel
     aj1 = shp(1,j)*xsj
     aj2 = shp(2,j)*xsj
     aj3 = shp(3,j)*xsj
     aj0 = lfac*rr

     !       Compute gravity, thermal, inertia, and stress contributions

     p(1,j) = p(1,j) + (b1 - ac(1) - aj0*al(1,j))*shp(4,j)*xsj &
          - aj1*sig(1)  - aj2*sig(4)  - aj3*sig(6)
     p(2,j) = p(2,j) + (b2 - ac(2) - aj0*al(2,j))*shp(4,j)*xsj &
          - aj1*sig(4)  - aj2*sig(2)  - aj3*sig(5)
     p(3,j) = p(3,j) + (b3 - ac(3) - aj0*al(3,j))*shp(4,j)*xsj &
          - aj1*sig(6)  - aj2*sig(5)  - aj3*sig(3)

  end do ! j

end subroutine resid3d

subroutine sbodyf(d, body)

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Compute body force values

  !     Inputs:
  !       d(*)    - Material parameters

  !     Outputs:
  !       body(*) - Body force intensities
  !-----[--+---------+---------+---------+---------+---------+---------+-]
  implicit   none

  include   'eldata.h'
  include   'prld1.h'

  integer ::    ii
  real*8 ::     d(*), body(3)

  !     Set body load levels

  do ii = 1,3
     if(int(d(73+ii)) > 0) then
        body(ii) = d(10+ii) + prldv(int(d(73+ii)))*d(70+ii)
     else
        body(ii) = d(10+ii)*dm
     endif
  end do ! ii

end subroutine sbodyf

subroutine slcn3d(sig,shp,xsj, p,s, lint,nel,nes)

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Project element variables to nodes

  !      Inputs:
  !        sig(6,*)     - Stresses at quadrature points
  !        shp(4,nes,*) - Shape functions at quadrature points
  !        xsj(*)       - Volume element at quadrature points
  !        lint         - Number of quadrature points
  !        nel          - Number nodes on element
  !        nes          - Dimension of shape function array

  !      Outputs:
  !        p(nen)       - Weights for 'lumped' projection
  !        s(nen,*)     - Integral of variables
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'cdata.h'
  include  'strnum.h'

  integer ::   ii, jj, l, lint, nel,nes
  real*8 ::    p(*),s(nen,*), xsj(*),shp(4,nes,*),sig(6,*), xj

  save

  !     Initialize the arrays

  do ii = 1,nel
     p(ii)    = 0.0d0
     do jj = 1,6
        s(ii,jj) = 0.0d0
     end do ! jj
  end do ! ii

  !     Compute projections: int ( sig * shp(i) * darea )

  do l = 1,lint
     do ii = 1,nel
        xj    = xsj(l)*shp(4,ii,l)
        p(ii) = p(ii)   + xj
        do jj = 1,6
           s(ii,jj) = s(ii,jj) + sig(jj,l)*xj
        end do ! jj
     end do ! ii
  end do ! l

end subroutine slcn3d

subroutine sld3d1(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

  ! ____________________________________________________________________c
  !     3-D linear elastic displacment element for feap

  !     Output records:

  !     Prints in element: sig-11, sig-22, sig-33, sig-12, sig-23, sig-31
  !                        eps-11, eps-22, eps-33, eps-12, eps-23, eps-31

  !     Prints at nodes:   1=sig-11, 2=sig-22, 3=sig-33,
  !                        4=sig-12  5=sig-23, 6=sig-31

  ! ____________________________________________________________________c

  implicit  none

  include  'bdata.h'
  include  'cdata.h'
  include  'eldata.h'
  include  'eltran.h'
  include  'hdata.h'
  include  'iofile.h'
  include  'prld1.h'
  include  'prstrs.h'
  include  'rdata.h'
  include  'comblk.h'

  integer ::   i,j,l,nn, i1,j1
  integer ::   ndf,ndm,nst,isw
  integer ::   lint,nhv,tdof, istrt

  real*8 ::    dv, dvm, xn, yn, zn, ta, rr, lfac, cfac, am, mass
  real*8 ::    a11, a12, a13, a21, a22, a23, a31, a32, a33
  real*8 ::    a41, a42, a43, a51, a52, a53, a61, a62, a63
  real*8 ::    b1 , b2 , b3
  real*8 ::    shp(4,8,8),sg(4,8),sig(8),eps(9,3),dd(6,6,5), xsj(8)
  real*8 ::    th(8),sv(5,4)

  integer ::   ix(*)
  real*8 ::    d(*),xl(ndm,*),ul(ndf,nen,*),tl(*),s(nst,*),p(nst)

  save

  !     Set nodal temperatures: Can be specified or computed

  if(isw > 1) then
     tdof = nint(d(19))
     if(tdof <= 0) then
        do i = 1,nel ! {
           th(i) = tl(i)
        end do ! i     }
     else
        do i = 1,nel ! {
           th(i) = ul(tdof,i,1)
        end do ! i     }
     endif
  endif

  !     Transfer to correct processor

  go to (1,1,3,3,1,3,1,3,1,1,1,3), isw

  !     Return

1 return

  !     Compute element tangent array

3 l = 2

  !     Set number of history terms

  nhv   = nint(d(15))
  istrt = nint(d(84))

  if(mod(isw,3) == 0) then

     !       Set body loading factors

     if(int(d(74)) > 0) then
        b1 = d(11) + prldv(int(d(74)))*d(71)
     else
        b1 = d(11)*dm
     endif
     if(int(d(75)) > 0) then
        b2 = d(12) + prldv(int(d(75)))*d(72)
     else
        b2 = d(12)*dm
     endif
     if(int(d(76)) > 0) then
        b3 = d(13) + prldv(int(d(76)))*d(73)
     else
        b3 = d(13)*dm
     endif

     !       Set mass factors

     rr   = d(4)
     if(d(7) >= 0.0d0) then
        cfac = d(7)
        lfac = 1.d0 - cfac
     else
        cfac = 0.0d0
        lfac = 0.0d0
     endif

     !       Get quadrature information

     if(nel == 4) then
        call tint3d(l,lint,sv)
     else
        call int3d(l,lint,sg)
     endif

     nn = 0
     do l = 1,lint
        if(nel == 4) then
           call tetshp(sv(1,l),xl,ndm,xsj(l),shp(1,1,l))
           dv = xsj(l)*sv(5,l)
        else
           call shp3d(sg(1,l),xsj(l),shp(1,1,l),xl,ndm)
           dv = xsj(l)*sg(4,l)
        endif

        !         Compute strain at point

        call strn3d(d,ul,th,shp(1,1,l),ndf,nel, eps,ta)

        !         Compute stress at point

        call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt, &
             dd,sig,isw)

        !         Residual computations

        if(isw == 3 .or. isw == 6) then

           !           Add stiffness part of Rayleigh damping to stress

           if(d(78) /= 0.0d0) then
              call rays3d(d,shp(1,1,l),shp(1,1,l),sig,dd,ul(1,1,4), &
                   ndf,nel, .false. )
           endif

           !           Form residual

           call resid3d(dv,shp(1,1,l),sig,d, &
                ul(1,1,4),ul(1,1,5),p,ndf,l)
        endif

        !         Stiffness computations

        if(isw == 3) then

           dvm   = rr*(ctan(3) + d(77)*ctan(2))*dv
           dv    =    (ctan(1) + d(78)*ctan(2))*dv

           j1 = 1
           do j = 1,nel

              !             Compute d * b matrix = a

              xn  = shp(1,j,l)*dv
              yn  = shp(2,j,l)*dv
              zn  = shp(3,j,l)*dv
              a11 = dd(1,1,1)*xn + dd(1,4,1)*yn + dd(1,6,1)*zn
              a21 = dd(2,1,1)*xn + dd(2,4,1)*yn + dd(2,6,1)*zn
              a31 = dd(3,1,1)*xn + dd(3,4,1)*yn + dd(3,6,1)*zn
              a41 = dd(4,1,1)*xn + dd(4,4,1)*yn + dd(4,6,1)*zn
              a51 = dd(5,1,1)*xn + dd(5,4,1)*yn + dd(5,6,1)*zn
              a61 = dd(6,1,1)*xn + dd(6,4,1)*yn + dd(6,6,1)*zn
              a12 = dd(1,2,1)*yn + dd(1,4,1)*xn + dd(1,5,1)*zn
              a22 = dd(2,2,1)*yn + dd(2,4,1)*xn + dd(2,5,1)*zn
              a32 = dd(3,2,1)*yn + dd(3,4,1)*xn + dd(3,5,1)*zn
              a42 = dd(4,2,1)*yn + dd(4,4,1)*xn + dd(4,5,1)*zn
              a52 = dd(5,2,1)*yn + dd(5,4,1)*xn + dd(5,5,1)*zn
              a62 = dd(6,2,1)*yn + dd(6,4,1)*xn + dd(6,5,1)*zn
              a13 = dd(1,3,1)*zn + dd(1,5,1)*yn + dd(1,6,1)*xn
              a23 = dd(2,3,1)*zn + dd(2,5,1)*yn + dd(2,6,1)*xn
              a33 = dd(3,3,1)*zn + dd(3,5,1)*yn + dd(3,6,1)*xn
              a43 = dd(4,3,1)*zn + dd(4,5,1)*yn + dd(4,6,1)*xn
              a53 = dd(5,3,1)*zn + dd(5,5,1)*yn + dd(5,6,1)*xn
              a63 = dd(6,3,1)*zn + dd(6,5,1)*yn + dd(6,6,1)*xn

              !             Add diagonal mass effects

              am           = shp(4,j,l)*dvm
              s(j1  ,j1  ) = s(j1  ,j1  ) + am*lfac
              s(j1+1,j1+1) = s(j1+1,j1+1) + am*lfac
              s(j1+2,j1+2) = s(j1+2,j1+2) + am*lfac

              if(isw == 3) then
                 i1 = 1
                 do i = 1,j

                    !                 Compute consistent mass matrix

                    xn   = shp(1,i,l)
                    yn   = shp(2,i,l)
                    zn   = shp(3,i,l)
                    mass = shp(4,i,l)*am*cfac

                    s(i1  ,j1  ) = s(i1  ,j1  ) + xn*a11 + yn*a41 + zn*a61 &
                         + mass
                    s(i1  ,j1+1) = s(i1  ,j1+1) + xn*a12 + yn*a42 + zn*a62
                    s(i1  ,j1+2) = s(i1  ,j1+2) + xn*a13 + yn*a43 + zn*a63
                    s(i1+1,j1  ) = s(i1+1,j1  ) + yn*a21 + xn*a41 + zn*a51
                    s(i1+1,j1+1) = s(i1+1,j1+1) + yn*a22 + xn*a42 + zn*a52 &
                         + mass
                    s(i1+1,j1+2) = s(i1+1,j1+2) + yn*a23 + xn*a43 + zn*a53
                    s(i1+2,j1  ) = s(i1+2,j1  ) + zn*a31 + yn*a51 + xn*a61
                    s(i1+2,j1+1) = s(i1+2,j1+1) + zn*a32 + yn*a52 + xn*a62
                    s(i1+2,j1+2) = s(i1+2,j1+2) + zn*a33 + yn*a53 + xn*a63 &
                         + mass
                    i1 = i1 + ndf
                 end do
              endif
              j1 = j1 + ndf
           end do
        endif
        nn = nn + nhv
     end do

     !       Construct symmetric part

     if(isw == 3) then
        do i = 2,ndf*nel
           do j = 1,i
              s(i,j) = s(j,i)
           end do
        end do
     endif
  endif

  !     Compute and output element variables

  if(isw == 4) then

     if(nel == 4) then
        call tint3d(l,lint,sv)
     else
        call int3d(l,lint,sg)
     endif

     !       Set initial counter for history terms in stress/strain

     nn = 0
     do l = 1,lint
        if(nel == 4) then
           call tetshp(sv(1,l),xl,ndm,xsj(l),shp)
        else
           call shp3d(sg(1,l),xsj(l),shp,xl,ndm)
        endif

        !         Compute strain at point

        call strn3d(d,ul,th,shp,ndf,nel, eps,ta)

        !         Compute stress at point

        call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt, &
             dd,sig,isw)

        !         Compute coordinates

        xn = 0.0
        yn = 0.0
        zn = 0.0
        do j = 1,nel
           xn = xn + shp(4,j,1)*xl(1,j)
           yn = yn + shp(4,j,1)*xl(2,j)
           zn = zn + shp(4,j,1)*xl(3,j)
        end do

        !         Compute principal stress values

        mct = mct - 3
        if(mct <= 0) then
           write(iow,2010) o,head
           if(ior < 0) write(*,2010) o,head
           mct = 50
        endif
        write(iow,2011) n,xn,(sig(i),i=1,6),ma,yn,(eps(i,1),i=1,6)
        if(ior < 0) then
           write(*,2011) n,xn,(sig(i),i=1,6),ma,yn,(eps(i,1),i=1,6)
        end if
        nn = nn + nhv
     end do

     !     Plot stress values

  elseif(isw == 8) then
     call stcn3d(ix,d,xl,th,ul,shp,hr(nph),hr(nph+numnp), &
          ndf,ndm,nel,numnp,nhv,istrt)
  endif
  return

  !     Formats

2010 format(a1,20a4//5x,'Element Stresses'//' Elmt 1-coord', &
       &     2x,'11-stress  22-stress  33-stress  12-stress', &
       &     2x,'23-stress  31-stress'/' matl 2-coord  11-strain', &
       &     2x,'22-strain  33-strain  12-strain  23-strain', &
       &     2x,'31-strain'/39(' -'))

2011 format(i4,0p1f9.3,1p6e11.3/i4,0p1f9.3,1p6e11.3/)

end subroutine sld3d1

subroutine stcn3d(ix,d,xl,th,ul,shp,dt,st, &
     ndf,ndm,nel,numnp,nhv,istrt)

  ! ____________________________________________________________________c
  implicit  none

  include  'hdata.h'
  include  'comblk.h'

  integer ::   ndf,ndm,nel,numnp,nhv,istrt
  integer ::   ii, l, ll, lint, nn

  real*8 ::    xsj, xj, ta

  integer ::   ix(*)
  real*8 ::    dt(numnp),st(numnp,*),xl(ndm,*),th(*),shp(4,8)
  real*8 ::    d(*),sig(8),eps(9,3),dd(6,6,5),ul(ndf,*),sg(4,8)
  real*8 ::    sv(5,4)

  save

  !     Compute stress projections to nodes

  l    = 2
  if(nel == 4) then
     call tint3d(l,lint,sv)
  else
     call int3d(l,lint,sg)
  endif

  !     Set initial counter for history terms in stress/strain

  nn   = 0
  do l = 1,lint
     if(nel == 4) then
        call tetshp(sv(1,l),xl,ndm,xsj,shp)
        xsj = xsj*sv(5,l)
     else
        call shp3d(sg(1,l),xsj,shp,xl,ndm)
        xsj = xsj*sg(4,l)
     endif

     !       Compute strain at point

     call strn3d(d,ul,th,shp,ndf,nel, eps,ta)

     !       Compute stress at point

     call modlsd(d,ta,eps,hr(nh1+nn),hr(nh2+nn),nhv,istrt, &
          dd,sig, 8)

     !       Compute projections: int ( sig * shp(i) * darea )

     do ii = 1,nel
        ll = abs(ix(ii))
        if(ll /= 0) then
           xj     = xsj*shp(4,ii)
           dt(ll) = dt(ll) + xj
           st(ll,1) = st(ll,1) + sig(1)*xj
           st(ll,2) = st(ll,2) + sig(2)*xj
           st(ll,3) = st(ll,3) + sig(3)*xj
           st(ll,4) = st(ll,4) + sig(4)*xj
           st(ll,5) = st(ll,5) + sig(5)*xj
           st(ll,6) = st(ll,6) + sig(6)*xj
        endif
     end do
     nn = nn + nhv
  end do

end subroutine stcn3d

subroutine strn3d(d,ul,th,shp,ndf,nel, eps,ta)

  ! ____________________________________________________________________c
  !     Three dimensional strain calculations

  implicit  none

  include  'cdata.h'

  integer ::   ndf,nel, j
  real*8 ::    ta

  real*8 ::    d(*),ul(ndf,nen,*),th(*),shp(4,*)
  real*8 ::    eps(9,*)

  save

  !     Compute stress and strain at point

  do j = 1,9
     eps(j,1) = 0.0d0
     eps(j,2) = 0.0d0
     eps(j,3) = 0.0d0
  end do ! j

  !     Compute temperature and coordinates

  ta = -d(9)
  do j = 1,nel
     ta     = ta         + shp(4,j)*th(j)
     eps(1,1) = eps(1,1) + shp(1,j)*ul(1,j,1)
     eps(2,1) = eps(2,1) + shp(2,j)*ul(2,j,1)
     eps(3,1) = eps(3,1) + shp(3,j)*ul(3,j,1)
     eps(4,1) = eps(4,1) + shp(1,j)*ul(2,j,1) &
          + shp(2,j)*ul(1,j,1)
     eps(5,1) = eps(5,1) + shp(2,j)*ul(3,j,1) &
          + shp(3,j)*ul(2,j,1)
     eps(6,1) = eps(6,1) + shp(3,j)*ul(1,j,1) &
          + shp(1,j)*ul(3,j,1)
     eps(1,3) = eps(1,3) + shp(1,j)*ul(1,j,2)
     eps(2,3) = eps(2,3) + shp(2,j)*ul(2,j,2)
     eps(3,3) = eps(3,3) + shp(3,j)*ul(3,j,2)
     eps(4,3) = eps(4,3) + shp(1,j)*ul(2,j,2) + shp(2,j)*ul(1,j,2)
     eps(5,3) = eps(5,3) + shp(2,j)*ul(3,j,2) + shp(3,j)*ul(2,j,2)
     eps(6,3) = eps(6,3) + shp(3,j)*ul(1,j,2) + shp(1,j)*ul(3,j,2)
  end do
  do j = 1,6
     eps(j,2) = eps(j,1) - eps(j,3)
  end do

end subroutine strn3d
