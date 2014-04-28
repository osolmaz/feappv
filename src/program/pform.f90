! Id:$
subroutine pform(ul,xl,tl,ld,p,s,ie,d,id,x,ix,f,t,jp, &
    u,ud,b,a,al,ndd,nie,ndf,ndm,nen1,nst,aufl,bfl,dfl, &
    isw,nn1,nn2,nn3)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Compute element arrays and assemble global arrays

  !      Inputs:
  !         ie(nie,*)   - Assembly information for material set
  !         d(ndd,*)    - Material set parameters
  !         id(ndf,*)   - Equation numbers for each active dof
  !         x(ndm,*)    - Nodal coordinates of mesh
  !         ix(nen1,*)  - Element nodal connections of mesh
  !         f(ndf,*,2)  - Nodal force and displacement values
  !         t(*)        - Nodal temperature values
  !         jp(*)       - Pointer array for row/columns of tangent
  !         u(*)        - Nodal solution values
  !         ud(*)       - Nodal rate values
  !         ndd         - Dimension for d array
  !         nie         - Dimension for ie array
  !         ndf         - Number dof/node
  !         ndm         - Spatial dimension of mesh
  !         nen1        - Dimension for ix array
  !         nst         - Dimension for element array
  !         aufl        - Flag, assemble coefficient array if true
  !         bfl         - Flag, assemble vector if true
  !         dfl         - Flag, assemble reactions if true
  !         isw         - Switch to control quantity computed
  !         nn1         - First element number to process
  !         nn2         - Last element number to process
  !         nn3         - Increment to nn1

  !      Scratch:
  !         ul(ndf,*)   - Element solution and rate values
  !         xl(ndm,*)   - Element nodal coordinates
  !         tl(*)       - Element nodal temperatures
  !         ld(*)       - Element local/global equation numbers
  !         p(*)        - Element vector
  !         s(nst,*)    - Element array

  !      Outputs:
  !         b(*)        - Global vector
  !         a(*)        - Global matrix, diagonal and upper part
  !         al(*)       - Global matrix, lower part
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  include  'cdata.h'
  include  'crotas.h'
  include  'ddata.h'
  include  'elcount.h'
  include  'eldata.h'
  include  'elplot.h'
  include  'eluser.h'
  include  'eqsym.h'
  include  'erotas.h'
  include  'fdata.h'
  include  'iofile.h'
  include  'hdata.h'
  include  'hdatam.h'
  include  'mdata.h'
  include  'modreg.h'
  include  'p_int.h'
  include  'pointer.h'
  include  'prld1.h'
  include  'prlod.h'
  include  'prstrs.h'
  include  'ptdat1.h'
  include  'ptdat2.h'
  include  'ptdat8.h'
  include  'rdata.h'
  include  'rdat0.h'
  include  'region.h'
  include  'tdata.h'
  include  'tdatb.h'
  include  'comblk.h'

  logical ::   aufl,bfl,dfl,efl
  integer ::   nrkn, nrcn, nrmn, ii, iid, ild, isw, jsw, ksw
  integer ::   i, j, k, jj, nn1, nn2, nn3, nst, nl1, nneq
  integer ::   numnp2, ndf, ndm, nrot, ndd, nie, nen1
  real*8 ::    un, dun, temp, prope

  integer ::   ld(*), ie(nie,*), id(ndf,*), ix(nen1,*), jp(*)
  real*8 ::    xl(ndm,*), p(*), s(nst,*), d(ndd,*), ul(nst,*)
  real*8 ::    x(ndm,*) ,f(ndf,numnp),u(ndf,*),ud(*),t(*),tl(*)
  real*8 ::    b(*), a(*), al(*)

  save

  !     Set element proportional loading value

  prope = theta(3)*(prop - propo) + propo

  !     Recover nh1, nh2, nh3 pointers

  nh1 = np(51)
  nh2 = np(52)
  nh3 = np(53)

  !     Set program and user material count parameters

  do i = 1,10
  nomats(1,i) = 0
  nomats(2,i) = 0
  unmats(1,i) = 0
  unmats(2,i) = 0
  end do ! i

  !     Set up local arrays before calling element library

  iel = 0
  efl = .false. 
  if( .not. dfl .and. isw == 6) efl = .true. 
  if(bfl .and. isw == 3)      efl = .true. 

  if(isw == 19) then
    if(bfl) efl = .true. 
    jsw = 5
    ksw = 5
  else
    jsw = isw
    ksw = 3
  endif

  nl1    = ndf*nen + 1
  numnp2 = numnp + numnp
  nneq   = numnp*ndf
  nrkn   = nrk*nneq - nneq
  nrcn   = nrc*nneq - nneq
  nrmn   = nrm*nneq - nneq

  !     Loop over active elements

  do n = nn1,nn2,nn3

  !       Check for active regions

  if((nreg < 0 .and. ix(nen1-1,n) >= 0) &
    .or. (abs(ix(nen1-1,n)) == nreg)) then

  !        Set up local arrays

  do ma = 1, nummat

  if(ie(nie-2,ma) == ix(nen1,n)) then

    !           Compute address and offset for history variables

    fp(1) = np(50) + ix(nen+1,n) + ie(nie-3,ma)
    fp(2) = np(50) + ix(nen+2,n) + ie(nie-3,ma)
    fp(3) = np(50) + ix(nen+3,n) + ie(nie-4,ma)

    !           If history variables exist move into nh1,nh2

    if(ie(nie,ma) > 0) then
      do i = 0,ie(nie,ma)-1
      hr(nh1+i) = hr(fp(1)+i)
      hr(nh2+i) = hr(fp(2)+i)
      end do
    endif

    !           If Element variables exist move into nh3

    if(ie(nie-5,ma) > 0) then
      do i = 0,ie(nie-5,ma)-1
      hr(nh3+i) = hr(fp(3)+i)
      end do
    endif

    if(ie(nie-1,ma) /= iel) mct = 0
    iel   = ie(nie-1,ma)
    rotyp = ie(nie-6,ma)

    !           Zero array used to store local displ, veloc, and accel

    do i = 1,nst
    ld(i) = 0
    do j = 1,7
    ul(i,j) = 0.0d0
    end do
    end do

    !           Zero array used to store local tl and coordinates

    do i = 1,nen
    tl(i) = 0.0d0
    do j = 1,ndm
    xl(j,i) = 0.0d0
    end do
    end do
    un   = 0.0d0
    dun  = 0.0d0

    !           Set up local nodal coord rotation array for sloping b.c.

    call pangl(ix(1,n),nen,hr(np(46)),hr(np(45)),nrot)
    do i = 1,nen

    !             Set up localized solution parameters

    ii = ix(i,n)
    if(ii > 0) then
      iid = ii*ndf - ndf
      ild =  i*ndf - ndf
      nel = i
      tl(i) = t(ii)
      do j = 1,ndm
      xl(j,i) = x(j,ii)
      end do
      do j = 1,ndf
      jj = ie(j,ma)
      if(jj > 0) then

        !                   Set solution, total increment, last increment

        ul(j+ild,1) = u(jj,ii)
        ul(j+ild,2) = u(jj,ii+numnp)
        ul(j+ild,3) = u(jj,ii+numnp2)

        !                   Set dynamics solutions

        if(fl(9)) then
          k = iid+jj
          if(nrk > 0) then
            ul(j+ild,1) = ud(nrkn+k)
          endif

          if(jsw == 13) then
            ul(j+ild,1) = u(jj,ii)
            ul(j+ild,4) = ud(k)
          else
            if(nrc > 0) ul(j+ild,4) = ud(nrcn+k)
            if(nrm > 0) ul(j+ild,5) = ud(nrmn+k)
          endif

          !                   Set acceleration for specified shift

        elseif(shflg) then
          ul(j+ild,5) = -shift*ul(j+ild,1)
        endif

        un          = max( un,abs(u(jj,ii)))

        !                   Set increment for specified boundary values

        if( id(jj,ii) <= 0) then
          ul(j+ild,7) = (f(jj,ii) - u(jj,ii))
          dun         = max(dun,abs(ul(j+ild,7)))
        endif

        !                   Set k for reactions

        if(dfl) then
          k = iid + jj

          !                   Set k for compressed assembly

        else
          k = id(jj,ii)
        endif

        !                 Reset k for inactive equation

      else
        k = 0
      endif

      !                 Form assembly array

      ld(j+ild) = k

      end do
    endif
    end do

    !           Form element array - rotate parameters if necessary

    if(nrot > 0) then
      if(iel > 0) then
        call ptrans(ia(1,iel),hr(np(46)),ul,p,s, &
          nel,ndf,nst,1)
        if(ir(1,iel) /= 0) then
          call ptrans(ir(1,iel),hr(np(46)),ul,p,s, &
            nel,ndf,nst,1)
        endif
      else
        call ptrans(ea(1,-iel),hr(np(46)),ul,p,s, &
          nel,ndf,nst,1)
        if(er(1,-iel) /= 0) then
          call ptrans(er(1,-iel),hr(np(46)),ul,p,s, &
            nel,ndf,nst,1)
        endif
      endif
    endif
    if(jsw == 8) then
      erav = hr(np(60)+n-1)
    else
      erav = 0.0d0
    endif
    dm = prope
    call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p, &
      ndf,ndm,nst,iel,jsw)

    !           Store time history plot data from element

    if(jsw == 6) then

      !             Standard element values

      do i = 1,nsplts
      if(ispl(1,i) == n) then
        jj = max(ispl(2,i),1)
        spl(i) = tt(jj)
      endif
      end do

      !             Standard user element values

      do i = 1,nuplts
      if(iupl(1,i) == n) then
        jj = max(iupl(2,i),1)
        upl(i) = ut(jj)
      endif
      end do

    endif

    !           Modify for rotated dof's

    if(nrot > 0) then
      if(iel > 0) then
        call ptrans(ia(1,iel),hr(np(46)),ul,p,s, &
          nel,ndf,nst,2)
        if(ir(1,iel) /= 0) then
          call ptrans(ir(1,iel),hr(np(46)),ul,p,s, &
            nel,ndf,nst,2)
        endif
      else
        call ptrans(ea(1,-iel),hr(np(46)),ul,p,s, &
          nel,ndf,nst,2)
        if(er(1,-iel) /= 0) then
          call ptrans(er(1,-iel),hr(np(46)),ul,p,s, &
            nel,ndf,nst,2)
        endif
      endif
    endif

    !           Add to total array

    if(aufl .or. bfl) then
      call dasble(s,p,ld,jp,nst,neqs,aufl,bfl, &
        b,al,a(neq+1),a)
    endif

    !           Position update terms 'nt1,nt2' from 'nh1,nh2' to save

    if(hflgu .and. ie(nie,ma) > 0) then
      do i = 0,ie(nie,ma)-1
      temp      = hr(fp(1)+i)
      hr(fp(1)+i) = hr(nh1+i)
      hr(nh1+i) = temp
      temp      = hr(fp(2)+i)
      hr(fp(2)+i) = hr(nh2+i)
      hr(nh2+i) = temp
      end do
    endif

    !           Position update terms 'nt3' from 'nh3' to save

    if(h3flgu .and. ie(nie-5,ma) > 0) then
      do i = 0,ie(nie-5,ma)-1
      hr(fp(3)+i) = hr(nh3+i)
      end do
    endif

    !           Modify for non-zero displacement boundary conditions

    if(efl .and. dun > 1.0d-7*un) then

      !             Get current element tangent matrix

      if ( .not. aufl) then
        dm = prop
        call elmlib(d(1,ma),ul,xl,ix(1,n),tl,s,p, &
          ndf,ndm,nst,iel,ksw)
        if(nrot > 0) then
          if(iel > 0) then
            call ptrans(ia(1,iel),hr(np(46)),ul,p,s, &
              nel,ndf,nst,2)
            if(ir(1,iel) /= 0) then
              call ptrans(ir(1,iel),hr(np(46)),ul,p,s, &
                nel,ndf,nst,2)
            endif
          else
            call ptrans(ea(1,-iel),hr(np(46)),ul,p,s, &
              nel,ndf,nst,2)
            if(er(1,-iel) /= 0) then
              call ptrans(er(1,-iel),hr(np(46)),ul,p,s, &
                nel,ndf,nst,2)
            endif
          endif
        endif
      end if

      !             Modify for displacements

      do i = 1,nst
      ul(i,7) = ul(i,7)*cc3
      end do
      call modify(b,ld,s,ul(1,7),nst)
    end if

  end if

  end do ! ma

end if ! regions

end do ! n

    end subroutine pform
