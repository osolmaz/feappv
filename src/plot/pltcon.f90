! Id:$
subroutine pltcon(x,ie,ix,ip,u, &
     nie,ndm,ndf,nen1,ic,mc,lc,mmc,label)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Plot of mesh contours: With inter-element smoothing

  !      Inputs:
  !         x(ndm,*)  - Nodal coordinates of mesh
  !         ie(nie,*) - Assembly data for material sets
  !         ix(nen1,*)- Element nodal connections
  !         ip(*)     - Sorted element order
  !         u(*)      - Solution state
  !         nie       - Dimension of ie array
  !         ndm       - Dimension of x array
  !         ndf       - Number dof/node
  !         nen1      - Dimension of ix array
  !         ic        - Component number to plot
  !         mc        - Number of contour lines: < 0 fill; > 0 line
  !         lc        - Dimensioning information on component to plot
  !         mmc       - Type of plot
  !         label     - Flag, put labels on plots if true

  !      Outputs:
  !         none      - Plot outputs to screen/file
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'cdata.h'
  include  'fdata.h'
  include  'iofile.h'
  include  'pbody.h'
  include  'pdata1.h'
  include  'pdata2.h'
  include  'pdata4.h'
  include  'pdatri.h'
  include  'prange.h'
  include  'prmptd.h'
  include  'psdat1.h'
  include  'rpdata.h'

  character y*1
  logical ::   tvc(9,9),vflg,errck,cont,pinput,label,labl
  integer ::   nie, ndm, ndf, nen1, ic, mc, lc, mmc, icolor
  integer ::   i, j, n, ma, nc, nnc, nerr, nlabi
  integer ::   iu, iutot, ns, ii, ne, iplt(30)
  real*8 ::    dx1, vmx, vmn

  integer ::   ie(nie,*),ix(nen1,*),ip(*)
  real*8 ::    xl(3,29),x(ndm,*),u(*),v(29),vc(12), vlu(2)

  save

  !     Contour plot routine for elements: lines if mc > 0;
  !                                        fills if mc < 0

  cont = .true. 
  labl = label
  call pzerol ( tvc , .true. , 81 )
1 if(mc > 0) then
     nc    = max(1,min(mc,12))
     nlabi = 0
     dx1   = .024d0/scale
     vflg  = ipb == 0
     nerr  = 0
11   if(ior < 0) write(*,2001) nc
     nnc = min(8,nc)
     if (prompt .and. .not. defalt) then
        if(ior < 0) write(*,2009)
        errck = pinput(vc,nnc)
        nerr  = nerr+1
        if(nerr > 5) then
           if(ior < 0) return
           call plstop
        endif
        if(errck) go to 11
     else
        vc(1) = 0.0d0
        vc(2) = 0.0d0
     endif
     if(nc > 1 .and. vc(1) == 0.0d0 .and. vc(2) == 0.0d0) then
        vc(1)   = rmn +    (rmx - rmn)/(nc+1)
        vc(nc)  = rmn + nc*(rmx - rmn)/(nc+1)
        do i = 2,nc-1
           vc(i) = vc(1) + (vc(nc)-vc(1))*(i-1)/(nc-1)
        end do
     else
        if(nc > 8) then
           nnc = min(12,nc) - 8
           if(ior < 0) write(*,2009)
           errck = pinput(vc(9),nnc)
           if(errck) go to 11
        endif
     endif
     if (prompt) then
        if(pfr) write(iow,2000) (vc(i),i=1,nc)
        if(ior < 0 .and. .not. defalt ) then
           write(*,2000) (vc(i),i=1,nc)
        endif
     endif

     !       Input label and color for first contour

13   if(prompt .and. .not. defalt) then
        if (ior < 0) write(*,2002)
        errck = pinput(vlu(1),1)
        if(errck) go to 13
        nlabi = max(1,min(int(vlu(1)),12)) - 1
        if(nlabi+nc > 12) then
           if(ior < 0) write(*,2007)
           nlabi = 12 - nc
        endif
     else
        nlabi = 0
     endif

     !     Inputs for filled plots

  else
     cont  = .false. 
     nc    = 6
     if(ipb >= 0) then
15      if(rangfl) then
           vc(1) = rangmn
           vc(2) = rangmx
        elseif(prompt .and. .not. defalt) then
           if(ior < 0) write(*,2008) rmn,rmx
           errck = pinput(vc,2)
           if(errck) go to 15
        else
           vc(1) = 0.0d0
           vc(2) = 0.0d0
        endif
        if(vc(1) == vc(2)) then
           vc(1) = rmn +      (rmx - rmn)/7.0d0
           vc(6) = rmn + 6.d0*(rmx - rmn)/7.0d0
        else
           vc(6) = vc(2)
        endif
        do i = 2,5
           vc(i) = vc(1) + (vc(6)-vc(1))*(i-1)/5.0d0
        end do
        if(prompt) then
           if(pfr) write(iow,2000) (vc(i),i=1,nc)
           if(ior < 0 .and. .not. defalt ) then
              write(*,2000) (vc(i),i=1,nc)
           endif
        endif
     endif
  endif

  !     If interactive, offer chance to change inputs

  if(ior < 0 .and. prompt .and. .not. defalt ) then
     write(*,2006)
20   read (*,1000,err=21,end=22) y
     goto  23
21   call  errclr ('PLTCON')
     goto  20
22   call  endclr ('PLTCON',y)
23   if(y == 'c' .or. y == 'C') return
     if(y /= 'y' .and. y /= 'Y') go to 1
  endif

  !     Find max/min of plot variable

  call plopen

  j   = ic
  xmx = x(1,1)
  ymx = x(2,1)
  xmn = x(1,1)
  ymn = x(2,1)
  vmn = u(j)
  vmx = u(j)
  do i = 1,numnp
     xmx = max(x(1,i),xmx)
     ymx = max(x(2,i),ymx)
     xmn = min(x(1,i),xmn)
     ymn = min(x(2,i),ymn)
     vmn = min(vmn,u(j))
     vmx = max(vmx,u(j))
     j   = j + ndf
  end do
  if(xmx /= xmn) xmx = 8.2d0/(xmx-xmn)
  if(ymx /= ymn) ymx = 8.2d0/(ymx-ymn)
  if(vmn == vmx) then
     write(iow,2005)
     if(ior < 0) write(*,2005)
     return
  endif

  !     Open plot and loop through elements

  call pzero(xl,3*max(4,nen))
  ic = max(1,min(ic,ndf))
  psmx  = vmn - 1.
  psmn  = vmx + 1.
  do ne = 1,nfac

     !       Get plot order for each element

     n  = ip(ne)

     !       Plot active regions: material number: maplt; all if maplt = 0;

     ma = ix(nen1,n)
     if(ix(nen1-1,n) >= 0 .and. &
          (maplt == 0 .or. ma == maplt)) then
        ma = ie(nie-1,ma)
        call pltord(ix(1,n),ma, iu,iplt)
        iutot = iu

        !         Check if element is in window and set values of vl and vu

        vlu(1) = vmx
        vlu(2) = vmn
        ns     = 0
        do i = 1,iu
           ii = ix(iplt(i),n)
           if(ii > 0) then
              ns       = ns + 1
              xl(1,ns) = x(1,ii)
              xl(2,ns) = x(2,ii)
              if(ndm >= 3) xl(3,ns) = x(3,ii)
              j      = ndf*(ii-1) + ic
              v(ns)  = u(j)
              vlu(1) = min(vlu(1),v(ns))
              vlu(2) = max(vlu(2),v(ns))

              !             Plot min/max for graphics

              if(psmn > v(ns)) then
                 psmn    = v(ns)
                 xpsn(1) = xl(1,ns)
                 xpsn(2) = xl(2,ns)
                 xpsn(3) = xl(3,ns)
              endif
              if(psmx < v(ns)) then
                 psmx    = v(ns)
                 xpsx(1) = xl(1,ns)
                 xpsx(2) = xl(2,ns)
                 xpsx(3) = xl(3,ns)
              endif
           endif
        end do
        if(ns > 3) then
           if(nen >= 9 .and. iutot == 9) then
              if(ix(9,n) > 0) v(29) = u(ndf*(ix(9,n)-1)+ic)
           elseif(nen >= 7 .and. iutot == 7) then
              if(ix(7,n) > 0) v(29) = u(ndf*(ix(7,n)-1)+ic)
           endif
           call pltris(ic,nc,n,ns,iutot,ndm,ndf,nen,nen1,nlabi, &
                icolor,ix,x,xl,v,vc,dx1,vlu(1),vlu(2),tvc, &
                cont,vflg)
        else
           call pltlfl(ns,xl,v,vc,nc)
        endif
     endif

  end do

  !     Put on labels

  if(labl) then
     if(cont) then
        call pltctx(vc,lc,nlabi,nc,mmc)
     else
        call pltftx(vc,-mc,mmc)
     endif
     labl = .false. 
  end if

  !     Formats

1000 format(a)
2000 format('   ------ Contour Values for Plot ------'/(3x,5e15.6))
2001 format(' Input',i3,' Contour Values for Plot - 8 Values/Line')
2002 format(' Input number for first contour label > ',$)
2005 format(' ** ERROR ** No plot - zero difference in values')
2006 format(' Input values correct? (y or n, c = cancel) > ',$)
2007 format(' ** WARNING ** Initial label reset to fit screen')
2008 format(' Input Min/Max (Default:',1p,e9.2,'/',1p,e9.2,'): >',$)
2009 format(3x,'>',$)

end subroutine pltcon
