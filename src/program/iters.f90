! Id:$
subroutine iters(bkmax,isw)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose:    Controls solution by iterative schemes. Also sets
  !                  storage for compressed arrays for direct solution
  !                  by sparse solvers or in blocks with disk stores.

  !      Inputs:
  !         bkmax  - Maximum size of block for direct solution
  !         isw    - Switch: isw =  1 for TANGS
  !                          isw =  2 for CMASS
  !                          isw =  3 for DAMPS
  !                          isw = -1 for USER module

  !      Outputs:
  !         none   - Outputs stored in blank common and pointers.
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'allotd.h'
  include  'cdata.h'
  include  'comblk.h'
  include  'compac.h'
  include  'compas.h'
  include  'iofile.h'
  include  'ndata.h'
  include  'pointer.h'
  include  'psize.h'
  include  'sdata.h'
  include  'ssolve.h'

  logical ::   setvar,palloc
  integer ::   isw,len,kp,bkmax, iptc
  real*4 ::    tary(2), etime , tt

  save

  !     Compute sparse storage for non-zero matrix

  if(isw > 0) then
     setvar = palloc(81,'TEMP1', numnp*ndf, 1)
     call elcnt(numnp,numel,nen,nen1,mr(np(33)),mr(np(81)), .true. )
     call sumcnt(mr(np(81)),numnp*ndf,kp)

     setvar = palloc(82,'TEMP2', kp, 1)
     call pelcon(numel,nen,nen1,mr(np(33)),mr(np(81)),mr(np(82)), &
          kp,1)
  endif

  !     1. TANGENT Formation

  if(isw == 1) then

     !       Slot for user supplied sparse solver

     if(ittyp == -2) then
        iptc   = 1
        !         ubycol =
        !         udiag  =
        !         uall   =
        !         iptc   =
        !         nspo   =

        !       Program solvers with sparse assembly

     else
        kbycol = .true.     ! Store by columns
        kdiag  = .false. 
        kall   = .false. 
        iptc   =  neq
     endif

     setvar = palloc( 2,'OINUC', iptc , 1)
     if(np(3) /= 0) then
        setvar = palloc( 3,'OINUO',0, 1)
     endif
     setvar = palloc( 3,'OINUO',  1, 1)
     call compro(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)), &
          mr(np(81)),mr(np(82)),mr(np(3)),mr(np(2)), &
          kp,maxm-mmax,kbycol,kdiag,kall)
     setvar = palloc( 3,'OINUO', kp, 1)

     !       Delete temporary arrays

     setvar = palloc(82,'TEMP2', 0, 1)
     setvar = palloc(81,'TEMP1', 0, 1)

     !       Set storage for sparse stiffness array

     if(ittyp == -1) then

        kp  = max(kp+neq,bkmax)
        len = kp

        !       User supplied sparse solver location

     elseif(ittyp == -2) then

        !       Profile solver assembly

     else

        kp  = kp + neq
        len = 0

     endif

     setvar = palloc(1,'TANGS', kp+len, 2)

     na     = np(1)
     nnr    = kp
     nau    = na  + neq
     nal    = nau + len
     numcels= 0
     compfl = .true. 

     !     2. Consistent MASS Formation

  elseif(isw == 2) then
     mbycol = .true.       ! Store by columns
     mdiag  = .false. 
     mall   = .false. 
     setvar =  palloc( 10,'OINMC', neq  , 1)
     if(np(11) /= 0) then
        setvar = palloc( 11,'OINMO',0, 1)
     endif
     setvar = palloc( 11,'OINMO',  1, 1)
     call compro(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)), &
          mr(np(81)),mr(np(82)),mr(np(11)),mr(np(10)), &
          kp,maxm-mmax,mbycol,mdiag,mall)
     setvar = palloc( 11,'OINMO', kp, 1)

     !       Delete temporary arrays

     setvar = palloc(82,'TEMP2', 0, 1)
     setvar = palloc(81,'TEMP1', 0, 1)

     !       Allocate mass storage

     nnm = kp  + neq
     setvar = palloc(9,'CMASS', nnm, 2)

     !     3. Consistent DAMP Formation

  elseif(isw == 3) then
     cbycol = .true.       ! Store by columns
     cdiag  = .false. 
     call   = .false. 
     setvar =  palloc(18,'OINCC', neq  , 1)
     if(np(19) /= 0) then
        setvar = palloc(19,'OINCO',0, 1)
     endif
     setvar = palloc(19,'OINCO',  1, 1)
     call compro(numnp,nen,nen1,ndf,mr(np(33)),mr(np(31)), &
          mr(np(81)),mr(np(82)),mr(np(19)),mr(np(18)), &
          kp,maxm-mmax,cbycol,cdiag,call)
     setvar = palloc(19,'OINCO', kp, 1)

     !       Delete temporary arrays

     setvar = palloc(82,'TEMP2', 0, 1)
     setvar = palloc(81,'TEMP1', 0, 1)

     !       Allocate mass storage

     nnc = kp  + neq
     setvar = palloc(17,'DAMPS', nnc, 2)

  elseif (isw < 0) then

     call uiters(kp,isw)
     bkmax = kp

  endif

  !     Output solution properties

  tt = etime(tary)
  write(iow,2000) kp,tary
  if(ior < 0) then
     write(*,2000) kp,tary
  end if

  !     Format

2000 format(10x,'Compressed Storage =',i9,20x,'t=',0p,2f9.2)

end subroutine iters
