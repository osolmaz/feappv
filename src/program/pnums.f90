! Id:$
subroutine pnums()

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Determine maximum node, element and material set
  !               number in mesh.

  !      Inputs:
  !         From read of data file(s)

  !      Outputs:
  !         numnp     - Maximum node         number input of generated
  !         numel     - Maximum element      number input of generated
  !         nummat    - Maximum material set number input of generated
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  include  'cblend.h'
  include  'cdata.h'
  include  'chdata.h'
  include  'codat.h'
  include  'comfil.h'
  include  'iodata.h'
  include  'iofile.h'
  include  'iosave.h'
  include  'sdata.h'
  include  'umac1.h'

  logical ::   pcomp, errck, pinput,tinput,vinput, lmesh
  logical ::   flag,readfl,savefl, eltype,pblktyp
  character cc*4,c2*8, tx(2)*15
  integer ::   numn,nume,numm,num,nt,nn,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9
  integer ::   di, dnumb,dlayer,nlay,j, mab
  real*8 ::    tb(7),td(15),dua(1),du
  integer ::   dia(1)

  save

  !     Use input data to determine total number of nodes or elmts

  numn      = 0
  nume      = 0
  numm      = 0
  numsn     = 0
  numsd     = 0
  numfc     = 0
  numbd     = 0
  dnumb     = 0
  mxilr     = 0

  lmesh = .false. 

  1 errck = tinput(tx,2,td,0)
  if(errck) go to 1
  cc = tx(1)(1:4)
  c2 = tx(2)(1:4)

  !     [read] - set read flag for reinputs

  if( pcomp(cc,'read',4) ) then
    lmesh = readfl(tx(2))
    go to 1
  endif

  !     [save] - set save flag for saving data

  if(pcomp(cc,'save',4)) then
    lsave = savefl(tx(2))
    go to 1
  endif

  !     [coor] - Nodal coordinate inputs

  if(pcomp(cc,'coor',4)) then

    100 errck = pinput(td,1)
    if(errck) go to 100
    num = nint(td(1))
    numn = max(numn,num)
    if(num > 0) go to 100

    !     [snod] - Super node inputs

  elseif(pcomp(cc,'snod',4)) then

    105 errck = pinput(td,1)
    num   = nint(td(1))
    numsn = max(numsn,num)
    if(num > 0) go to 105

    !     [elem] - Element data inputs

  elseif(pcomp(cc,'elem',4)) then

    110 errck = pinput(td,1)
    if(errck) go to 110
    num = nint(td(1))
    nume = max(nume,num)

    !       Input additional records

    if(num > 0) then
      do n1 = 14,nen,16
      n2 = min(nen+1-n1,16)
      111 errck = pinput(td,n2)
      if(errck) go to 111
      end do
      go to 110
    endif

    !     [side] - Side data inputs

  elseif(pcomp(cc,'side',4)) then

    tx(1) = 'initial'
    do while ( .not. pcomp(tx(1),'    ',4))
    errck = tinput(tx,1,td,0)
    if(pcomp(tx(1),'pola',4) .or. pcomp(tx(1),'cart',4) .or. &
      pcomp(tx(1),'segm',4) .or. pcomp(tx(1),'elip',4)) then
    numsd = numsd + 1
  endif
  end do ! while

  !     [bloc] - block inputs

elseif(pcomp(cc,'bloc',4)) then

  120 errck = tinput(tx,1,tb,7)
  if(errck) go to 120
  n2  = nint(tb(1))
  if(n2 <= 0) then
    write(iow,3002) n2
    call plstop()
  endif
  n3  = nint(tb(2))
  n4  = nint(tb(3))
  n5  = nint(tb(4))
  n6  = nint(tb(5))
  n7  = nint(tb(6))
  n8  = nint(tb(7))

  nn  = -1
  121 errck = tinput(tx,1,td,7)
  if(errck) go to 121
  if(pcomp(tx(1),'laye',4)) then
    dlayer = nint(tb(1))
    if(dlayer == 1) then
      nlay = n2
    elseif(dlayer == 2) then
      nlay = n3
    elseif(dlayer == 3) then
      nlay = n4
    endif

    j = 1
    do while(j <= nlay)
    123 errck = tinput(tx,0,td,1)
    if(errck) go to 123
    j = j + 16
    end do ! while
    !       No layers set a coordinate number
  else
    mab    = 0
    eltype = .false. 
    do while ( .not. eltype)
    eltype = pblktyp(tx(1),td, n8,n3,mab)
    if( .not. eltype) then
      errck = tinput(tx,1,td,2)
    endif
    end do !
    if(eltype) then
      nn = nn + 1
    else
      go to 121
    endif
  end if ! layers

  n7 = max(0,n7)
  124 errck = pinput(td,1)
  nn = nn + 1
  if(int(td(1)) /= 0) go to 124
  if(n8 == 0 .and. n4 > 0 .and. ndm == 3 .and. nn >= 7) then
    n8 = 10
  endif

  if(n8 < 10) then
    nt = 1
    if(nn <= 3) then
      n1 = n2+1
      if(n3 <= 1) then
        n0 =  n2
      else
        n0 =  n2/2
      endif
    else
      if(n3 <= 0) then
        write(iow,3002) n2,n3
        call plstop()
      endif
      n0 =  n2*n3
      n1 = (n2+1)*(n3+1) + n7*n3
      if(abs(n8) == 7) then
        n0 = n0/2
        if(n8 == -7) n1 = n1 + n0
      elseif(n8 >= 8) then
        n0 = n0/4
        if(n8 == 8) n1 = n1 - n0
      elseif(n8 > 0) then
        n0 = n0*2
      elseif(n8 == -1) then
        n1 = n1 + n0
      endif
    endif
    if(n4 > 0) then
      numn = max(numn,n4+n1-1)
    else
      numn = numn+n1
    endif
    if(n5 > 0) then
      nume = max(nume,n5+n0-1)
    elseif(n5 == 0) then
      nume = nume+n0
    endif
  elseif(n8 < 20) then
    nt = n4
    if(n8 == 10) then
      n9 = 1
    elseif(n8 == 11) then
      n9 = 6
    else
      write(iow,3004) n8
      call plstop()
    endif
    if(n4 <= 0) then
      write(iow,3002) n2,n3,n4
      call plstop()
    endif
    if(n5 > 0) then
      numn = max(numn,n5+(n2+1)*(n3+1)*(n4+1)-1)
    else
      numn = numn+(n2+1)*(n3+1)*(n4+1)
    endif
    if(n6 > 0) then
      nume = max(nume,n6+n2*n3*n4*n9-1)
    elseif(n6 == 0) then
      nume = nume+n2*n3*n4*n9
    endif
  elseif(n8 < 30) then
    nt = 1
    if(n3 <= 0) then
      write(iow,3002) n2,n3
      call plstop()
    endif
    n0 =  n2*n3
    n1 = (n2+1)*(n3+1) + n7*n3
    if(abs(n8) == 27) then
      n0 = n0/2
      if(n8 == -7) n1 = n1 + n0
    elseif(n8 >= 28) then
      n0 = n0/4
      if(n8 == 8) n1 = n1 - n0
    elseif(n8 > 20) then
      n0 = n0*2
    endif
    if(n4 > 0) then
      numn = max(numn,n4+n1-1)
    else
      numn = numn+n1
    endif
    if(n5 > 0) then
      nume = max(nume,n5+n0-1)
    elseif(n5 == 0) then
      nume = nume+n0
    endif
  else
    if(ndm <= 2) then
      nt = 1
      n4 = 1
      n0 = nint(td(3))
      n1 = nint(td(4))
      n5 = nint(td(5))
    else
      nt = n4
      n4 = nint(td(3))
      n0 = nint(td(4))
      n1 = nint(td(5))
      n5 = nint(td(6))
    endif
    call ublk(td,nn,n2,n3,n4,dua,dua,dia,dia,du,du,du, &
      n1,n0,ndm,nen1,n5,tx(1), .false. , 1)
    numn = max(numn,n1)
    nume = max(nume,n0)
  endif

  !     [blen] - blending function inputs

elseif(pcomp(cc,'blen',4)) then

  dnumb = dnumb + 1
  errck = tinput(tx(2),1,td,7)

  n0 = int(td(1))
  n1 = max(1,int(td(2)))

  if(pcomp(tx(2),'surf',4)) then
    n2   = 1
    n3   = int(td(3))
    n4   = int(td(4))
    n5   = int(td(5))
    n6   = int(td(6))
    errck= pinput(td,4)
    flag = int(td(4)) == 0
  elseif(pcomp(tx(2),'soli',4)) then
    n2   = max(1,int(td(3)))
    n3   = int(td(4))
    n4   = int(td(5))
    n5   = int(td(6))
    n6   = max(10,int(td(7)))
    flag = .false. 
  elseif(pcomp(tx(2),'line',4)) then
    n2   = 1
    n3   = int(td(3))
    n4   = int(td(4))
    n5   = int(td(5))
    n6   = int(td(6)) + 30
    errck= pinput(td,4)
    flag = int(td(4)) == 0
  else
    write(iow,3003) tx(2)
    call plstop()
  endif
  if(n5 < 0) then
    mxilr = max(mxilr,n0,n1,n2)
  endif

  errck  = tinput (tx(2),1,tb,7)
  di     = n6
  mab    = 0
  eltype = .false. 
  do while ( .not. eltype)
  eltype = pblktyp(tx(2),tb, di,n0,mab)
  if(di /= n6) then
    n6   = di
  endif
  if( .not. eltype) then
    errck = tinput(tx(2),1,tb,7)
  endif
  end do ! while

  flag = .false. 
  call pnumbl(ndm,n0,n1,n2,n6, n7,n8, flag)

  !       Set maximum node and element numbers

  if(n3 > 0) then
    numn = max(numn,n3+n8-1)
  else
    numn = numn+n8
  endif

  if(n4 > 0) then
    nume = max(nume,n5+n7-1)
  elseif(n4 == 0) then
    nume = nume+n7
  endif

  !     [mate] - material data set inputs

elseif(pcomp(cc,'mate',4)) then

  errck = vinput(yyy(16:30),15,td,1)
  n0    = nint(td(1))
  if(n0 == 0 .and. numm /= 0) then
    write(*,*) '     ERROR - Zero Material Number Specified'
    call plstop()
  else
    numm = max(numm,n0,1)
  endif

  !     [para] - parameter inputs

elseif(pcomp(cc,'cons',4) .or. pcomp(cc,'para',4)) then

  coflg = .true. 
  call pconst( .false. )

  !     [end] - end of mesh input data

elseif(pcomp(cc,'end',3)) then

  if(lsave) then
    write(iow,3000)
    call plstop()
  endif

  !       Set final number of blends

  numbd = numbd + dnumb

  go to 200

  !     User function for node element counting

else

  do j = 1,5
  if(pcomp(cc,umshc(j),4)) then
    uct    = umshc(j)
    unumnd = 0
    unumel = 0
    call umshlib(j,tx, .false. )
    numn = max(numn,unumnd)
    nume = max(nume,unumel)
  endif
  end do ! j

endif
go to 1

200 numnp  = max(numnp ,numn)
numel  = max(numel ,nume)
nummat = max(nummat,numm)

if(numnp <= 0 .or. numel <= 0 .or. nummat <= 0) then
  write(iow,3001) numnp,numel,nummat
  call plstop()
endif

!     Find start of problem for input of mesh data

rewind ior
300 read(ior,1000) cc
if( .not. pcomp(cc,'feap',4)) go to 300

!     Advance past control record and return to 'PCONTR'

read(ior,1000) cc

!     Input/Output Formats

1000 format(a4,11x,i15)

3000 format(' *ERROR* No closing SAVE,END statement for input data.')

3001 format(' *ERROR* Size of problem does not permit a solution:'/ &
  '         Number of nodes         = ',i9/ &
  '         Number of elements      = ',i9/ &
  '         Number of material sets = ',i9/)

3002 format(' *ERROR* Number of block increments incorrect:'/ &
  '         Number of 1-increments = ',i6/: &
  '         Number of 2-increments = ',i6/: &
  '         Number of 3-increments = ',i6/)

3003 format(' *ERROR* Blending option: ',a,' not supported')

3004 format(' *ERROR* Generation for type =',i5,' not supported')

    end subroutine pnums
