! Id:$
subroutine genint(nd,ix,nix,num,cname,carg,prt,prth,err,type)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Generate integer data arrays

  !      Inputs:
  !         nd        - Number of integer items/node to generate
  !         nix       - Dimension of integer array ix
  !         num       - Number of generated items
  !         cname     - Header identification for outputs
  !         carg      - Value identifier for outputs
  !         prt       - Output generated data if true
  !         prth      - Output title/header if true
  !         type      - Generation type: 1=node, 2=element

  !      Outputs:
  !         ix(nix,*) - Integer data generated
  !         err       - True if error occurred
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'iofile.h'

  logical ::   prt,prth,err,errck, pinput, oflg
  character cd*12, cname*(*), carg*(*)
  integer ::   i,j,l,n,lg,ng,nd,nix,num,mct,type

  integer ::   ix(nix,*)
  integer ::   ixl(14)
  real*8 ::    td(16)

  save

  err = .false. 
  cd  = carg
  mct = 0
  n   = 0
  ng  = 0
  100 l   = n
  lg  = ng

  !     Call input routine - values returned in td and then moved

  101 if(ior < 0) write(*,2010)
  errck = pinput(td,2+nd)
  if(errck) go to 101
  n   = nint(td(1))
  if(n > num) write(iow,3001) n,cname
  if(n > num .and. ior < 0) write(  *,3001) n,cname
  if(n <= 0 .or. n > num) go to 105
  ng  = nint(td(2))
  do i = 1,nd
  ixl(i)  = nint(td(i+2))
  ix(i,n) = ixl(i)
  end do
  if(lg) 102,100,102
  102 lg = sign(lg,n-l)
  103 l = l + lg
  if((n-l)*lg <= 0) go to 100
  if(l <= 0 .or. l > num) go to 104
  do i = 1,nd
  ix(i,l) = ix(i,l-lg)
  end do
  go to 103
  104 write(iow,3000) l,cname
  if(ior < 0) write(  *,3000) l,cname
  err = .true. 
  go to 100

  105 if( .not. prt) return

  do j = 1,num

  !       Output if non-zero

  oflg = .false. 
  do l = 1,nd
  if(ix(l,j) /= 0) oflg = .true. 
  enddo

  if(oflg) then

    !         Output header

    mct = mct - 1
    if(mct <= 0) then
      mct = 50
      call prtitl(prth)
      if(type == 1) then
        write(iow,2001) cname,(l,cd,l=1,nd)
        if(ior < 0) then
          write(*,2001) cname,(l,cd,l=1,nd)
        endif
      else
        write(iow,2002) cname,(l,cd,l=1,nd)
        if(ior < 0) then
          write(*,2002) cname,(l,cd,l=1,nd)
        endif
      endif
    endif

    !         Output values

    write(iow,2009) j,(ix(l,j),l=1,nd)
    if(ior < 0) then
      write(*,2009) j,(ix(l,j),l=1,nd)
    endif
  endif

  end do

  !     Formats

  2001 format(5x,a//'      Node',6(i5,a6):/(10x,6(i5,a6):))
  2002 format(5x,a//'   Element',6(i5,a6):/(10x,6(i5,a6):))

  2009 format(i10,6i11:/(10x,6i11):)

  2010 format(' Input: item#, inc., values'/3x,'>',$)

  3000 format(' *ERROR* Attempt to generate item',i5,' in ',a)

  3001 format(' *ERROR* Attempt to input item',i5,', terminate', &
    ' input in ',a)

end subroutine genint
