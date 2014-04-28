! Id:$
subroutine compro(numnp, nen, nen1, ndf, ix, id, &
    ic, ielc, ir, jc, kp, lir, bycol, wdiag, all)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose:  Compute locations for non-zero terms in coefficient
  !                matrix.

  !      Inputs:
  !         numnp      -  Number of nodes in mesh
  !         nen        -  Maximum number of nodes on any element
  !         nen1       -  Dimension for 'ix' array
  !         ndf        -  Number of unknowns at each node.
  !         ix(nen1,*) - List of nodes connected to each element
  !         id         -  Active unknowns at each node.
  !         neqv       -  Number of equations
  !         kp         -  Dimension of IELC (= ic(neq))
  !         lir        -  Available length for IR array.
  !         bycol      -  Storage by columns if true
  !         all        -  Storage all row/column if true

  !      Outputs:
  !         ielc       -  Holds the set of elements connected to each node.
  !         ir         -  Row number of each nonzero in the stiffness matrix.
  !         jc         -  end of enteries in ir from a given column.
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'compac.h'
  include  'iofile.h'

  include  'pointer.h'
  include  'comblk.h'

  logical ::   bycol, wdiag, all
  integer ::   i, j, k, ne, nep, neq, nn
  integer ::   numnp, nen, nen1, ndf, kp, kpo, lir
  integer ::   ix(nen1,*), id(ndf,*), ic(*), ir(*), ielc(*), jc(*)

  save

  !     Set up compressed profile pointers.

  neq = 0
  do i = 1, numnp
  do j = 1,ndf
  neq = max(neq,id(j,i))
  end do ! j
  end do ! i

  !     Check all equations

  kp  = 0
  nep = 1
  do i = 1, neq
  ne    = ic(i)
  jc(i) = kp
  kpo   = kp + 1
  do k = nep, ne
  nn = ielc(k)

  !         Check element type(>0: FE)

  if(nn > 0) then
    call comelm(id,ix(1,nn), ir, ndf,nen, kpo,kp,i,lir, &
      bycol,wdiag,all)
  else
    write(*,*) ' **ERROR** Incorrect COMPRO type'
  endif

  !         End element tests

  end do ! k
  jc(i) = kp
  nep   = ne + 1
  end do ! i

end subroutine compro

subroutine comelm(id,ix, ir, ndf,nen, kpo,kp,neq,lir, &
    bycol,wdiag,all)

  implicit  none

  include  'iofile.h'

  logical ::   addeq, bycol, wdiag, all
  integer ::   ndf,nen,kpo,kp,neq,lir
  integer ::   i,l,m, kk,neqj
  integer ::   id(ndf,*),ix(*),ir(*)

  save

  do l = 1,nen
  kk = ix(l)
  if(kk > 0) then
    do m = 1, ndf
    neqj = id(m,kk)

    !           Check if equation to be added

    if(all) then                           ! all terms
      addeq   = neqj > 0
    elseif(bycol) then                     ! by columns
      if(wdiag) then
        addeq = neqj <= neq .and. neqj > 0  ! diagonal in
      else
        addeq = neqj < neq .and. neqj > 0  ! diagonal out
      endif
    else                                   ! by rows
      if(wdiag) then
        addeq = neqj >= neq                ! diagonal in
      else
        addeq = neqj > neq                ! diagonal out
      endif
    endif

    !           Add equation to list

    if(addeq) then

      !             Check if equation already in list.

      do i = kpo, kp
      if(ir(i) == neqj) go to 200
      end do ! i

      !             New equation, add to list

      kp = kp + 1
      if(kp <= lir) then
        ir(kp) = neqj
      else
        write(iow,2000)
        if(ior < 0) write(*,2000)
        call plstop()
      endif
      200 continue
    endif
    end do ! m
  endif
  end do ! l

  !     Format

  2000 format('   *ERROR* Insufficient memory for compressed storage')

end subroutine comelm
