! Id:$
subroutine meshck(ip,ie,id,nty,ix,nie,nen,nen1,ndf, &
     numnp,numel,nummat,errs)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Perform check on mesh data to ensure nodes/elements input

  !      Inputs:
  !         ie(nie,*)  - Material set assembly information
  !         id(ndf,*)  - Boundary condition and equation number array
  !         nty(*)     - Nodal type
  !         ix(nen1,*) - Element nodal connection lists
  !         nie        - Dimension of ie array
  !         nen        - Maximum number of nodes/element
  !         nen1       - Dimension for ix array
  !         ndf        - Number dof/node
  !         numnp      - Number of nodes in mesh
  !         numel      - Number of elemenst in mesh
  !         nummat     - Number of material sets

  !      Outputs:
  !         ip(ndf,*)  - List of active nodes, used for graphics
  !         errs       - Flag, true if errors detected
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'cblend.h'
  include  'elflag.h'
  include  'iofile.h'
  include  'prflag.h'

  include  'pointer.h'
  include  'comblk.h'

  logical ::   errs
  integer ::   i,ii,j,ma,mg,n,nen,nen1,nie,ndf,numnp,numel,nummat

  integer ::   ip(ndf,*),ie(nie,*),id(ndf,*),ix(nen1,*),nty(*)

  save

  !     Perform mesh checks to ensure nodes/elements input

  errs = .false. 
  do n = 1,numel
     if (ix(nen1,n) <= 0 .or. ix(nen1,n) > nummat) then
        write(iow,2000) n
        if(ior < 0) write(*,2000) n
        errs = .true. 
     else
        do i = 1,nen
           ii = ix(i,n)
           if(ii > numnp .or. ii < 0) then
              write(iow,2001) ii,n
              if(ior < 0) write(*,2001) ii,n
              errs = .true. 
           elseif(ii /= 0 .and. nty(ii) < 0) then
              write(iow,2002) ii,n
              if(ior < 0) write(*,2002) ii,n
              errs = .true. 
           endif
        end do
     endif
  end do

  !     Remove unused dof's using ie(nie,*) array

  do n = 1,numnp
     do j = 1,ndf
        ip(j,n) = 0
     end do
  end do

  !     Check nodes on each element for active dof's

  do n = 1,numel
     mg = ix(nen1,n)

     !       Loop over the material sets

     do ma = 1,nummat
        if(ie(nie-2,ma) == mg) then
           do i = 1,nen
              ii = ix(i,n)
              if(ii > 0) then
                 do j = 1,ndf
                    if(ie(j,ma) > 0) then
                       ip(ie(j,ma),ii) = 1
                    endif
                 end do
              endif
           end do
        endif
     end do
  end do

  !     Set b.c. restraints for unused dof's

  do n = 1,numnp
     do j = 1,ndf
        if(ip(j,n) == 0) then
           id(j,n) = -1000
        end if
     end do
  end do

  !     Remove unused nodes - for graphics

  do n = 1,numnp
     ip(1,n) = 0
  end do

  do n = 1,numel
     do i = 1,nen
        ii = ix(i,n)
        if(ii > 0) ip(1,ii) = 1
     end do
  end do

  !     Set flat to indicate node is not used

  do n = 1,numnp
     if(ip(1,n) == 0) then
        nty(n) = -1
     end if
  end do

  !     Fix all unspecified coordinate dof's

  do n = 1,numnp
     if(nty(n) < 0) then
        do i = 1,ndf
           id(i,n) = 1
        end do
     endif
  enddo

  !     If supernodes used then

  if(numbd > 0) then
     if(numsn > 0) then
        if(numsd > 0) then
           call mshcksn(mr(np(162)),mr(np(164)),numsd,numsn,numbd,errs)
        else
           write(iow,2003)
           if(ior < 0) write(*,2003)
           errs = .true. 
        endif
     else
        write(iow,2004)
        if(ior < 0) write(*,2004)
        errs = .true. 
     endif
  endif

  !     Set first and last element numbers for each material type

  do ma = 1,min(80,nummat)
     do n = 1,numel
        if(ix(nen1,n) == ma) then
           elstart(ma) = n
           go to 100
        endif
     end do ! n
100  do n = numel,1,-1
        if(ix(nen1,n) == ma) then
           ellast(ma) = n
           go to 200
        endif
     end do ! n
200  continue
  end do ! ma

  !     Formats

2000 format(10x,' *ERROR* Data for element ',i6,' not input')

2001 format(10x,' *ERROR* Data for node ',i6,' on element',i6, &
       ' greater than maximum or negative')
2002 format(10x,' *ERROR* Data for node ',i6,' on element',i6, &
       ' not input')

2003 format(10x,' *ERROR* Blending functions used but no SIDEs', &
       ' exist')

2004 format(10x,' *ERROR* Blending functions used but no SNODes', &
       ' exist')
end subroutine meshck

subroutine mshcksn(is,iblend,numsd,numsn,numbd,errs)

  implicit  none

  include  'iofile.h'

  logical ::   errs
  integer ::   numsd,numsn,numbd,n,i,inc
  integer ::   is(16,numsd),iblend(20,numbd)

  save

  !     Loop over SIDE nodes to check if any greater than number SNODes

  do n = 1,numsd
     if(is(1,n) == 2) then
        inc = 2
     else
        inc = 1
     endif
     do i = 2,16,inc
        if(is(i,n) > numsn) then
           write(iow,2000) n, numsn, i
           errs = .true. 
        endif
     end do ! i
  end do ! n

  !     Loop over BLENd nodes to check if any greater than number SNODes

  do n = 1,numbd
     do i = 11,18
        if(iblend(i,n) > numsn) then
           write(iow,2001) n, numsn, i
           errs = .true. 
        endif
     end do ! i
  end do ! n

  !     Formats

2000 format(' *ERROR* SIDE',i5,' has value greater than maximum SNODE' &
       ,' (',i5,') at entry',i5/)

2001 format(' *ERROR* BLENd',i5,' has value greater than maximum SNODE' &
       ,' (',i5,') at entry',i5/)

end subroutine mshcksn
