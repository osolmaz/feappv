! Id:$
subroutine plfacn(ix,ia,nen,numel,nface,ie,nie)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Determines which exterior faces are directed toward
  !               view point.

  !      Inputs:
  !         ix(nen1,*)- Element nodal connection lists
  !         ia(*)     - Active element plots based on materials
  !         nen       - Number nodes/element
  !         numel     - Number of elements/faces
  !         ie(nie,*) - Material set assembly data
  !         nie       - Dimension of ie array

  !      Outputs:
  !         nface     - Number of faces
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'iofile.h'
  include  'pdata5.h'
  include  'pdata6.h'
  include  'plclip.h'
  include  'sdata.h'

  include  'pointer.h'
  include  'comblk.h'

  logical ::   lclip,addfac
  integer ::   nen,numel,nface,nie, i,j,m,n
  integer ::   iel,iiel

  integer ::   ix(nen1,numel), ia(*), ie(nie,*)
  integer ::   iq(4,6), it(3,4)

  save

  !     8-node brick faces

  data iq/3,2,1,4, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8, 5,6,7,8/

  !     4-node tet faces

  data it/1,2,4, 2,3,4, 3,1,4, 1,3,2/

  !     Compute location of boundary faces

  nface = 0
  do n = 1,numel
     if(ix(nen1-1,n) >= 0 .and. ia(n) >= 0) then
        iel = ie(nie-1,ix(nen1,n))
        if(iel > 0) then
           iiel = inord(iel)
        else
           iiel = exord(-iel)
        endif

        !        No face if inord < 0

        if     (iiel < 0) then

           !        Set for tetrahedral element faces

        elseif (iiel == 9 ) then

           if( lclip(ix(1,n),4,hr(np(43)),ndm) ) then
              do m = 1,4
                 addfac = .true. 
                 do j = 1,3
                    i = ix(it(j,m),n) - 1
                    if(mr(np(47)+i) == 0) then
                       addfac = .false. 
                    endif
                 end do ! j
                 if(addfac) then
                    nface = nface + 1
                 endif
              end do ! m
           end if

           !        Set for brick element faces

        elseif (iiel > 10 ) then

           if( lclip(ix(1,n),8,hr(np(43)),ndm) ) then
              do m = 1,6
                 addfac = .true. 
                 do j = 1,4
                    i = ix(iq(j,m),n) - 1
                    if(mr(np(47)+i) == 0) then
                       addfac = .false. 
                    endif
                 end do ! j
                 if(addfac) then
                    nface = nface + 1
                 endif
              end do ! m
           end if

           !        Set space for line elements

        elseif( iiel > 0 .and. iiel <= 3 ) then
           nface = nface + 1

           !        Set space for top and bottom shell faces

        elseif( lclip(ix(1,n),min(4,nen),hr(np(43)),ndm) ) then

           nface = nface + 2

        end if

     end if
  end do

end subroutine plfacn
