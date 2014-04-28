! Id:$
subroutine plfacx(ix,ia,ixf,nen,numel,ie,nie)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Construct surface mesh array - ixf

  !      Inputs:
  !         ix(nen1,*)- Element nodal connection lists
  !         ia(*)     - Active element plots based on materials
  !         nen       - Dimension of ix array
  !         numel     - Number of elements
  !         ie(nie,*) - Assembly data for material sets
  !         nie       - Dimension of ie array

  !      Outputs:
  !         ixf(7,*)  - Face array
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  include  'pdata5.h'
  include  'pdata6.h'
  include  'sdata.h'

  include  'pointer.h'
  include  'comblk.h'

  logical ::   lclip,addfac, setval,palloc
  integer ::   nen,numel,nie, i,j,j1,m,n, nf
  integer ::   iel,iiel

  integer ::   ix(nen1,numel), ia(*), ixf(7,*), ie(nie,*)
  integer ::   iq(4,7), it(3,4), iline(6), ii(4)

  save

  !     8-node brick faces

  data iq/3,2,1,4, 1,2,6,5, 2,3,7,6, 3,4,8,7, 4,1,5,8, 5,6,7,8, &
       &         1,2,3,4/

  !     4-node tet faces

  data it/1,2,4, 2,3,4, 3,1,4, 1,3,2/

  !     Compute location of boundary faces

  nf = 1
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

                 !             Face is to be plotted if visible

                 if(addfac) then
                    ii(1)  = 1
                    do i = 2,3
                       if(ix(it(ii(1),m),n) > ix(it(i,m),n)) then
                          ii(1)= i
                       endif
                    end do
                    ii(2) = mod(ii(1),3) + 1
                    ii(3) = mod(ii(2),3) + 1
                    iline(1) = ix(it(ii(1),m),n)
                    iline(2) = ix(it(ii(2),m),n)
                    iline(3) = ix(it(ii(3),m),n)
                    iline(4) = 0
                    iline(5) = ix(nen1-1,n)
                    iline(6) = ix(nen1,n)

                    call pfacex(iq(1,7),iline,ixf(1,nf),4,6,nf,n)

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

                 !             Face is to be plotted if visible

                 if(addfac) then
                    ii(1)  = 1
                    do i = 2,4
                       if(ix(iq(ii(1),m),n) > ix(iq(i,m),n)) then
                          ii(1)= i
                       endif
                    end do
                    ii(2) = mod(ii(1),4) + 1
                    ii(3) = mod(ii(2),4) + 1
                    ii(4) = mod(ii(3),4) + 1
                    do j = 1,4
                       iline(j) = ix(iq(ii(j),m),n)
                    end do ! j
                    iline(5) = ix(nen1-1,n)
                    iline(6) = ix(nen1,n)

                    call pfacex(iq(1,7),iline,ixf(1,nf),4,6,nf,n)

                 endif
              end do ! m
           end if

           !        Set space for line elements

        elseif( iiel > 0 .and. iiel <= 3 ) then

           !          Do a line element

           iline(1) = ix(1,n)
           iline(2) = ix(2,n)
           iline(3) = ix(2,n)
           iline(4) = ix(1,n)
           iline(5) = ix(nen1-1,n)
           iline(6) = ix(nen1,n)

           call pfacex(iq(1,7),iline,ixf(1,nf),4,6,nf,n)

           !        Set space for top and bottom shell faces

        elseif( lclip(ix(1,n),min(4,nen),hr(np(43)),ndm) ) then

           !          Do a 2-d surface with both faces considered

           ii(1)  = 1
           do i = 2,4
              if(ix(iq(ii(1),1),n) > ix(iq(i,1),n)) then
                 ii(1)= i
              endif
           end do
           ii(2) = mod(ii(1),4) + 1
           ii(3) = mod(ii(2),4) + 1
           ii(4) = mod(ii(3),4) + 1
           do j = 1,4
              iline(j) = ix(iq(ii(j),1),n)
           end do ! j
           iline(5) = ix(nen1-1,n)
           iline(6) = ix(nen1,n)

           call pfacex(iq(1,7),iline,ixf(1,nf),4,6,nf,-n)

           ii(1)  = 1
           do i = 2,4
              if(ix(iq(ii(1),7),n) > ix(iq(i,7),n)) then
                 ii(1)= i
              endif
           end do
           ii(2) = mod(ii(1),4) + 1
           ii(3) = mod(ii(2),4) + 1
           ii(4) = mod(ii(3),4) + 1
           do j = 1,4
              iline(j) = ix(iq(ii(j),7),n)
           end do ! j
           iline(5) = ix(nen1-1,n)
           iline(6) = ix(nen1,n)

           call pfacex(iq(1,7),iline,ixf(1,nf),4,6,nf,-n)

        end if
     end if
  end do

  nf = nf - 1
  if(nf > 0) then
     setval = palloc(82,'TEMP2',7*nf,1)
     call mergei( 0, 7, ixf, nf, mr(np(82)) )
     setval = palloc(82,'TEMP2', 0,1)
  endif

  i  = 1
  ii(1) = ixf(1,i)
  do j = 2,nf
     if(ixf(1,j) > ii(1) .or. j == nf) then
        if(j == nf) then
           j1 = nf
        else
           j1 = j - 1
        endif
        do m = i,j1
           if( ixf(4,m) == 0 .or. ixf(4,m) == ixf(1,m)) then
              do n = m+1,j-1
                 if(ixf(2,m) == ixf(3,n) .and. &
                      ixf(3,m) == ixf(2,n) .and. &
                      (ixf(4,n) == ixf(1,n) .or. ixf(4,n) == 0) ) then
                    ixf(7,m) = - abs(ixf(7,m))
                    ixf(7,n) = - abs(ixf(7,n))
                 endif
              end do ! n
              ixf(4,m) = ixf(1,m)
           else
              do n = m+1,j-1
                 if(ixf(2,m) == ixf(4,n) .and. &
                      ixf(4,m) == ixf(2,n) .and. &
                      ixf(3,m) == ixf(3,n) .and. &
                      ixf(5,m) > 0      ) then
                    ixf(7,m) = - abs(ixf(7,m))
                    ixf(7,n) = - abs(ixf(7,n))
                 endif
              end do ! n
           endif
        end do ! m
        i  = j
        ii(1) = ixf(1,j)
     endif
  end do ! j

end subroutine plfacx
