! Id:$
subroutine pltord(ix,iel, iju,jplt)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Sets plot order for element type iel

  !      Inputs:
  !         ix(*)     - Nodal connection list
  !         iel       - Element type

  !      Outputs:
  !         iju       - Number of points to describe element plot
  !         jplt(*)   - Element nodal plot sequence
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  include  'cdata.h'
  include  'pdata3.h'
  include  'pdata5.h'
  include  'pdata6.h'

  integer ::   iel, iiu, iju, ij, iplt(9), jplt(*)
  integer ::   ix(*)

  !     Default orders for 3-9 node 2-d elements

  data iplt/1,5,2,6,3,7,4,8,1/

  !     Get number of plot points to go around element

  if(iel > 0) then
     iiu = inord(iel)
     iju = 0

     if(iiu /= 0 .and. .not. hide) then

        !         Set plot table for specified values

        do ij = 1,abs(iiu)
           if(ipord(ij,iel) > 0 .and. ipord(ij,iel) <= nen) then
              if(ix(ipord(ij,iel)) > 0) then
                 iju       = iju + 1
                 jplt(iju) = ipord(ij,iel)
              endif
           endif
        end do

     elseif(hide) then

        !         Set plot table for hidden surface

        if(iiu > 0 .and. iiu < 5) then
           do ij = 1,abs(iiu)
              if(ipord(ij,iel) > 0 .and. ipord(ij,iel) <= nen) then
                 if(ix(ipord(ij,iel)) > 0) then
                    iju       = iju + 1
                    jplt(iju) = ipord(ij,iel)
                 endif
              endif
           end do
        else

           !           Set plot table for 4 node element

           do ij = 1,4
              if(ix(ij) > 0) then
                 iju       = iju + 1
                 jplt(iju) = ij
              endif
           end do
           iju       = iju + 1
           jplt(iju) = 1

        endif

     elseif(nen == 3) then

        !         Set plot table for 3 node element

        do ij = 1,3
           if(ix(ij) > 0) then
              iju       = iju + 1
              jplt(iju) = ij
           endif
        end do
        iju       = iju + 1
        jplt(iju) = 1

     else

        !       Set plot table for 3-9 node element

        do ij = 1,9
           if(iplt(ij) <= nen) then
              if(ix(iplt(ij)) > 0) then
                 iju       = iju + 1
                 jplt(iju) = iplt(ij)
              endif
           endif
        end do

     endif
  elseif(iel < 0) then
     iiu = exord(-iel)
     iju = 0

     if(iiu /= 0 .and. .not. hide) then

        !         Set plot table for specified values

        do ij = 1,abs(iiu)
           if(epord(ij,-iel) > 0 .and. epord(ij,-iel) <= nen) then
              if(ix(epord(ij,-iel)) > 0) then
                 iju       = iju + 1
                 jplt(iju) = epord(ij,-iel)
              endif
           endif
        end do

     elseif(hide) then

        !         Set plot table for hidden surface

        if(iiu > 0 .and. iiu < 5) then
           do ij = 1,abs(iiu)
              if(epord(ij,-iel) > 0 .and. epord(ij,-iel) <= nen) then
                 if(ix(epord(ij,-iel)) > 0) then
                    iju       = iju + 1
                    jplt(iju) = epord(ij,-iel)
                 endif
              endif
           end do
        else

           !           Set plot table for 4 node element

           do ij = 1,4
              if(ix(ij) > 0) then
                 iju       = iju + 1
                 jplt(iju) = ij
              endif
           end do
           iju       = iju + 1
           jplt(iju) = 1

        endif

     elseif(nen == 3) then

        !         Set plot table for 3 node element

        do ij = 1,3
           if(ix(ij) > 0) then
              iju       = iju + 1
              jplt(iju) = ij
           endif
        end do
        iju       = iju + 1
        jplt(iju) = 1

     else

        !       Set plot table for 3-9 node element

        do ij = 1,9
           if(iplt(ij) <= nen) then
              if(ix(iplt(ij)) > 0) then
                 iju       = iju + 1
                 jplt(iju) = iplt(ij)
              endif
           endif
        end do

     endif

  endif

end subroutine pltord
