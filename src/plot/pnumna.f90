! Id:$
subroutine pnumna(ix,nen1,nen,numel, ip)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  implicit  none

  include  'pbody.h'

  integer ::   nen1,nen, numel, i,nn
  integer ::   ix(nen1,*), ip(*)

  save

  !     Tag active nodes

  do nn = 1,numel
  if(ix(nen1-1,nn) >= 0 .and. &
    (maplt == 0 .or. ix(nen1,nn) == maplt)) then
  do i = 1,nen
  if(ix(i,nn) > 0) ip(ix(i,nn)) = 1
  end do
endif
end do

    end subroutine pnumna
