! Id:$
function psub(val)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  implicit  none

  real*8 ::    psub, val, xval

  save

  data      xval / 0.0d0 /

  !     Look at parameter

  if(val == 0.0d0) then
     xval = 0.0d0
  else
     xval = xval - val
  endif

  psub = xval

end function psub
