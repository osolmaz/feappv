! Id:$
function asind(x)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved
  implicit none

  real*8 :: asind, x

  asind = 45.d0/atan(1.d0)*asin(x)

end function asind
