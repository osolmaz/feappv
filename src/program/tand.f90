! Id:$
function tand(x)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved
  implicit none

  real*8 :: tand, x

  tand = tan(atan(1.d0)*x/45.d0)

end function tand
