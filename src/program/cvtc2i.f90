! Id:$
function cvtc2i(cdev)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Convert a character to an integer

  !      Inputs:
  !         cdev    - Character array to convert

  !      Outputs:
  !         cvtc2i  - Integer value of character
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  integer ::   cvtc2i
  character cdev(12)*1
  integer ::   i,n,nz

  nz= ichar('0')
  n = 0
  do i = 1,12
  if(cdev(i) == char(0) .or. cdev(i) == ' ') go to 200
  n = 10*n + (ichar(cdev(i)) - nz)
  end do

  200 cvtc2i = n

end function cvtc2i
