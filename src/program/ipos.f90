! Id:$
function ipos(file,nn)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Locate last character in character array

  !      Inputs:
  !         file(*) - Array to search
  !         nn      - Length of array

  !      Outputs:
  !         ipos   - Position of last character
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  integer ::   ipos
  integer ::   n,nn
  character file(nn)*1

  do n = nn,1,-1
     if(file(n) /= ' ') go to 100
  end do
  n    = 0
100 ipos = n

end function ipos
