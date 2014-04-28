! Id:$
subroutine just(y,nt,n0)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Justify alphanumeric data in a string:
  !               - Numbers are right justified
  !               - Alphanumerics remain left justified

  !      Inputs:
  !         y*(*) - Unjustified string of data
  !         nt    - Length of string
  !         n0    - Field width for justification

  !      Outputs:
  !         y*(*) - Justified string of data
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  integer ::   nt,n0,n1,i,j,l,nl
  character y*(*)

  save

  n1 = n0 - 1
  do i = 1,nt,n0

  !       Find last non-blank character in string

  do j = i,i+n1
  if(y(j:j) /= ' ') go to 100
  end do
  y(i+n1:i+n1) = '0'
  100 if(y(i+n1:i+n1) == ' ') then

    !         Identify a number in field and right justify

    if((y(i:i) >= '0' .and. y(i:i) <= '9') &
      .or. (y(i:i) == '-') &
      .or. (y(i:i) == '+') &
      .or. (y(i:i) == '.')) then
    do j = i+n1-1,i,-1
    if(y(j:j) /= ' ') go to 110
    end do
    110 nl = n1 + i - j
    do l = j,i,-1
    y(l+nl:l+nl) = y(l:l)
    y(l:l)       = ' '
    end do
  endif
endif
end do

    end subroutine just
