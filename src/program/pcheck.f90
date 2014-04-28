! Id:$
subroutine pcheck(nc,xs,error)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Check that input string contains admissible data
  !               and parentheses match.  Convert all input letters
  !               to lower case for further processing

  !      Inputs:
  !         nc     - Number of characters to check
  !         xs(*)  - Character array

  !      Outputs:
  !         error  - Flag, true if error occurs
  !-----[--.----+----.----+----.-----------------------------------------]

  implicit  none

  logical ::   error
  integer ::   i,j,n,nc
  character x(75)*1,xs(75)*1

  save

  !     Make sure that all of x is lower case and blanks are removed

  i = 0
  do j = 1,75
  x(j) = ' '
  if(xs(j) /= ' ' .and. xs(j) /= '=' .and. xs(j) /= ',') then
    i = i + 1
    x(i)  = xs(j)
    xs(j) = ' '
    n = ichar( x(i) )
    if(n >= 65 .and. n <= 90) x(i) = char(n + 32)
  endif
  end do

  !     Move back and check characters for incorrect parenthesis

  error = .false. 
  n = 0
  do j = 1,i
  xs(j) = x(j)
  if(xs(j) == '(') n = n+1
  if(xs(j) == ')') n = n-1
  if(n < 0 .or. n > 1 ) error = .true. 
  end do

  if(n /= 0) error = .true. 
  n = ichar(xs(1))
  if(n < 97 .or. n > 122) error = .true. 

  !     Check characters for incorrect parameters

  if( .not. error) then
    do j = 2,i
    n = ichar(xs(j))
    if( .not. (n >= 97 .and. n <= 122) .and. &
      .not. (n >= 40 .and. n <= 57) ) then
    error = .true. 
  endif
  end do
endif

if(error) then
  write(*,2000)
else
  write(*,2001) nc,(xs(j),j=1,i)
endif

!     Formats

2000 format(' Incorrect statement - reinput ')

2001 format('   No.',i3,'>',a1,' = ',74a1)

    end subroutine pcheck
