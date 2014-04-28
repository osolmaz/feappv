! Id:$
subroutine evalex(xs,v,val,ns,error)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Identify expression in character string and evaluate

  !      Inputs:
  !         xs(*) - Character string of input data
  !         v(*)  - Array of real values
  !         ns    - Length of character string

  !      Outputs:
  !         val   - Value of expression
  !         error - Error indicator if true
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'corfil.h'
  include  'iofile.h'

  logical ::   error
  character xs*(*),x*256,y*1,op(25)*1
  integer ::   i,j,k,ns,num
  real*8 ::    val, v(*)

  save

  !     Pack expression by removing any ' ' characters

  num = 0
  do i = 1,ns
     if(xs(i:i) /= ' ') then
        num        = num + 1
        x(num:num) = xs(i:i)
     endif
  end do ! i

  !     Evaluate an expression: (+) add, (-) subtract, (*) multiply,
  !                             (/) divide, (^) power.

  k  = 0
  do i = 1,num
     if(k == 0 .and. x(i:i) == ' ') go to 100
     if((x(i:i) == '+') .or. (x(i:i) == '-')) then
        if(i > 2) then
           y = x(i-1:i-1)
           if(y == 'e' .or. y == 'd' .or. y == 'E' .or. y == 'D') then
              if((x(i-2:i-2) >= '0' .and. x(i-2:i-2) <= '9') .or. &
                   x(i-2:i-2) == '.') go to 100
           endif
        endif
        k      = k + 1
        op(k)  = x(i:i)
        x(i:i) = ','
     endif
     if((x(i:i) == '*') .or. (x(i:i) == '/') .or. (x(i:i) == '^')) then
        k      = k + 1
        op(k)  = x(i:i)
        x(i:i) = ','
     endif
100  continue
  end do ! i

  call dcheck(x,v,num,error)

  if(error) return

  !     Compute value of expression

  val = v(1)
  if(k >= 1) then

     !      1. Evaluate all exponentiations

     i = 1
110  if(op(i) == '^') then
        v(i) = v(i) ** v(i+1)
        k = k - 1
        do j = i,k
           v(j+1) = v(j+2)
           op(j) = op(j+1)
        end do ! j
     endif
     i = i + 1
     if(i <= k) go to 110

     !      2. Evaluate all multiplications and divisions

     i = 1
120  if(op(i) == '*' .or. op(i) == '/') then
        if(op(i) == '*') then
           v(i) = v(i) * v(i+1)
        else
           if(v(i+1) == 0.0d0) then
              write(iow,3000) let
              if(v(i) /= 0.0d0) then
                 v(i) = sign(1.d20,v(i))
              else
                 v(i) = 0.0d0
              endif
           else
              v(i) = v(i) / v(i+1)
           endif
        endif
        k = k - 1
        do j = i,k
           v(j+1) = v(j+2)
           op(j) = op(j+1)
        end do ! j
     else
        i = i + 1
     endif
     if(i <= k) go to 120

     !      3. Evaluate all additions and subractions

     val = v(1)
     if(k > 0) then
        do i = 1,k
           if(op(i) == '+') val = val + v(i+1)
           if(op(i) == '-') val = val - v(i+1)
        end do ! i
     endif
  endif

  !     Format

3000 format(' *ERROR* Dividing by zero in expression for: ',a)

end subroutine evalex
