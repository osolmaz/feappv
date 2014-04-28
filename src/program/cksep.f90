! Id:$
logical function cksep(x1)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Check for existence of separator characters in data.
  !               Separators are blank, comma, or equal sign.

  !      Inputs:
  !         x1  -  Character to check

  !      Outputs:
  !         cksep - True of character is a valid separator; else false.
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  character x1*1

  cksep = (x1 == ' ') .or. (x1 == ',') .or. (x1 == '=')

end function cksep
