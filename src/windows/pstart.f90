! Id: pstart.f,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine pstart()

  !      * * F E A P * * A Finite Element Analysis Program
  !                        -      -       -        -
  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  implicit  none

  include  'pdata2.h'

  interface
     logical(4)  function initialsettings
     end function initialsettings
  end interface

  !     Graphics Driver for PC versioni

  idev = 2

  !     Start Windows

  call pwopn()

end subroutine pstart

logical(4) function initialsettings ( )

  use       DFLIB

  implicit  none

  type(qwinfo) winfo
  integer ::      status

  save

  !     Maximize Frame

  winfo.type = qwin$max
  status     = setwsizeqq(qwin$framewindow,winfo)

  initialsettings = .true. 

end function initialsettings
