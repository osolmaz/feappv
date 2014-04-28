! Id: pltext.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
subroutine pltext(x,y,il,str)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--+---------+---------+---------+---------+---------+---------+-]
  !      Purpose: Place graphics text at screen positions

  !      Inputs:
  !         x,y       - Screen coordinates for graphics text
  !         il        - Length of text
  !         str(*)    - Text string to place

  !      Outputs:
  !         none      - Plot outputs to screen/file
  !-----[--+---------+---------+---------+---------+---------+---------+-]

  implicit  none

  include  'pdatxt.h'

  character str(*)*1
  integer ::   il
  real*8 ::    x,y

  save

  !     Set coordinates for plot

  dtext = 0.03d0

  call tplot(x,y,str,il,0)

end subroutine pltext
