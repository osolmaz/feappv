! Id:$
subroutine getcon(epmac)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Perform installation parameter computations

  !      Inputs:

  !      Outputs:
  !         epmac   - Smallest number that can be added to 1.0
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit  none

  include  'psize.h'
  include  'comblk.h'

  real*8 ::    epmac

  save

  !     Compute machine epsilon estimate

  epmac = 1.0d0
100 epmac = epmac*0.5d0
  if(1.d0 /= 1.d0 + epmac) go to 100
  epmac = 2.0d0*epmac

end subroutine getcon
