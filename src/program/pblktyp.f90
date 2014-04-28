! Id:$
logical function pblktyp(layer,td, ntyp,ns,mab)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----[--.----+----.----+----.-----------------------------------------]
  !     Modification log                                Date (dd/mm/year)
  !       Original version                                    01/11/2006
  !-----[--.----+----.----+----.-----------------------------------------]
  !      Purpose: Describe element type and number of nodes/element.

  !      Inputs:
  !        layer       - Type of element
  !        td(*)       - Number nodes on element

  !      Outputs:
  !        ntyp        - Element type
  !        ns          - Generation type
  !        mab         - Material number
  !-----[--.----+----.----+----.-----------------------------------------]
  implicit   none

  include   'cdata.h'
  include   'iofile.h'
  include   'ublk1.h'

  logical ::    pcomp
  character  layer*15
  integer ::    ntyp,ns,mab, n,j
  real*8 ::     td(5)

  !     Line elements

  if    (pcomp(layer,'line',4)) then
     pblktyp = .false. 
     n       = max(2,nint(td(1)))
     if(n > nen) go to 900
     if(n == 2) then
        ns = 1
     elseif(n == 3) then
        ns = 2
     elseif(n == 4) then
        ns = 3
     endif

     !     Triangular elements

  elseif(pcomp(layer,'tria',4)) then
     pblktyp = .false. 
     n       = max(3,nint(td(1)))
     if(n > nen) go to 900
     j       = nint(td(2))
     if(n == 3) then              ! 3-node linear triangle
        if(j >= 0) then
           ntyp = min(6,max(1,j))
        elseif(j == -1) then
           ntyp = -1
        endif
     elseif(n == 6) then          ! 6-node linear triangle
        ntyp = 7
     elseif(n == 7) then          ! 7-node linear triangle
        ntyp = -7
     endif

     !     Quadrilateral elements

  elseif(pcomp(layer,'quad',4)) then
     pblktyp = .false. 
     n       = max(4,nint(td(1)))
     if(n > nen) go to 900
     if(n == 4) then       ! 4-node Linear quadrilateral
        ntyp = 0
     elseif(n == 8) then  !  8-node Serendipity quadratic quad
        ntyp = 8
     elseif(n == 9) then  !  9-node Lagrangian  quadratic quad
        ntyp = 9
     elseif(n == 16) then  ! 16-node Lagrangian  cubic     quad
        ntyp = 16
     endif

     !     Tetrahedral elements

  elseif(pcomp(layer,'tetr',4)) then
     pblktyp = .false. 
     n       = max(4,nint(td(1)))
     if(n > nen) go to 900
     if(n == 4) then              !  4-node linear tetrahedron
        ntyp = 11
     elseif(n == 10) then         ! 10-node quadratic tetrahedron
        ntyp = 13
     elseif(n == 11) then         ! 11-node quadratic tetrahedron
        ntyp = 15
     endif

     !     Brick elements

  elseif(pcomp(layer,'bric',4)) then
     pblktyp = .false. 
     n       = max(8,nint(td(1)))
     if(n > nen) go to 900
     if(n == 8) then       !  8-node linear brick
        ntyp = 10
     elseif(n == 20) then  ! 20-node Serendipity quadratic brick
        ntyp = 14
     elseif(n == 27) then  ! 27-node Lagrangian  quadratic brick
        ntyp = 12
     endif

     !     User elements

  elseif(pcomp(layer,'user',4)) then

     pblktyp = .false. 
     n       = max(1,nint(td(1)))
     if(n > nen) go to 900
     ntyp = 30+n
     ublknum = max(1,nint(td(2)))
     do j = 1,3
        ublkdat(j,ublknum) = nint(td(j+2))
     end do ! j

     !     Material number

  elseif(pcomp(layer,'mate',4)) then

     pblktyp = .false. 
     mab     = nint(td(1))

     !     No match

  else
     pblktyp = .true. 
     n       =  0
  endif

  return

  !     Error on element type

900 write(iow,3000) layer,n,nen
  if(ior > 0) then
     call plstop()
  else
     write(*,3000) layer,n,nen
  endif

  !     Formats

3000 format('--> ERROR in BLOCk generation: Element type ',a/ &
       '    with ',i3,' nodes greater than ',i3,' specified', &
       ' on control record')

end function pblktyp
