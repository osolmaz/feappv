c$Id:$
      subroutine pointnd(d,ul,s,r,ndf,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose:  Point Stiffness & Mass ELEMENT

c     Input data: (isw = 1)

c       Record ('mass',m)
c         m - Mass / dof

c       Record ('spri',k)
c         k - Spring / dof

c     Outputs: (isw = 4)
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cdata.h'
      include  'eldata.h'
      include  'eltran.h'
      include  'iofile.h'

      logical   errck, tinput, pcomp, spring, masses
      character vtype*15
      integer   ndf, nst, isw, i
      real*8    d(*),ul(ndf,nen,*),s(nst,*),r(ndf,*),td(6)

      save

c     INPUT MATERIAL PROPERTIES

      if(isw.eq.1) then

c       Record:

        spring = .false.
        masses = .false.

        vtype = 'start'
        do while(.not.pcomp(vtype,'    ',4))

          errck = tinput(vtype,1,td,6)

c         Mass parameter

          if    (pcomp(vtype,'mass',4)) then
            
            do i = 1,ndf
              d(i) = td(i)
            end do ! i
            masses = .true.

c         Spring parameter

          elseif(pcomp(vtype,'spri',4)) then

            do i = 1,ndf
              d(i+10) = td(i)
            end do ! i
            spring = .true.

c         Inadmissible data

          elseif(.not.pcomp(vtype,'    ',4)) then
            write(  *,4000) vtype
            write(iow,4000) vtype

          end if

        end do ! while

c       Output material properties

        write(iow,2000)
        if(masses) then
          write(iow,2001) (i,d(i),i=1,ndf)
        endif
        if(spring) then
          write(iow,2002) (i,d(i+10),i=1,ndf)
        endif

c       Set for no plots

        call pltpt1(iel)

c     CHECK ELEMENTS

      elseif(isw.eq.2) then

c     COMPUTE ELEMENT STIFFNESS AND RESIDUAL ARRAYS

      elseif(isw.eq.3  .or. isw.eq.6 ) then

        do i = 1,ndf
          r(i,1) = -d(i)*ul(i,1,5) - d(i+10)*ul(i,1,1)
          s(i,i) =  d(i)*ctan(3)   + d(i+10)*ctan(1)
        end do ! i

c     COMPUTE MASS MATRIX

      elseif(isw.eq.5) then

        do i = 1,ndf
          r(i,1) = d(i)
          s(i,i) = d(i)
        end do ! i

      endif

c     I/O Formats

2000  format(9x,'P o i n t    E l e m e n t'/)
2001  format(14x,'MASS(',i1,')     :',1p,1e12.4)
2002  format(14x,'SPRING(',i1,')   :',1p,1e12.4)

4000  format(' *WARNING* Property: ',a,' is not valid')

      end
