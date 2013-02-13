c$Id:$
      subroutine pload(id,f1,dr,prop,flg)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Form nodal load vector for current time

c      Inputs:
c         id(*)    - Equation numbers for degree of freedom
c         prop     - Total proportional load level
c         flg      - Flag: Form residual if true; else reactions

c      Outputs:
c         f1(*)    - Total nodal load for t_n+1
c         dr(*)    - Total reaction/residual
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'ddata.h'
      include  'fdata.h'
      include  'p_int.h'
      include  'prld1.h'
      include  'sdata.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   flg
      integer   j,n, ipro
      integer   id(*)
      real*8    prop,thn, f1(nneq,*),dr(*)

c     Set force vectors for t_n+1

      fl(11) = .false.
      do n = 1,nneq

c               F
        fp(1) = np(27) + n - 1
        fp(2) = fp(1) + nneq
c               FU
        fp(3) = np(28) + n - 1
        fp(4) = fp(3) + nneq
c               F0
        fp(5) = fp(4) + nneq
        fp(6) = fp(5) + nneq

c                   FPRO
        ipro = mr(np(29)+n-1)
        if(ipro.eq.0) then     ! {
          if(id(n).gt.0) then
            f1(n,1) = hr(fp(1))*prop  + hr(fp(5)) + hr(fp(3))
          else
            f1(n,1) = hr(fp(2))*prop  + hr(fp(6)) + hr(fp(4))
          endif
          f1(n,3)   = hr(fp(1))*prop + hr(fp(5)) + hr(fp(3))
        else
          if(id(n).gt.0) then
            f1(n,1) = hr(fp(1))*prldv(ipro) + hr(fp(5)) + hr(fp(3))
          else
            f1(n,1) = hr(fp(2))*prldv(ipro) + hr(fp(6)) + hr(fp(4))
          endif
          f1(n,3) = hr(fp(1))*prldv(ipro) + hr(fp(5)) + hr(fp(3))
        endif                  ! if }
      end do ! n

c     Initialize residual/reaction

      if(flg) then
        do n = 1,nneq
          dr(n) = 0.0d0
        end  do ! n
      endif

c     Compute interpolated load vector

      thn = 1.0d0 - theta(3)

      do n = 1,nneq
        j = id(n)
        if(j.gt.0) then
          if(flg) then
            dr(j) = dr(j) + theta(3)*f1(n,1) + thn*f1(n,2)
          else
            dr(n) =         theta(3)*f1(n,1) + thn*f1(n,2)
          endif
        endif
      end do ! n

      end
