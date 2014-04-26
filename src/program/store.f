c$Id:$
      subroutine store(v,w,nv,itrn)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2012: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Store/retrieve BFGS vectores in blank common arrays

c      Inputs:
c         v(*),w(*) - BFGS Vectors to save
c         nv        - Number of vector pair to save
c         itrn      - Switch: = 1 to save; = 2 to retrieve

c      Outputs:
c         none      - Saves in hr(*):  (itrn = 1)
c         v(*),w(*) - BFGS Vector   :  (itrn = 2)
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'p_point.h'
      include  'pointer.h'
      include  'comblk.h'

      integer   nv, itrn, ns

      real*8    v(*),w(*)

      save

      ns = (neq+neq)*(nv-1)

      if(itrn.eq.1) then
        point = np(71) + ns
        call pmove(v,hr(point),neq)
        point = point + neq
        call pmove(w,hr(point),neq)
      elseif(itrn.eq.2) then
        point = np(71) + ns
        call pmove(hr(point),v,neq)
        point = point + neq
        call pmove(hr(point),w,neq)
      endif

      end
