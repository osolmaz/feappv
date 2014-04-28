! Id:$
subroutine presol(cfr, error)

  !      * * F E A P * * A Finite Element Analysis Program

  !....  Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  implicit   none

  include   'allotd.h'
  include   'cdata.h'
  include   'compas.h'
  include   'eqsym.h'
  include   'fdata.h'
  include   'iofile.h'
  include   'iodata.h'
  include   'ldata.h'
  include   'ndata.h'
  include   'pathn.h'
  include   'prflag.h'
  include   'psize.h'
  include   'setups.h'

  include   'p_point.h'

  include   'pointer.h'
  include   'comblk.h'

  logical ::    cfr, error, palloc, setvar, flags(5)
  integer ::    ip,kp,len

  !     Active equations case


  !     Program initialization
  if(solver) then
     error    = .false. 
     floop(2) = .true. 

     !       Symmetric part allocation

     if( .not. compfl) then
        if(np(1) > 0) then
           call pgetd('TANGS',point,kp,ip,setvar)
           len = kp*ipr
        else
           len = 0
        endif
        kp = (mr(np(21)+neq-1)+neq)
        setvar = palloc(1,'TANGS',kp,2)
        if( .not. setvar) then
           write(iow,3007) 'Symmetric',kp*ipr
           if(ior < 0) then
              write(*,3007) 'Symmetric',kp*ipr
           endif
           if(ior < 0 .and. l == 2) then
              error = .true. 
              return
           else
              call plstop()
           endif
        endif
        call pzero(hr(np(1)),kp)

        !         Unsymmetric part allocation

        if(cfr) then
           if(np(5) > 0) then
              call pgetd('UTANG',point,kp,ip,setvar)
              len = kp*ipr
           else
              len = 0
           endif
           kp = max(1,(mr(np(21)+neq-1)))
           setvar = palloc(5,'UTANG',kp,2)
           if( .not. setvar) then
              write(iow,3007) 'Unsymmetric',kp*ipr
              if(ior < 0) then
                 write(*,3007) 'Unsymmetric',kp*ipr
              endif
              if(ior < 0 .and. l == 2) then
                 error = .true. 
                 return
              else
                 call plstop()
              endif
           endif
           call pzero(hr(np(5)),kp)
           na   = np(1)
           nau  = na + neq
           nal  = np(5)
           neqs = 1
        else
           na   = np(1)
           nau  = na + neq
           nal  = nau
           neqs = neq
        endif
        fl(4) = .false. 
     else
        neqs = neq
        call pzero(hr(np(1)),nnr)
     endif

  else
     flags(1) = .true. 
     flags(2) = .false. 
     flags(3) =  cfr
     flags(4) = .false. 
     flags(5) = .false. 
     call usolve(flags,hr(1)) ! N.B. hr(*) should not be modified
     error = flags(5)
  endif

  !     Format

3007 format(' *WARNING* Insufficient Storage for ',a,' Profile:'/ &
       '            Array size =',i10)

end subroutine presol
