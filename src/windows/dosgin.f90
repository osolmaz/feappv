! Id: dosgin.f90,v 1.1 2000/08/24 20:49:59 rlt Exp $
subroutine dosgin(ix,iy,butn)

  !      * * F E A P * * A Finite Element Analysis Program

  !      Copyright (c) 1984-2012: Robert L. Taylor
  !                               All rights reserved

  !-----------------------------------------------------------------
  !      Purpose: Graphical input of screen coordinates with mouse
  !
  !      Inputs:
  !         none
  !
  !      Outputs:
  !         ix       - x screen coordinate
  !         iy       - y screen coordinate
  !         butn     - Button pressed
  !-----------------------------------------------------------------
  use       DFLIB

  implicit  none

  include  'wdata.h'

  character butn*1
  integer ::   mevent,shftl,shftr,key,result
  integer ::   ix,iy,x,y

  integer ::         idxl,idyl,jfill
  common /vgraph/ idxl,idyl,jfill

  save

  !     Check for left or right button click

  shftl = mouse$ks_shift .or. mouse$ks_lbutton
  shftr = mouse$ks_shift .or. mouse$ks_rbutton

  mevent = mouse$lbuttondown .or. mouse$rbuttondown

  result = waitonmouseevent(mevent,key,x,y)

  if(key == shftl .or. key == shftr) then
     butn = 'm'
  elseif(key == mouse$ks_lbutton) then
     butn = 'l'
  elseif(key == mouse$ks_rbutton) then
     butn = 'r'
  else
     butn = 'e'
  endif

  !     Return coordinates

  ix  = x*idxl
  iy  = 22000 - y*idyl

end subroutine dosgin
