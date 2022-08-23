function fxn(pt,wgt)
  use nrtype
  use radius
  implicit none
  real(sp), dimension(:), intent(in) :: pt
  real(sp), intent(in) :: wgt
  real(sp) :: fxn
  fxn = merge(1d0, 0d0, sum(pt(:)*pt(:)).le.rad*rad)
end function fxn
