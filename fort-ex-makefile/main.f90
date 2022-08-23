program main
  use nrtype
  use nr
  use radius
  implicit none
  integer(i4b) :: init,itmax,ncall,ndim,nprn
  real(sp) :: avgi,chi2a,sd
  real(sp), dimension(20) :: region
  interface
    function fxn(pt,wgt)
    use nrtype
    implicit none
    real(sp), dimension(:), intent(in) :: pt
    real(sp), intent(in) :: wgt
    real(sp) :: fxn
    end function fxn
  end interface
  real(sp) :: ans

  rad = 1d0
  ndim = 3; ncall = 1E5; itmax = 10; nprn = 0
  avgi = 0d0; sd = 0d0; chi2a = 0d0
  region(1:ndim) = -rad
  region(1+ndim:2*ndim) = +rad

  ! warm-up run
  init = -1
  call vegas(region(1:2*ndim),fxn,init,ncall,itmax,nprn,avgi,sd,chi2a)
  ! main run
  init = +1
  call vegas(region(1:2*ndim),fxn,init,ncall*10,1,nprn,avgi,sd,chi2a)

  ! analytic result
  ans = PI**(ndim/2d0) / gamma(ndim/2d0+1d0) * rad**ndim

  write(*,*) new_line('a')
  write(*,*) 'The volume of a hyper sphere'
  write(*,*) 'with dimension:',ndim
  write(*,*) 'and radius:',rad
  write(*,*) 'is:'
  write(*,*) avgi,'integrated result'
  write(*,*) ans,'analytic result'

end program main

