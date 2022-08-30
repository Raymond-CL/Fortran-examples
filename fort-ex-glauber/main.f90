! main driver program
program main
  use glauber
  implicit none
  interface 
    subroutine thickness_profile
    end subroutine thickness_profile
    subroutine thickness_integrate
    end subroutine thickness_integrate
    subroutine overlap_profile
    end subroutine overlap_profile
    subroutine overlap_integrate
    end subroutine overlap_integrate
  end interface
  write(*,*) 'Glauber module test'
  call setgeo(208)
  !call thickness_profile()
  !call thickness_integrate()
  !call overlap_profile()
  !call overlap_integrate()
end program main



! procedure to print thickness profile
subroutine thickness_profile()
  use nrtype
  use glauber
  implicit none
  integer(i4b) :: i,nx
  real(sp) :: xmax,xbin,xi
  xmax = 20d0
  nx = 20
  xbin = xmax/dble(nx)
  do i=0,+nx
    xi = i*xbin
    write(*,*) xi, TA(xi,0.0)
  enddo
end subroutine thickness_profile



! procedure to calculate mass
subroutine thickness_integrate()
  use nrtype
  use nr
  use glauber
  implicit none
  integer(i4b) :: init,itmax,ncall,ndim,nprn
  real(sp) :: avgi,chi2a,sd
  real(sp), dimension(4) :: region
  real(sp) xmax,ymax
  ncall = 10000; itmax = 5
  nprn = -1
  ndim = 2
  xmax = 20d0
  ymax = 20d0
  region(1) = -xmax
  region(2) = -ymax
  region(3) = +xmax
  region(4) = +ymax
  init = -1
  call vegas(region,fxn_thick,init,ncall,itmax,nprn,avgi,sd,chi2a)
  init = +1
  call vegas(region,fxn_thick,init,ncall,itmax,nprn,avgi,sd,chi2a)
  write(*,*) avgi,sd
contains
  function fxn_thick(pt,wgt)
  use nrtype
  use glauber
  implicit none
  real(sp), dimension(:), intent(in) :: pt
  real(sp), intent(in) :: wgt
  real(sp) :: fxn_thick
  real(sp) :: x,y
  x = pt(1);  y = pt(2)
  fxn_thick = TA(x,y)
  end function fxn_thick
end subroutine thickness_integrate



! procedure to print overlap profile
subroutine overlap_profile()
  use nrtype
  use nr
  use glauber
  implicit none
  integer(i4b) :: init,itmax,ncall,ndim,nprn
  real(sp) :: avgi,chi2a,sd
  real(sp), dimension(4) :: region
  real(sp) :: xmax,ymax
  integer(i4b) :: i,nb
  real(sp) :: bmax,bbin,bi
  common /bperp/ bi
  ncall = 100000; itmax = 5
  nprn = -1
  ndim = 2
  xmax = 20d0
  ymax = 20d0
  region(1) = -xmax
  region(2) = -ymax
  region(3) = +xmax
  region(4) = +ymax
  bmax = 20d0
  nb = 20
  bbin = bmax/dble(nb)
  do i=0,+nb
    bi = i*bbin
    init = -1
    call vegas(region,fxn_op,init,ncall,itmax,nprn,avgi,sd,chi2a)
    init = +1
    call vegas(region,fxn_op,init,ncall,itmax,nprn,avgi,sd,chi2a)
    write(*,*) bi, avgi
  enddo
contains
  function fxn_op(pt,wgt)
  use nrtype
  use glauber
  implicit none
  real(sp), dimension(:), intent(in) :: pt
  real(sp), intent(in) :: wgt
  real(sp) :: fxn_op
  real(sp) :: x,y,bi
  common /bperp/ bi
  x = pt(1);  y = pt(2)
  fxn_op = TAA(x,y,bi)
  end function fxn_op
end subroutine overlap_profile



! procedure to calculate mass^2
subroutine overlap_integrate()
  use nrtype
  use nr
  use glauber
  implicit none
  integer(i4b) :: init,itmax,ncall,ndim,nprn
  real(sp) :: avgi,chi2a,sd
  real(sp), dimension(8) :: region
  real(sp) xmax,ymax,bxmax,bymax
  ncall = 100000; itmax = 5
  nprn = -1
  ndim = 4
  xmax = 40d0;  ymax = 40d0
  bxmax = 20d0; bymax = 20d0
  region(1) = -xmax;  region(ndim+1) = +xmax
  region(2) = -ymax;  region(ndim+2) = +ymax
  region(3) = -bxmax; region(ndim+3) = +bxmax
  region(4) = -bymax; region(ndim+4) = +bymax
  init = -1
  call vegas(region,fxn_over,init,ncall,itmax,nprn,avgi,sd,chi2a)
  init = +1
  call vegas(region,fxn_over,init,ncall,itmax,nprn,avgi,sd,chi2a)
  write(*,*) avgi,sd
contains
  function fxn_over(pt,wgt)
  use nrtype
  use glauber
  implicit none
  real(sp), dimension(:), intent(in) :: pt
  real(sp), intent(in) :: wgt
  real(sp) :: fxn_over
  real(sp) :: x,y,bx,by,b
  x = pt(1);  y = pt(2)
  bx = pt(3); by = pt(4)
  b = sqrt(bx*bx+by*by)
  fxn_over = TAA(x,y,b)
  end function fxn_over
end subroutine overlap_integrate
