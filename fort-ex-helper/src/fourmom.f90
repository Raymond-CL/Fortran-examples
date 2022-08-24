module fourmom

  use nrtype
  implicit none
  private

  type, public :: pvec
    private
    real(dp) :: pe,px,py,pz
  contains
    procedure, public :: setp => setpvec
    procedure, public :: printp => printpvec
    procedure, public :: E => getE
    procedure, public :: X => getX
    procedure, public :: Y => getY
    procedure, public :: Z => getZ
    procedure, public :: plus => getplus
    procedure, public :: minus => getminus
    procedure, public :: perp => getperp
    procedure, public :: perp2 => getperp2
    procedure, public :: mag => getmag
    procedure, public :: mag2 => getmag2
    procedure, public :: phi => getphi
    procedure, public :: theta => gettheta
    procedure, public :: costh => getcosth
    procedure, public :: rap => getrap
    procedure, public :: prap => getprap
  end type pvec

contains

  ! setter
  subroutine setpvec(this,e,x,y,z)
  class(pvec), intent(inout) :: this
  real(dp), intent(in) :: e,x,y,z
  this%pe=e; this%px=x; this%py=y; this%pz=z
  end subroutine setpvec

  ! print components
  subroutine printpvec(this,u)
  class(pvec), intent(in) :: this
  integer(i4b) :: u
  write(*,*) this%pe,this%px,this%py,this%pz
  end subroutine printpvec

  ! components of the 4-momentum
  function getE(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%pe
  end function getE 
  function getX(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%px
  end function getX
  function getY(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%py
  end function getY
  function getZ(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%pz
  end function getZ

  ! plus and minus component
  function getplus(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%pe + this%pz
  end function getplus
  function getminus(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%pe - this%pz
  end function getminus
  
  ! transverse component
  function getperp2(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%px*this%px + this%py*this%py
  end function getperp2
  function getperp(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = sqrt(this%perp2())
  end function getperp

  ! magnitude (mass)
  function getmag2(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%plus()*this%minus() - this%perp2()
  end function getmag2
  function getmag(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r,m2
  m2 = this%mag2()
  r = merge(sqrt(m2), -sqrt(-m2), m2.ge.0.0)
  end function getmag

  ! angles (azimuth and polar)
  function getphi(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = atan2(this%py,this%px)
  end function getphi
  function gettheta(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = atan2(this%perp(),this%pz)
  end function gettheta
  function getcosth(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = this%pz/sqrt(this%perp2() + this%pz*this%pz)
  end function getcosth

  ! rapidities
  function getrap(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = log(this%plus()/this%minus()) / 2.0
  end function getrap
  function getprap(this) result(r)
  class(pvec), intent(in) :: this
  real(dp) :: r
  r = -log(tan(this%theta()/2.0))
  end function getprap

end module fourmom

module fourmomop

  use nrtype
  use fourmom
  interface assignment(=)
    module procedure fmomeq
  end interface

contains

  subroutine fmomeq(a,b)
  class(pvec), intent(out) :: a
  class(pvec), intent(in) :: b
  call a%setp(b%E(),b%X(),b%Y(),b%Z())
  end subroutine fmomeq

  function dot(a,b) result(r)
  class(pvec), intent(in) :: a,b
  real(dp) :: r
  r = a%E()*b%E() - a%X()*b%X() - a%Y()*b%Y() - a%Z()*b%Z()
  end function dot

end module fourmomop
