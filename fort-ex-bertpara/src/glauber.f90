module glauber

! simple Glauber geometry module
! includes density, thickness and overlap function
! use to normalize AA calculations to pp
! \int dxdy TA(x,y) = mass
! \int dxdy TAA(x,y,b=0) = taa0
! \int dxdyd^2b TAA(x,y,b) = mass^2

use nrtype
use gauss
implicit none
private
real(sp) :: rad,dr,d0,taa0
real(sp), parameter :: maxsize = 15d0
integer(i4b), parameter :: gsn = 50
public :: taa0,setgeo,TAA,TA

contains

subroutine setgeo(A)
  integer :: A
  if(A.eq.238) then ! Uranium 238
    rad = 6.8054d0
    dr = 0.605d0
    d0 = 0.1673d0
    taa0 = 0d0
  elseif(A.eq.208) then ! Lead 208
    rad = 6.62d0
    dr = 0.546d0
    d0 = 0.16039d0
    taa0 = 304.26d0
  elseif(A.eq.207) then ! Lead 207
    rad = 6.62d0
    dr = 0.546d0
    d0 = 0.15962d0
    taa0 = 0d0
  elseif(A.eq.197) then ! Gold 197
    rad = 6.38d0
    dr = 0.535d0
    d0 = 0.169346d0
    taa0 = 293.19d0
  elseif(A.eq.63) then ! Copper 63
    rad = 4.163d0
    dr = 0.606d0
    d0 = 0.1739d0
    taa0 = 57.74d0
  elseif(A.eq.27) then ! Aluminium 27
    rad = 3.07d0
    dr = 0.519d0
    d0 = 0.1736d0
    taa0 = 0d0
  else
    rad = 0d0
    dr = 0d0
    d0 = 0d0
    taa0 = 0d0
  endif
  ! extracted from: Ultrarelativistic heavy-ion collisions, Ramona
  ! Vogt, 2007, Elsevier, ISBN13:9780444521965
  ! previous mass parameterization
  !mass = 208d0 
  !rad = 1.12d0*mass**(1d0/3d0)-0.86d0*mass**(-1d0/3d0)
  !density0 = 0.1700d0
end subroutine setgeo

function Density(x,y,z)
  real(sp) :: Density,x,y,z,R
  R = sqrt(x*x+y*y+z*z)
  Density = d0/(1d0 + exp((R-rad)/dr))
  return
end function Density

function TA(x,y)
  real(sp) :: TA,x,y
  real(sp) :: zmin,zmax,z(gsn),zw(gsn)
  integer(i4b) :: iz
  zmin = -maxsize
  zmax = +maxsize
  call gauss50(zmin,zmax,z,zw)
  TA = 0d0
  do iz = 1,gsn
    TA = TA + Density(x,y,z(iz)) * zw(iz)
  enddo
  return
end function TA

function TAA(x,y,b)
  real(sp) :: TAA,x,y,b
  real(sp) :: x1,den1,ta1
  real(sp) :: x2,den2,ta2
  real(sp) :: zmin,zmax,z(gsn),zw(gsn)
  integer(i4b) :: iz
  TAA = 0d0
  zmin = -maxsize
  zmax = +maxsize
  call gauss38(zmin,zmax,z,zw)
  ta1 = 0d0
  ta2 = 0d0
  do iz = 1,gsn
    x1 = x - b/2d0
    x2 = x + b/2d0
    den1 = Density(x1,y,z(iz))
    den2 = Density(x2,y,z(iz))
    ta1 = ta1 + den1*zw(iz)
    ta2 = ta2 + den2*zw(iz)
  enddo
  TAA = ta1*ta2
  return
end function TAA

end module glauber
