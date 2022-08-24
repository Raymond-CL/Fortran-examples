module histogram

  use nrtype
  implicit none
  private

  type, public :: hist
    private
    integer(i1b) :: ind,histsize
    integer(i4b) :: hitcount,misscount
    logical :: logplot
    character(len=40)  :: title
    real(sp) :: histmin,histmax,histbin
    real(sp) :: total
    real(sp), allocatable :: histdata(:)
  contains
    procedure, public :: geti => getind
    procedure, public :: seth => sethist
    procedure, public :: fillh => fillhist
    procedure, public :: printh => printhist
  end type hist

contains

  function getind(this) result(i)
  class(hist), intent(in) :: this
  integer(i4b) :: i
  i = this%ind
  end function getind

  subroutine sethist(this,i,islog,s,n,hmin,hmax)
  class(hist), intent(inout) :: this
  integer(i4b), intent(in) :: i,n
  logical, intent(in) :: islog
  character(len=*), intent(in) :: s
  real(sp), intent(in) :: hmin,hmax
  integer(i1b) :: err
  this%ind = i
  this%logplot = islog
  this%title = s
  this%histsize = n
  allocate(this%histdata(n),stat=err) ! dynamic hist. allocate
  if(err.ne.0) then
    write(*,*) 'mem allocation err @ histogram mod:',err
    call exit(4)
  endif
  this%histdata = 0.0
  this%histmin = hmin
  this%histmax = hmax
  if(this%logplot) then
    if(this%histmin .le. 0d0) then
      write(*,*) 'histmin<=0 for logplot, reset to 1E-10.'
      this%histmin = 1e-10
    endif
    this%histbin = (log(this%histmax)-log(this%histmin)) &
           /this%histsize
  else
    this%histbin = (this%histmax-this%histmin)/this%histsize
  endif
  this%hitcount = 0
  this%misscount = 0
  this%total = 0.0
  end subroutine sethist
  
  subroutine fillhist(this,x,y)
  class(hist), intent(inout) :: this
  real(sp), intent(in) :: x,y
  integer(i1b) :: i
  if(x.lt.this%histmin .or. x.ge.this%histmax) then
    this%misscount = this%misscount + 1
  else
    if(this%logplot) then
      i = floor( (log(x)-log(this%histmin))/this%histbin+1 )
      if(i.le.0) write(*,*) i,x,y
      this%histdata(i) = this%histdata(i) + y
    else
      i = floor( (x-this%histmin)/this%histbin+1 )
      if(i.le.0) write(*,*) i,x,y
      this%histdata(i) = this%histdata(i) + y
    endif
    this%hitcount = this%hitcount + 1
  endif
  end subroutine fillhist

  subroutine printhist(this,u)
  class(hist), intent(inout) :: this
  integer(i4b) :: u,i
  real(sp) :: histL,histR,histmid
  character(len=*), parameter :: tab=char(9)
  write(u,*) 'histogram number:',this%ind
  write(u,*) 'title:',trim(this%title)
  write(u,*) 'hit:',this%hitcount, &
            'miss:',this%misscount
  write(u,*) 'efficiency:', &
         real(this%hitcount) * 100.0 / &
         real(this%hitcount+this%misscount),'%'
  write(u,*) 'bin-left',tab,tab, &
            'bin-mid',tab,tab, &
            'bin-right',tab,'result'
  this%total = 0.0
  do i = 1,this%histsize
    if(this%logplot) then
      histL = this%histmin * exp((i-1)*this%histbin)
      histR = this%histmin * exp(i*this%histbin)
      histmid = (histL + histR)/2d0
      write(u,*) histL,histmid,histR, &
       this%histdata(i)/(histR-histL)
    else
      histL = this%histmin + (i-1)*this%histbin
      histR = this%histmin + i*this%histbin
      histmid = (histL + histR)/2d0
      write(u,*) histL,histmid,histR, &
       this%histdata(i)/this%histbin
    endif
    this%total = this%total + this%histdata(i)
  enddo
  write(u,*) 'integrated total:',this%total
  write(u,*) new_line('a')
  end subroutine printhist

end module histogram


module bookkeep

  use nrtype
  use histogram
  implicit none
  public
  type(hist), dimension(:), allocatable :: book
  integer(i1b) :: booksize
  
  contains
  
  subroutine initbook()
  allocate(book(0)) ! dynamic book create
  booksize = 0
  end subroutine initbook
  
  subroutine newbook(i,l,s,n,hmin,hmax)
  integer(i4b), intent(in) :: i,n
  logical, intent(in) :: l
  character(len=*), intent(in) :: s
  real(sp), intent(in) :: hmin,hmax
  type(hist) :: htemp
  call htemp%seth(i,l,s,n,hmin,hmax)
  book = [book,htemp]     ! dynamic vector push
  booksize = booksize+1
  end subroutine newbook
  
  subroutine fillbook(i,x,y)
  integer(i4b), intent(in) :: i
  real(sp), intent(in) :: x,y
  integer(i4b) :: j
  do j=1,booksize
    if( book(j)%geti() .eq. i) then
      call book(j)%fillh(x,y)
      exit
    endif
  enddo
  end subroutine fillbook
  
  subroutine printbook(u)
  integer(i4b) u,i
  do i = 1,booksize
    call book(i)%printh(u)
  enddo
  end subroutine printbook

end module bookkeep

