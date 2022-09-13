program main

  implicit none
  character(len=*), parameter :: phrase = '1234 abcd ABCD +-*/'
  integer(1) :: code,i,j

  write(*,*) 'ASCII/ANSI character table:',new_line('a')
  write(*,*) '     0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15'
  do i = 2,7
    write(*,'(i3.3,a)',advance='no') i*16,'+'
    do j = 0,15
      write(*,'(a3)',advance='no') achar(i*16 + j)
    enddo
    write(*,*)
  enddo 
  write(*,*)

  write(*,*) 'color code table:',new_line('a')
  do code = 30, 37
    write(*,*) 'code:', code, ', sample: ', color(phrase, code)
  enddo
  write(*,*)

  do code = 90, 97
    write(*,*) 'code:', code, ', sample: ', color(phrase, code)
  enddo
  write(*,*)

contains

  function color(str, code) result(res)
  character(len=*), intent(in) :: str
  integer(1), intent(in) :: code
  character(len=:), allocatable :: res
  character(len=2) :: cs
  write(cs,'(i2.2)') code
  res = achar(27) // '[' // cs // 'm' // str // achar(27) // '[0m'
  return
  end function

end program main

