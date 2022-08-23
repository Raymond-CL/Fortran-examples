program main

      use nrtype
      use nr, only: ran0_v,ran1_v,ran2_v
      IMPLICIT NONE
      real :: t1,t2
      integer(i4b) :: n_tot,n_hit,i
      real(sp), dimension (2) :: rnum
      real(sp) :: t,l,y,a,dy
      call cpu_time(t1)

      n_tot = 1E7       ! number of needles
      n_hit = 0         ! initialize hits

      t = 2.0           ! separation
      l = 1.0           ! needle length

      do i=1,n_tot
            !call ran0_v(rnum)
            !call ran1_v(rnum)
            call ran2_v(rnum)       ! generates 2 random numbers
            y = rnum(1)*t           ! needle position
            a = rnum(2)*PI          ! needle orientation
            dy = l/2.0*sin(a)       ! needle reach
            if(y+dy.ge.t .or. y-dy.le.0d0) then
                  n_hit = n_hit+1
            endif
      enddo
      
      write(*,*) 'estimate:', 2d0 * real(n_tot)/real(n_hit) * l/t
      write(*,*) 'exact   :', PI

      call cpu_time(t2)
      write(*,*) 'time elapsed:',t2-t1,'seconds.'

end program main
