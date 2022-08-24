program main

implicit none

! compile with: gfortran main.f90

write(*,*) 'test to display colors on terminal'

write(*,*) ''//achar(27)//'[90mdark grey'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[91mdark peach'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[92mlight green'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[93mlight yellow'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[94mlight blue'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[95mpink'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[96mlight aqua'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[97mpearl white'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[30mblack'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[31mred'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[32mgreen'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[33myellow'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[34mblue'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[35mpurple'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[36maqua'//achar(27)//'[0m'
write(*,*) ''//achar(27)//'[37mgrey'//achar(27)//'[0m'

end program main
