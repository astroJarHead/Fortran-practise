!An example of erroneous code
!compile with 
!gfortran -Wunused-variable B.f95 -o Brun
!to check for unsed variables
program B
    implicit none
    ! integer :: i, j=15, k        !dummy indices
    integer :: i, j=15
    do i=1,3
        j = j+1
        write(*,*) i, j
    enddo
    write(*,*) "Bye"
end program B