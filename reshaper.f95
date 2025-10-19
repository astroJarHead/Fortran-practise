! Handout 7 Exercise 1
! Take a 1-dimensional array and re-shape it to 
! a 4x4 array. The 1-d array begins with integers in 
! nmerical order 1 -> 16 and the 4x4 array maintains
! the numerical order. 

program reshaper
    implicit none

    integer, dimension(16) :: aa  ! 1-d row with 16 columns
    integer, dimension(4,4) :: bb ! 2-d 4x4 matrix
    integer, dimension(4,4) :: bb_transp ! transpose of bb 
    real :: start, finish       ! cpu time variables

    call cpu_time(start)    ! begin cpu timing 

    write(*,*)
    write(*,*) "*****PROGRAM RESHAPER*****"
    write(*,*) " A Fortran 95 to test matrix reshaping"
    write(*,*) " operations from Handout #7."
    write(*,*)

    ! Create the 1-d row array
    aa = (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/)
    print '("Array aa: ",16i3)',aa

    ! Re-shape aa -> 'box' shape
    bb = reshape(aa,(/4,4/))
    call printi('Array bb reshaped from aa: ',bb)

    ! Now transpose bb -> bb_transp
    bb_transp = transpose(bb)
    call printi('Transpose of bb: ',bb_transp)
    call cpu_time(finish)
    ! Inform user of performance time
    write(*,*)
    print '(" CPU Time = ",ES12.3," seconds.")',finish-start
    !print '("CPU Time = ",f9.4," seconds.")',finish-start
    write(*,*) "*****END PROGRAM RESHAPER*****"
    write(*,*)

contains

subroutine printi(title,arr)
    implicit none

    !@(#) print small 2d integer arrays in row-column format

    character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
    character(len=*),intent(in)  :: title
    integer,intent(in)           :: arr(:,:)
    integer                      :: i
    character(len=:),allocatable :: biggest

    print all
    print all, trim(title),':(',shape(arr),')'  ! print title
    biggest='           '  ! make buffer to write integer into
    ! find how many characters to use for integers
    write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
    ! use this format to write a row
    biggest='(" > [",*(i'//trim(biggest)//':,","))'
    ! print one row of array at a time
    do i=1,size(arr,dim=1)
       write(*,fmt=biggest,advance='no')arr(i,:)
       write(*,'(" ]")')
    enddo

end subroutine printi


end program reshaper