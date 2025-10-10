PROGRAM characters

    ! Example for manipulating strings with concatenantion
    ! and extraction of substrings in FORTRAN

    implicit none

    character (len=*), parameter :: headline = "Man United will win the league?" 
    character (len=*), parameter :: fname = "Steve", lname = "Smith"
    character (len=11) :: fullname

    ! *** Example of concatenation of two strings ***
    fullname = fname//lname
    write(*,*) "*** Concatenation of two strings example"
    print*,fullname

    ! *** Concatenation of a string a character and a string ***
    fullname = fname//" "//lname
    write(*,*) "*** Concatenation of two strings between a character example"
    print*,fullname

    ! *** Example of a substring ***
    write(*,*) "*** Extraction of a substring"    
    print*,headline(5:10)
    
END PROGRAM characters