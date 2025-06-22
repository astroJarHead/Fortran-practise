! Estimate winds power

!========= main program =============
program windpowermain
    
    ! variable declarations
    implicit none         !enforce strong typing 
    real :: power = 0.0   ! power output (watts) from turbine

    !setup
    write(*,*) "Welcome to Wind Power"  !welcome user

    !save results 
    write(*,*) "power = ", power  ! display result

end program windpowermain