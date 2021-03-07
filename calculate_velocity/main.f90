!------------------------------------------------------------------------------
! FUNCTION: apply_linear_acceleration
!------------------------------------------------------------------------------
!> Apply linear acceleration to a velocity, given a time delta
!!
!! @note returns mm/s, example of fortran documentation
!!
!! @param[in] time_delta   dt, in ms
!! @param[in] velocity     mm/s
!! @param[in] acceleration mm/s/s
!------------------------------------------------------------------------------
function apply_linear_acceleration(time_delta, velocity, acceleration) result(r)
    real, intent(in)    :: time_delta   ! input, s
    real, intent(in)    :: velocity     ! input, m/s
    real, intent(in)    :: acceleration ! input, m/s/s
    real                :: r            ! output, m/s
    ! velocity = initial_velocty + (dt * acceleration)
    r = (time_delta * acceleration) + velocity
end function

function calculate_position(time_delta, initial_position, velocity) result(r)
    real, intent(in)    :: time_delta       ! input, s
    real, intent(in)    :: initial_position ! input, m
    real, intent(in)    :: velocity         ! input, m/s
    real                :: r                ! output, m
    ! position = (velocity * dt) + intitial_position
    r = (velocity * time_delta) + initial_position
end function

function calculate_potential_energy(mass, position, acceleration_to_rest) result(r)
    integer, intent(in) :: mass                 ! input, kg
    real, intent(in)    :: position             ! input, m
    real, intent(in)    :: acceleration_to_rest ! input, m/s/s
    real                :: r                    ! output, newtons
    ! potential_energy = mass * acceleration_to_rest * distance_from_rest
    r = mass * acceleration_to_rest * position
end function

function calculate_kinetic_energy(mass, velocity) result(r)
    integer, intent(in) :: mass     ! input, kg
    real, intent(in)    :: velocity ! input, m/s
    real                :: r        ! output, newtons
    ! kinetic_energy = (1/2) mass * (velocity * velocity)
    r = (mass * (velocity ** 2))/2
end function

function calculate_total_energy(potential_energy, kinetic_energy) result(r)
    real, intent(in)    :: potential_energy    ! input, newtons
    real, intent(in)    :: kinetic_energy      ! input, newtons
    real                :: r                   ! output, newtons
    r = potential_energy + kinetic_energy
 end function

program calculate_velocity
    implicit none
    real :: apply_linear_acceleration
    real :: calculate_position
    real :: calculate_potential_energy
    real :: calculate_kinetic_energy
    real :: calculate_total_energy

    real    :: ACCELERATION     = -9.81 ! m/s/s
    real    :: DT               = 0.01  ! s
    integer :: mass             = 1     ! kg
    real    :: initial_velocty  = 10    ! m/s
    real    :: current_velocity = 10    ! m/s
    real    :: initial_position = 2     ! m
    real    :: current_position = 2     ! m
    integer :: time             = 0     ! s
    real    :: potential_energy = 0     ! newtons
    real    :: kinetic_energy   = 0     ! newtons
    real    :: total_energy     = 0     ! newtons

    potential_energy = calculate_potential_energy(mass, initial_position, -ACCELERATION)
    kinetic_energy   = calculate_kinetic_energy(mass, initial_velocty)
    total_energy     = calculate_total_energy(potential_energy, kinetic_energy)

    open (9,  file = "output.csv", status = "new")
    ! write(9,  '(A)') "time, velocity"
    write(9, *) "time, acceleration, velocity, position, potential_energy, kinetic_energy, total_energy"
    write(9, *) 0, ", ", 0, ", ", initial_velocty , ", ", current_velocity, ", ", &
                potential_energy, ", ", kinetic_energy, ", ", total_energy

    do while (total_energy > 0.001 .OR. time .EQ. 0)
        current_velocity = apply_linear_acceleration(DT, current_velocity, ACCELERATION)
        current_position = calculate_position(DT, current_position, current_velocity)
        potential_energy = calculate_potential_energy(mass, current_position, -ACCELERATION)
        kinetic_energy   = calculate_kinetic_energy(mass, current_velocity)
        total_energy     = calculate_total_energy(potential_energy, kinetic_energy)

        time = time + 1

        print*, "time: ", time, " current_velocity: ", current_velocity, " current_position: ", current_position, &
                " current_potential_energy: ", potential_energy, " current_kinetic_energy: ", kinetic_energy,     &
                " total_energy: ", total_energy

        ! write(9,  '(F0.0, A, F0.0)') time, ", ", current_velocity
        write(9, *) time, ", ", ACCELERATION, ", ", current_velocity, ", ", current_position, ", ",               &
                    potential_energy, ", ", kinetic_energy, ", ", total_energy

    end do

    close(9)

end program calculate_velocity
