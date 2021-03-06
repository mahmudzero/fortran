function add(ix, jx) result(r)
    ! declaring `intent(in)` means that the value is immutable
    ! not declaring an `intent` implicitly binds `intent(out)`
    integer, intent(in)    :: ix ! input
    integer, intent(in)    :: jx ! input
    integer                :: r  ! output
    r = ix + jx
end function

program functions
    ! this is a comment
    implicit none
    ! have to declare return type of add
    ! I believe this also 'imports' the function into the 'program'
    integer :: add
    print *, "Executing addition of 5, 6: ", add(5, 6)
end program functions
