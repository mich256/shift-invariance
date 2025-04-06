! File: integer_predicates.f90
module integer_predicates
  implicit none
contains

  ! Predicate: Returns .TRUE. if x is nearly an integer.
  logical function is_wholenumber(x)
    implicit none
    double precision, intent(in) :: x
    double precision :: tol
    tol = sqrt(epsilon(1.0d0))  ! Tolerance based on machine precision
    is_wholenumber = abs(x - dble(nint(x))) < tol
  end function is_wholenumber

  ! Function: Checks input and returns its square if x is an integer.
  double precision function my_function(x)
    implicit none
    double precision, intent(in) :: x
    if (.not. is_wholenumber(x)) then
      print*, "Error: Input x must be an integer!"
      stop 1  ! Terminate the program with an error code.
    end if
    my_function = x**2
  end function my_function

end module integer_predicates


program test_predicates
  use integer_predicates
  implicit none
  double precision :: a, result

  ! Test with a valid integer-like value.
  a = 4.0d0
  result = my_function(a)
  print*, "my_function(", a, ") = ", result

  ! Test with a non-integer value; this will trigger an error.
  a = 4.0001d0
  result = my_function(a)
  print*, "my_function(", a, ") = ", result

end program test_predicates