module aof25
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, aof25!"
  end subroutine say_hello
end module aof25
