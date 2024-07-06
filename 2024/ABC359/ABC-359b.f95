program abc358b
    implicit none
    integer N
    integer i, cnt
    integer, allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(2*N))
    read (*, *) A(:)
    cnt = 0

    do i = 1, 2*N - 2
        if (A(i) == A(i + 2)) then
            cnt = cnt + 1
        end if
    end do

    write (*, *) cnt
1 
end program abc358b
