program abc356a
    implicit none
    integer N, L, R, i
    integer, allocatable:: A(:)

    read (*, *) N, L, R
    allocate (A(N))

    !A初期化
    do i = 1, N
        A(i) = i
    end do

    !入れ替え
    do i = L, R
        A(i) = L + (R - i)
    end do

    write (*, *) A
end program abc356a
