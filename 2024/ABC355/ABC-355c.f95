program abc355c
    !
    implicit none
    integer, allocatable::A(:), tate(:), yoko(:)
    integer N, T, x, y, i, cnt, naname1, naname2

    !
    read (*, *) N, T
    allocate (A(T), tate(N), yoko(N))
    read (*, *) A(:)

    !
    tate = 0; yoko = 0; naname1 = 0; naname2 = 0
    do i = 1, T
        !マス位置の特定
        if (mod(A(i), N) == 0) then
            y = A(i)/N
            x = N
        else
            y = A(i)/N + 1
            x = mod(A(i), N)
        end if
        !
        tate(x) = tate(x) + 1
        yoko(y) = yoko(y) + 1
        if (x == y) naname1 = naname1 + 1
        if (x + y == N + 1) naname2 = naname2 + 1
        !
        cnt = max(maxval(tate), maxval(yoko), naname1, naname2)
        if (cnt == N) then
            write (*, *) i
            stop
        end if
    end do
    write (*, *) - 1
end program abc355c
