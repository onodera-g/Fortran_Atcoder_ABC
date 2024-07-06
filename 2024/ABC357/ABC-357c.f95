program abc357c
    implicit none
    integer i, j
    integer N, center
    character(1), allocatable::arr(:, :)

    !入力
    read (*, *) N
    allocate (arr(3**N, 3**N))

    !N=0の場合
    if (N == 0) then
        write (*, '(a1)') "."
        stop
    end if

    !N=>1
    arr = "#" !一旦黒塗り

    !N=1
    if (N > 1) then
        do i = 2, 3**N, 3
            do j = 2, 3**N, 3
                arr(i, j) = "."
            end do
        end do
    end if

    !N=2
    if (N > 2) then
        do i = 4, 3**N, 9
            do j = 4, 3**N, 9
                arr(i:i + 2, j:j + 2) = "."

            end do
        end do
    end if

    !N=3
    if (N > 3) then
        do i = 10, 3**N, 27
            do j = 10, 3**N, 27
                arr(i:i + 8, j:j + 8) = "."
            end do
        end do
    end if

    !N=4
    if (N > 4) then
        do i = 28, 3**N, 81
            do j = 28, 3**N, 81
                arr(i:i + 26, j:j + 26) = "."
            end do
        end do
    end if

    !N=5
    if (N > 5) then
        do i = 82, 3**N, 243
            do j = 82, 3**N, 243
                arr(i:i + 80, j:j + 80) = "."
            end do
        end do
    end if

    !N=6
    if (N > 6) then
        do i = 82, 3**N, 243
            do j = 82, 3**N, 243
                arr(i:i + 80, j:j + 80) = "."
            end do
        end do
    end if

    !中心の作成
    center = (3**N)/3
    arr(center + 1:center + 3**(N - 1), center + 1:center + 3**(N - 1)) = "."

    do i = 1, 3**N
        do j = 1, 3**N
            if (j /= 3**N) then
                write (*, '(a)', advance="no") arr(i, j)
            else
                write (*, '(a)') arr(i, j)
            end if
        end do
    end do
end program abc357c
