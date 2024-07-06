program abc356b
    implicit none
    integer i, j
    integer N, M
    integer, allocatable::A(:), X(:, :), Ans(:)

    !入力
    read (*, *) N, M
    allocate (A(M), X(N, M), Ans(M))
    read (*, *) A(:)
    do i = 1, N
        read (*, *) (X(i, j), j=1, M)
    end do
    Ans = 0

    !合計
    do i = 1, N
        do j = 1, M
            Ans(j) = Ans(j) + X(i, j)
        end do
    end do

    !判定
    do i = 1, M
        !write (*, *) Ans(i)
        if (Ans(i) < A(i)) then
            write (*, '(a)') 'No'
            stop
        end if
    end do
    write (*, '(a)') 'Yes'

end program abc356b
