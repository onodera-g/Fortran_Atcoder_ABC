program abc343b
    !N：頂点の数
    !A：単純無向グラフ
    implicit none
    integer i, j
    integer N
    integer, allocatable::A(:, :)

    !入力
    read (*, *) N
    allocate (A(N, N))
    do i = 1, N
        read (*, *) A(i, :)
    end do

    !結果の出力
    do i = 1, N
        do j = 1, N
            if (A(i, j) == 1) write (*, '(i0,1x)', advance='no') j
        end do
        write (*, *)
    end do
end program abc343b
