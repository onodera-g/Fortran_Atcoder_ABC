program abc343b
    !A：整数
    !N：整数Aの個数
    implicit none
    integer i
    integer A, N
    integer ans(100)

    !入力
    N = 0
    do
        read (*, *) A
        N = N + 1
        ans(N) = A
        if (A == 0) exit
    end do

    do i = 0, N - 1
        write (*, *) ans(N - i)
    end do
end program abc343b
