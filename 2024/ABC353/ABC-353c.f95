program abc353c
    !
    implicit none
    integer(16) N
    integer(16) i, Ans, tmp, tmp2, j, tmp3
    integer(16), allocatable ::A(:)

    !入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    Ans = 0; tmp = 0; tmp2 = 0

    tmp3 = 10**8
    !
    do i = 1, N - 1
        !tmp2 = sum(A(i + 1:N))
        !tmp = mod(A(i)*(N - i) + tmp2, 10**8)
        !write (*, *) tmp
        !Ans = Ans + tmp
        do j = i + 1, N
            tmp2 = A(i) + A(j)
            tmp = mod(tmp2, tmp3)
            !write (*, *) tmp
            Ans = tmp + Ans
        end do
    end do

    !結果の出力
    write (*, '(i0)') Ans
end program abc353c
