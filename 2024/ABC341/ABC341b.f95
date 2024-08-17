program abc341b
    !N：正整数
    !A：各国の通過
    !S：支払い単位（i国)
    !T：支払い単位（i+1国)
    implicit none
    integer(16) N, i
    integer(16), allocatable::A(:), S(:), T(:)

    !入力
    read (*, *) N
    allocate (A(N), S(N - 1), T(N - 1))
    read (*, *) (A(i), i=1, N)
    do i = 1, N - 1
        read (*, *) S(i), T(i)
    end do

    !通過の変換
    do i = 1, N - 1
        A(i + 1) = A(i + 1) + A(i)/S(i)*T(i)
    end do

    !結果の出力
    write (*, *) A(N)
end program abc341b
