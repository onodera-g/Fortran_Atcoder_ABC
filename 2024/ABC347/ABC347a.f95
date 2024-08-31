program abc347a
    !N：数列の長さ
    !A：正整数A
    !K：割る倍数K
    implicit none
    integer i
    integer N, K
    integer, allocatable::A(:)

    !入力
    read (*, *) N, K
    allocate (A(N))
    read (*, *) A

    !結果の出力
    do i = 1, N
        if (mod(A(i), K) == 0) then !Kの倍数であるか
            write (*, '(i0,1x)', advance='no') A(i)/K
        end if
    end do

end program abc347a
