program abc346
    !N：整数の個数
    !A：整数A
    !B：整数B
    implicit none
    integer i
    integer N
    integer, allocatable ::A(:), B(:)

    !入力
    read (*, *) N
    allocate (A(N), B(N - 1))
    read (*, *) A

    !Bの計算,出力
    do i = 1, N - 1
        B(i) = A(i)*A(i + 1)
        write (*, '(i0,1x)', advance='no') B(i)
    end do

end program abc346
