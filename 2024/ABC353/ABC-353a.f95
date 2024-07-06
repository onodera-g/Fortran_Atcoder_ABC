program abc352a
    !N    ：ビルの数
    !H    ：各ビルの高さ
    !max_H：1番目のビルより高いビル
    implicit none
    integer N, max_H, i
    integer, allocatable ::H(:)

    !入力
    read (*, *) N
    allocate (H(N))
    read (*, *) H(:)

    !結果の出力
    max_H = H(1)
    do i = 2, N
        if (max_H < H(i)) then
            write (*, *) i
            stop
        end if
    end do
    write (*, *) - 1

end program abc352a
