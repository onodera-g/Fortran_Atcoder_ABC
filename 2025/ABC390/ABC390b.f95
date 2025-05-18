program ABC390b
    ! N   : シーケンスの長さ
    ! A   : 長さ N の正整数列
    ! i   : ループカウンタ
    ! ok  : 等比数列かどうかを示すフラグ
    implicit none

    integer    :: N, i
    integer, allocatable :: A(:)
    logical           :: ok

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    ! 等比数列判定
    ok = .true.
    do i = 1, N - 1
        ! 小数を使わずに A(i+1)/A(i) = A(2)/A(1) を判定
        if (A(i + 1)*A(1) /= A(2)*A(i)) then
            ok = .false.
            exit
        end if
    end do

    ! 出力
    if (ok) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program ABC390b
