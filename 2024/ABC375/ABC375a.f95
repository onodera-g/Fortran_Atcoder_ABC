program abc375a
    ! N   : 文字列Aの長さ（座席の数）
    ! cnt : 条件を満たす位置の数をカウント
    ! A   : 座席の状態（#または.）
    implicit none
    integer(16) :: i, N, cnt
    character(:), allocatable :: A

    ! 入力
    read (*, *) N
    allocate (character(N) :: A)
    read (*, *) A

    ! 文字列の1からN-2までループして確認
    cnt = 0
    do i = 1, N - 2
        if (A(i:i) == "#" .and. A(i + 1:i + 1) == "." .and. A(i + 2:i + 2) == "#") then
            cnt = cnt + 1
        end if
    end do

    ! 結果の出力
    write (*, *) cnt

end program abc375a
