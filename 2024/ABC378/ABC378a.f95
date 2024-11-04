program abc378a
    ! A   : 各ボールの色
    ! An  : ボールを捨てているか管理する配列
    ! cnt : 捨てたボールの回数
    implicit none
    integer A(4), An(4), i, j, cnt
    !入力
    read (*, *) A(1:4)

    !　ボールの色判定
    cnt = 0; An = 0
    do i = 1, 4
        do j = i + 1, 4
            !　色が一致しているならボールを捨てる
            if (A(i) == A(j) .and. An(i) == 0 .and. An(j) == 0) then
                cnt = cnt + 1
                An(i) = 1
                An(j) = 1
            end if
        end do
    end do

    ! 結果の出力
    print *, cnt

end program abc378a
