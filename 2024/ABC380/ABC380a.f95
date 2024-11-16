program abc380a
    ! N    : 入力された各桁の数字を格納する6要素の整数配列
    ! cnt1 : 数字 '1' の出現回数をカウントする変数
    ! cnt2 : 数字 '2' の出現回数をカウントする変数
    ! cnt3 : 数字 '3' の出現回数をカウントする変数
    implicit none
    integer N(6), i, cnt1, cnt2, cnt3

    ! 入力
    read (*, '(6i1)') (N(i), i=1, 6)

    ! 条件の判定
    cnt1 = 0
    cnt2 = 0
    cnt3 = 0
    do i = 1, 6
        if (N(i) == 1) cnt1 = cnt1 + 1
        if (N(i) == 2) cnt2 = cnt2 + 1
        if (N(i) == 3) cnt3 = cnt3 + 1
    end do

    ! 結果の出力
    if (cnt1 == 1 .and. cnt2 == 2 .and. cnt3 == 3) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program abc380a
