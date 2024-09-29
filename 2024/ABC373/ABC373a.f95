program abc373a
    ! S 　: 入力文字列(100文字以下)
    ! cnt : 文字列のカウント用
    implicit none
    integer i
    integer cnt
    character(100) S(12)

    !入力
    cnt = 0
    do i = 1, 12
        read (*, *) S(i)
        if (len_trim(S(i)) == i) cnt = cnt + 1
    end do

    !出力
    print *, cnt

end program abc373a
