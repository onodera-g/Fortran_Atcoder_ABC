program abc414b
    ! N                : 入力される文字と長さの組の数
    ! c                : 現在処理中の文字
    ! l                : 現在処理中の長さ
    ! i, j             : ループ変数
    ! total_length     : 全体の長さの合計
    ! S                : 復元した文字列
    implicit none
    integer :: N, i, j
    integer(kind=8) :: l, total_length
    character(len=1) :: c
    character(len=100) :: S

    ! 入力
    read (*, *) N

    S = ''
    total_length = 0

    do i = 1, N
        read (*, *) c, l
        ! 文字数の合計が100を超えるか確認
        if (total_length + l > 100) then
            print *, 'Too Long'
            stop
        end if

        ! 文字列に連結
        do j = 1, l
            total_length = total_length + 1
            S(total_length:total_length) = c
        end do
    end do

    ! 結果の出力
    print *, trim(S)

end program abc414b
