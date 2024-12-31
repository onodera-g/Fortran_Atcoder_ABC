program abc386b
    ! S      : 目標の表示文字列（入力される数字列）
    ! count  : ボタン押下回数のカウント
    ! len_S  : 文字列 S の有効な長さ
    implicit none
    character(1001) S
    integer count, i, len_S

    ! 入力
    read (*, *) S

    ! 1桁目からボタンを入力
    len_S = len_trim(S)
    count = 0
    i = 1
    do while (i <= len_S)
        if (S(i:i) .eq. '0') then
            if (i < len_S .and. S(i + 1:i + 1) .eq. '0') then
                ! "00"ボタンを押す
                count = count + 1
                i = i + 2
            else
                ! "0"ボタンを押す
                count = count + 1
                i = i + 1
            end if
        else
            ! 他の数字ボタンを押す
            count = count + 1
            i = i + 1
        end if
    end do

    ! 結果の出力
    print *, count
end program abc386b
