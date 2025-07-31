program abc416a
    ! N         : 文字列の長さ
    ! L, R      : チェックする範囲（1-indexed）
    ! S         : 入力される文字列
    ! i         : ループ変数
    ! valid     : 範囲内が全て'o'かどうかを示す論理値
    implicit none
    integer :: N, L, R, i
    character(len=100) :: S
    logical :: valid

    ! 入力
    read (*, *) N, L, R
    read (*, *) S

    valid = .true.

    ! LからRまでを確認
    do i = L, R
        if (S(i:i) /= 'o') then
            valid = .false.
            exit
        end if
    end do

    ! 結果の出力
    if (valid) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program abc416a
