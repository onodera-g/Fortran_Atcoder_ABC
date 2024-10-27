program abc377b
    ! S           : 各マスが `#` (コマが置かれている) または `.` (空マス) で表される。
    ! row_koma(8) : 各行に少なくとも1つコマがあるかどうかを保持する配列。行にコマがあれば `true`、なければ `false`。
    ! col_koma(8) : 各列に少なくとも1つコマがあるかどうかを保持する配列。列にコマがあれば `true`、なければ `false`。
    ! cnt         : 自分のコマを置くことができる空マスの数
    implicit none
    character(8) :: S(8)
    integer :: i, j
    logical :: row_koma(8) = .false.
    logical :: col_koma(8) = .false.
    integer :: cnt

    ! 入
    do i = 1, 8
        read (*, '(A)') S(i)
    end do

    ! 行と列にコマがあるかどうかを確認
    do i = 1, 8
        do j = 1, 8
            if (S(i) (j:j) == '#') then
                row_koma(i) = .true. ! 行iにコマがあることを記録
                col_koma(j) = .true. ! 列jにコマがあることを記録
            end if
        end do
    end do

    ! 空マスで、かつ行と列にコマがないマスを数える
    cnt = 0
    do i = 1, 8
        do j = 1, 8
            if (S(i) (j:j) == '.' .and. .not. row_koma(i) .and. .not. col_koma(j)) then
                cnt = cnt + 1 ! 条件を満たす空マスのカウントを増やす
            end if
        end do
    end do

    ! 結果の出力
    print *, cnt ! コマを置けるマスの数を出力
end program abc377b
