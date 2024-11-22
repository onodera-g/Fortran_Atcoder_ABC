program abc381c
    ! N        : 入力文字列 S の長さ
    ! max_len  : 最大の 11/22 文字列の長さ
    ! S        : 入力文字列 (1, 2, / からなる)
    ! left_1   : 各位置における左側から連続する '1' の長さを格納する配列
    ! right_2  : 各位置における右側から連続する '2' の長さを格納する配列
    implicit none
    integer :: N, i, max_len
    character(:), allocatable :: S
    integer, allocatable :: left_1(:), right_2(:)

    ! 入力
    read (*, *) N
    allocate (character(N) :: S)
    read (*, '(A)') S
    allocate (left_1(N), right_2(N))
    left_1 = 0; right_2 = 0

    ! 左から右への連続する '1' の長さを計算
    do i = 1, N
        if (S(i:i) == '1') then
            if (i == 1) then ! 最初の文字の場合
                left_1(i) = 1
            else ! 前の位置の値に 1 を加える
                left_1(i) = left_1(i - 1) + 1
            end if
        else ! '1' でない場合は 0
            left_1(i) = 0
        end if
    end do

    ! 右から左への連続する '2' の長さを計算
    do i = N, 1, -1
        if (S(i:i) == '2') then
            if (i == N) then ! 最後の文字の場合
                right_2(i) = 1
            else ! 次の位置の値に 1 を加える
                right_2(i) = right_2(i + 1) + 1
            end if
        else ! '2' でない場合は 0
            right_2(i) = 0
        end if
    end do

    ! 最大の 11/22 文字列の長さを計算
    max_len = 0
    do i = 1, N
        if (S(i:i) == '/') then
            ! 左の '1' の長さと右の '2' の長さの最小値を計算
            ! 部分文字列長は 2 * min(left_1, right_2) + 1 で計算
            max_len = max(max_len, 2*min(left_1(i - 1), right_2(i + 1)) + 1)
        end if
    end do

    ! 結果の出力
    print *, max_len

end program abc381c
