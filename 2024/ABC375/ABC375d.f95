program abc375d
    ! S               : 英大文字からなる入力文字列
    ! lenS            : 文字列Sの長さ
    ! c               : 文字のインデックスを計算するための一時変数
    ! left_count(26)  : 左側（i < j）の各文字の出現回数
    ! right_count(26) : 右側（j < k）の各文字の出現回数
    ! count           : 条件を満たす (i, j, k) の組み合わせの総数
    ! A_CODE          : 文字'A'のASCIIコード
    ! c_loop          : 文字ごとのループ変数
    implicit none
    character(len=200000) :: S
    integer(8) :: lenS
    integer :: i, j, c
    integer, parameter :: A_CODE = ichar('A')
    integer(8) :: left_count(26), right_count(26)
    integer(8) :: count
    integer :: c_loop

    ! 入力
    read (*, *) S
    lenS = len_trim(S)

    ! 右側のカウントを初期化（全文字の出現回数をカウント）
    right_count = 0
    do i = 1, lenS
        c = ichar(S(i:i)) - A_CODE + 1
        right_count(c) = right_count(c) + 1
    end do

    ! 各位置jを中心として、i < j < kかつS[i] = S[k]を満たすペアをカウント
    count = 0
    left_count = 0
    do j = 1, lenS
        ! 現在のjの文字を右側のカウントから減少
        c = ichar(S(j:j)) - A_CODE + 1
        right_count(c) = right_count(c) - 1 ! 右側のカウントを更新

        ! 左右の同じ文字の組み合わせをカウント
        do c_loop = 1, 26
            count = count + left_count(c_loop)*right_count(c_loop)
        end do
        left_count(c) = left_count(c) + 1
    end do

    ! 結果の出力
    print *, count

end program abc375d
