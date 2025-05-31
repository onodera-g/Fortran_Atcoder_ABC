program ABC393b
    ! S : 入力の文字列 (英大文字からなる長さ 3～100)
    ! n : 文字列 S の実際の長さ
    ! i, j : 'A','B' の位置を走査するループ変数 (1-based)
    ! k : 三文字目 'C' があるべき位置
    ! ans : 条件を満たす (i,j,k) の組み合わせ数

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    character(len=100) :: S ! 最大 100 文字まで読める固定長文字列
    integer(int32)     :: n, i, j, k, ans

    ! 入力
    read (*, '(A)') S
    n = len_trim(S) ! 有効な文字数（末尾の空白を除去）

    ! A → B → C が等間隔に並ぶ」(i,j,k) をすべて数える
    ans = 0
    do i = 1, n
        if (S(i:i) /= 'A') cycle
        do j = i + 1, n
            if (S(j:j) /= 'B') cycle
            k = 2*j - i
            if (k > n) cycle
            if (S(k:k) == 'C') ans = ans + 1
        end do
    end do

    ! 結果の出力
    print *, ans

end program ABC393b
