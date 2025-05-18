program ABC389d
    ! R         : 円の半径。1 ≤ R ≤ 10^6 の整数。
    ! S         : 比較用に 4*R^2 を格納する変数。
    ! i, j      : グリッド上の正方形中心の絶対座標 (i, j)。非負整数。
    ! ans       : 条件を満たす正方形の個数を累積する変数。
    ! 条件      : 正方形が円に完全に内包されるための (2*i+1)^2 + (2*j+1)^2 ≤ S の判定を行う。
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64) :: R, S
    integer(int64) :: i, j, ans

    ! 入力
    read (*, *) R

    ! S を計算
    S = 4*R*R
    ans = 0

    ! j を初期化（i=0 のときの j を仮に R から開始）
    j = R

    ! i を増やしながら j を調整
    do i = 0, R
        ! 条件を満たす最大の j を求める
        do while (j >= 0 .and. (2*i + 1)*(2*i + 1) + (2*j + 1)*(2*j + 1) > S)
            j = j - 1
        end do
        if (j < 0) exit
        if (i == 0) then
            ! i=0 列：j=0 は 1 個、j>0 は上下対称で 2*j 個
            ans = ans + 2*j + 1
        else
            ! i>0 列：左右対称で 2 個、さらに j>0 は四象限で 4*j 個
            ans = ans + 2 + 4*j
        end if
    end do

    ! 結果出力
    print *, ans
end program ABC389d
