program abc371d
    ! maxN：最大の村数
    ! int64：64ビット整数の選択
    ! N：村の数
    ! Q：クエリの数
    ! i：ループカウンタ
    ! X：村の座標を格納する配列
    ! P：各村の人口を格納する配列
    ! S：累積人口を格納する配列（村0から村iまでの人口の総和）
    ! left：クエリに対してL以上の最初の村のインデックス
    ! right：クエリに対してR以下の最後の村のインデックス
    ! L, R：クエリで与えられた区間の左端と右端
    ! total：クエリに対してLからRまでの人口の総和
    ! mid, lo, hi：二分探索で使用する変数（midは中央、loは下限、hiは上限）
    implicit none
    integer, parameter :: maxN = 200000 ! 最大の村数
    integer, parameter :: int64 = selected_int_kind(15) ! 64ビット整数の選択
    integer :: N ! 村の数
    integer :: Q ! クエリの数
    integer :: i ! ループカウンタ
    integer, dimension(1:maxN) :: X ! 村の座標を格納する配列
    integer, dimension(1:maxN) :: P ! 各村の人口を格納する配列
    integer(kind=int64), dimension(0:maxN) :: S ! 累積人口を格納する配列（村0から村iまでの人口の総和）
    integer :: left ! クエリに対してL以上の最初の村のインデックス
    integer :: right ! クエリに対してR以下の最後の村のインデックス
    integer :: L, R ! クエリで与えられた区間の左端と右端
    integer(kind=int64) :: total ! クエリに対してLからRまでの人口の総和
    integer :: mid, lo, hi ! 二分探索で使用する変数（midは中央、loは下限、hiは上限）

    ! 入力の読み込み
    read (*, *) N ! 村の数を読み込む
    read (*, *) (X(i), i=1, N) ! 各村の座標を読み込む
    read (*, *) (P(i), i=1, N) ! 各村の人口を読み込む

    ! 累積人口の計算
    S(0) = 0_int64 ! 累積人口の初期値を0に設定
    do i = 1, N
        S(i) = S(i - 1) + P(i) ! 各村までの累積人口を計算
    end do

    ! クエリの処理
    read (*, *) Q ! クエリの数を読み込む
    do i = 1, Q
        read (*, *) L, R ! クエリの区間[L, R]を読み込む
        ! 左側のインデックスを二分探索で見つける
        left = bisect_left(X, N, L)
        ! 右側のインデックスを二分探索で見つける
        right = bisect_right(X, N, R) - 1

        ! クエリの範囲に村が存在しない場合の処理
        if (left > right) then
            total = 0_int64 ! 村が存在しない場合は人口総数は0
        else
            total = S(right) - S(left - 1) ! 累積人口の差から指定範囲の人口を計算
        end if
        !結果の出力
        print *, total
    end do

contains

    ! 二分探索で左側のインデックスを見つける関数
    integer function bisect_left(a, n, x)
        integer, intent(in) :: a(:), n, x
        integer :: lo, hi, mid
        lo = 1
        hi = n + 1
        do while (lo < hi)
            mid = (lo + hi)/2
            if (a(mid) < x) then
                lo = mid + 1 ! 探索範囲の下限を更新
            else
                hi = mid ! 探索範囲の上限を更新
            end if
        end do
        bisect_left = lo ! L以上の最初の村のインデックスを返す
    end function bisect_left

    ! 二分探索で右側のインデックスを見つける関数
    integer function bisect_right(a, n, x)
        integer, intent(in) :: a(:), n, x
        integer :: lo, hi, mid
        lo = 1
        hi = n + 1
        do while (lo < hi)
            mid = (lo + hi)/2
            if (a(mid) <= x) then
                lo = mid + 1 ! 探索範囲の下限を更新
            else
                hi = mid ! 探索範囲の上限を更新
            end if
        end do
        bisect_right = lo ! R以下の最後の村のインデックスを返す
    end function bisect_right

end program abc371d
