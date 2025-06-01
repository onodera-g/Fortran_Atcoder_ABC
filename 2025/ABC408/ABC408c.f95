program ABC408c
    ! N       : 城壁の総数 (1 ≤ N ≤ 10^6)
    ! M       : 砲台の総数 (1 ≤ M ≤ 2×10^5)
    ! i       : ループ用インデックス
    ! L, R    : 各砲台が守る城壁の範囲 [L, R]
    ! diff    : 差分配列。長さ N+1。diff(x) = +1 は x 以降でカバーが始まることを示し、
    !           diff(x) = -1 は x 以降でカバーが終わることを示す
    ! curr    : 累積和をとったときの現在の城壁 i を守る砲台数
    ! minCov  : 全城壁のうち最小のカバー数（最小で何台に守られているか）

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, M
    integer(int32) :: i, L, R
    integer(int32), allocatable :: diff(:)
    integer(int32) :: curr, minCov

    ! 入力
    read (*, *) N, M
    allocate (diff(N + 1))
    diff = 0
    do i = 1, M
        read (*, *) L, R
        diff(L) = diff(L) + 1
        if (R + 1 <= N) then
            diff(R + 1) = diff(R + 1) - 1
        end if
    end do

    ! 累積和で各城壁のカバー数を計算し、同時に最小カバー数を求める
    curr = 0
    minCov = M
    do i = 1, N
        curr = curr + diff(i)
        if (curr < minCov) then
            minCov = curr
        end if
    end do

    ! 結果の出力
    print *, minCov

end program ABC408c
