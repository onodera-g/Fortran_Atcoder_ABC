program ABC409c
    ! N         ：点の総数
    ! L         ：円周長
    ! d(i)      ：1 ≤ i ≤ N-1 の移動距離
    ! pos(i)    ：点 i の円周上の位置 (0 ≤ pos(i) < L)
    ! delta     ：L/3（等分した弧長）
    ! freq(p)   ：位置 p に存在する点の個数（添字範囲 0..L-1）
    ! cnt       ：正三角形を構成する点の組み合わせ数（64ビット整数）

    use, intrinsic :: iso_fortran_env, only: int64, int64
    implicit none

    integer(int64) :: N, L
    integer(int64), allocatable :: d(:), pos(:), freq(:)
    integer(int64) :: i, p, q, r, delta
    integer(int64) :: cnt

    ! 入力
    read (*, *) N, L
    allocate (d(N - 1))
    read (*, *) (d(i), i=1, N - 1)

    ! 点の位置を累積和で計算
    allocate (pos(N))
    pos(1) = 0_int64
    do i = 2, N
        pos(i) = mod(pos(i - 1) + d(i - 1), L)
    end do

    ! L が 3 の倍数でなければ正三角形は存在しない
    if (mod(L, 3_int64) /= 0_int64) then
        print *, 0_int64
        stop
    end if
    delta = L/3_int64

    ! 各位置にいる点の個数をカウント (添字 0..L-1)
    allocate (freq(0:L - 1))
    freq = 0
    do i = 1, N
        freq(pos(i)) = freq(pos(i)) + 1
    end do

    ! 正三角形を構成する組み合わせ数を計算
    cnt = 0_int64
    do p = 0, L - 1
        if (freq(p) == 0) cycle
        q = mod(p + delta, L)
        r = mod(p + 2*delta, L)
        if (freq(q) > 0 .and. freq(r) > 0) then
            cnt = cnt + int(freq(p), int64)*int(freq(q), int64)*int(freq(r), int64)
        end if
    end do

    ! 結果の出力
    cnt = cnt/3_int64
    print *, cnt

end program ABC409c
