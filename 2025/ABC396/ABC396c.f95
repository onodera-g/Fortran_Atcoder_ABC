program ABC396c
    ! N, M       : 配列 A, B の長さ
    ! A(:), B(:) : 入力される整数列 (64ビット)
    ! S(i)       : A の降順に並べたとき先頭 i 項の累積和 (0 ≤ i ≤ N)
    ! T(j)       : B の降順に並べたとき先頭 j 項の累積和 (0 ≤ j ≤ M)
    ! maxT(j)    : T(0..j) の中の最大値
    ! ans        : 答え (S(i) + maxT(min(i,M)) の最大値)
    ! i, j       : ループ用インデックス

    use, intrinsic :: iso_fortran_env
    implicit none

    integer(int32) :: N, M, i, j
    integer(8), allocatable :: A(:), B(:)
    integer(8), allocatable :: S(:), T(:), maxT(:)
    integer(8) :: ans
    ! 入力
    read (*, *) N, M
    allocate (A(N), B(M))
    read (*, *) (A(i), i=1, N)
    read (*, *) (B(i), i=1, M)

    ! マージソートで昇順ソート
    call margesort_int64(A, N)
    call margesort_int64(B, M)

    ! 降順累積和 S, T と T の累積最大 maxT を計算
    allocate (S(0:N), T(0:M), maxT(0:M))
    S(0) = 0
    do i = 1, N
        S(i) = S(i - 1) + A(N - i + 1)
    end do
    T(0) = 0; maxT(0) = 0
    do i = 1, M
        T(i) = T(i - 1) + B(M - i + 1)
        maxT(i) = max(maxT(i - 1), T(i))
    end do

    ! 最大値を探索
    ans = 0
    do i = 0, N
        j = i
        if (j > M) j = M
        ans = max(ans, S(i) + maxT(j))
    end do

    ! 結果の出力
    print *, ans

contains
    recursive subroutine margesort_int64(x, n)
        integer(8), intent(inout) :: x(:)
        integer(4), intent(in) :: n
        integer(8), allocatable :: tmp(:)
        call loop_margesort(x, tmp, 1, n)
    end subroutine

    recursive subroutine loop_margesort(x, tmp, left, right)
        integer(8), intent(inout) :: x(:)
        integer(8), allocatable, intent(inout) :: tmp(:)
        integer(4), intent(in) :: left, right
        integer(4) :: mid, i, j
        if (left >= right) return
        mid = (left + right)/2
        call loop_margesort(x, tmp, left, mid)
        call loop_margesort(x, tmp, mid + 1, right)
        if (.not. allocated(tmp)) allocate (tmp(size(x)))
        ! 左半分コピー
        do i = left, mid; tmp(i) = x(i); end do
        ! 右半分を逆順コピー
        do i = mid + 1, right; tmp(i) = x(right - (i - mid - 1)); end do
        i = left; j = right
        do mid = left, right
            if (tmp(i) <= tmp(j)) then
                x(mid) = tmp(i); i = i + 1
            else
                x(mid) = tmp(j); j = j - 1
            end if
        end do
    end subroutine

end program ABC396c
