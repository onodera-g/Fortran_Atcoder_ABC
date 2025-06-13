program ABC397c
    ! N             : 配列長
    ! A(1..N)       : 入力される整数列 (1 ≤ A(i) ≤ N)
    ! seen(1..N)    : 要素の出現フラグ (0=未,1=済)
    ! prefix_dist(i): 1 ≤ i ≤ N までの区間での異なる要素数
    ! suffix_dist(i): i ≤ k ≤ N の区間での異なる要素数
    ! ans           : 最大の prefix_dist(i) + suffix_dist(i+1)
    ! i             : ループ用インデックス

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N, i, ans
    integer(int32), allocatable :: A(:), seen(:), prefix_dist(:), suffix_dist(:)
    integer(int32) :: count

    ! 入力
    read (*, *) N
    allocate (A(N), seen(N), prefix_dist(N), suffix_dist(N + 1))
    read (*, *) (A(i), i=1, N)

    ! prefix distinct counts
    seen = 0
    count = 0
    do i = 1, N
        if (seen(A(i)) == 0) then
            seen(A(i)) = 1
            count = count + 1
        end if
        prefix_dist(i) = count
    end do

    ! suffix distinct counts
    seen = 0
    count = 0
    suffix_dist(N + 1) = 0
    do i = N, 1, -1
        if (seen(A(i)) == 0) then
            seen(A(i)) = 1
            count = count + 1
        end if
        suffix_dist(i) = count
    end do

    ! 最大値探索: 1 ≤ split ≤ N-1
    ans = 0
    do i = 1, N - 1
        ans = max(ans, prefix_dist(i) + suffix_dist(i + 1))
    end do

    ! 結果の出力
    print *, ans

end program ABC397c
