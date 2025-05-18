program abc388c
    ! N        : 餅の数 (1 ≤ N ≤ 5×10^5)
    ! A        : 各餅の大きさ
    ! B        : マージソート用の一時配列
    ! left     : two-pointer の左ポインタ
    ! right    : two-pointer の右ポインタ
    ! ans      : 組み合わせの総数 (64bit 整数)
    implicit none
    integer(kind=8) :: N
    integer(kind=8) :: i, left, right
    integer(kind=8) :: ans
    integer(kind=8), allocatable :: A(:), B(:)
    integer(kind=8) :: width, l, mid, r, li, ri, k

    ! 入力
    read (*, *) N
    allocate (A(N), B(N))
    read (*, *) (A(i), i=1, N)

    ! マージソート（ボトムアップ）
    width = 1
    do while (width < N)
        i = 1
        do while (i <= N)
            l = i
            mid = min(i + width - 1, N)
            r = min(i + 2*width - 1, N)

            li = l
            ri = mid + 1
            k = l
            do while (li <= mid .and. ri <= r)
                if (A(li) <= A(ri)) then
                    B(k) = A(li)
                    li = li + 1
                else
                    B(k) = A(ri)
                    ri = ri + 1
                end if
                k = k + 1
            end do
            do while (li <= mid)
                B(k) = A(li)
                li = li + 1
                k = k + 1
            end do
            do while (ri <= r)
                B(k) = A(ri)
                ri = ri + 1
                k = k + 1
            end do

            i = i + 2*width
        end do

        do i = 1, N
            A(i) = B(i)
        end do
        width = width*2
    end do

    ! two-pointer によるペアカウント
    ans = 0
    left = 1
    do right = 1, N
        do while (left <= right .and. 2*A(left) <= A(right))
            left = left + 1
        end do
        ans = ans + (left - 1)
    end do

    ! 出力
    print *, ans

end program abc388c
