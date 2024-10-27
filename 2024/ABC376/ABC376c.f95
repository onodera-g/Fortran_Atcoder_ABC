program abc376c
    ! N        : おもちゃの数 (1からNまでの番号)
    ! M        : 既存の箱の数 (M = N - 1)
    ! mid      : 二分探索の中間値
    ! low      : 二分探索の下限
    ! high     : 二分探索の上限
    ! ans      : 最小の購入箱のサイズ x を保持する変数
    ! A(:)     : おもちゃのサイズ配列 (1からNまで)
    ! B(:)     : 既存の箱のサイズ配列 (1からMまで)
    ! B_with_x(:): 新しく購入した箱 x を挿入した後の箱のサイズ配列 (1からNまで)
    implicit none
    integer :: N, i, j, mid, low, high, ans
    integer, allocatable :: A(:), B(:), B_with_x(:)

    ! 入力
    read (*, *) N
    allocate (A(N), B(N - 1))
    read (*, *) A
    read (*, *) B

    ! おもちゃと箱のサイズをソート
    call margesort(A, N)
    call margesort(B, N - 1)

    ! 二分探索
    low = 1; high = A(N)
    ans = -1
    allocate (B_with_x(N))
    do while (low <= high)
        mid = low + (high - low)/2
        ! サイズ x = mid の箱を B に挿入して B_with_x を作成
        j = find_insert_pos(B, N - 1, mid)
        if (j > N - 1) then
            B_with_x(1:N - 1) = B(1:N - 1)
            B_with_x(N) = mid
        else
            B_with_x(1:j - 1) = B(1:j - 1)
            B_with_x(j) = mid
            B_with_x(j + 1:N) = B(j:N - 1)
        end if

        ! 割り当て判定（大きい順）
        i = N
        j = N
        do while (i >= 1 .and. j >= 1)
            if (A(i) <= B_with_x(j)) then
                i = i - 1
            end if
            j = j - 1
        end do

        if (i == 0) then
            ! 割り当て可能な場合
            ans = mid
            high = mid - 1
        else
            ! 割り当て不可能な場合
            low = mid + 1
        end if
    end do

    ! 結果の出力
    print *, ans

contains

    ! マージソートのサブルーチン
    subroutine margesort(x, n)
        integer :: n
        integer :: x(:)
        integer, allocatable :: tmp(:)
        allocate (tmp(n))
        call loop_margesort(x, tmp, 1, n)
        deallocate (tmp)
    end subroutine margesort

    recursive subroutine loop_margesort(x, tmp, left, right)
        integer :: x(:), tmp(:)
        integer:: left, right
        integer :: mid, i, j, k

        if (left >= right) return

        mid = (left + right)/2
        call loop_margesort(x, tmp, left, mid)
        call loop_margesort(x, tmp, mid + 1, right)

        ! マージステップ
        j = 0
        do i = left, mid
            tmp(i) = x(i)
        end do
        do i = mid + 1, right
            tmp(i) = x(right - j)
            j = j + 1
        end do

        i = left
        j = right

        do k = left, right
            if (i > mid) then
                x(k) = tmp(j)
                j = j - 1
            else if (j < mid + 1) then
                x(k) = tmp(i)
                i = i + 1
            else if (tmp(i) <= tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else
                x(k) = tmp(j)
                j = j - 1
            end if
        end do
    end subroutine loop_margesort

    ! 挿入位置を見つける二分探索関数
    integer function find_insert_pos(B, n, x_val)
        integer :: n, x_val
        integer:: B(n)
        integer :: low, high, mid

        low = 1
        high = n
        find_insert_pos = n + 1

        do while (low <= high)
            mid = low + (high - low)/2
            if (B(mid) < x_val) then
                low = mid + 1
            else
                find_insert_pos = mid
                high = mid - 1
            end if
        end do
    end function find_insert_pos
end program abc376c
