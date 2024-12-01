program abc382c
    ! N         : 箱（人）の総数
    ! M         : 流れる寿司の総数
    ! A(:)      : 各人の美食度を格納する配列（長さN）
    ! min_arr(:): 各位置までの美食度の最小値を保持する配列（長さN）
    ! B(:)      : 各寿司の美味しさを格納する配列（長さM）
    ! Ans(:)    : 各寿司を食べた人の番号を格納する配列（長さM）。誰も食べなかった場合は-1
    implicit none
    integer(16) :: i, N, M
    integer(16), allocatable :: A(:), min_arr(:), B(:), Ans(:)

    ! 入力
    read (*, *) N, M
    allocate (A(N), min_arr(N), B(M), Ans(M))
    read (*, *) A
    read (*, *) B

    ! min_arrの構築
    ! (minをとる範囲を少しずつ広げていくので、同じ値かだんだん小さくなっていく配列ができる)
    min_arr(1) = A(1) ! 最初の人の美食度を設定
    do i = 2, N
        if (A(i) < min_arr(i - 1)) then
            min_arr(i) = A(i) ! 現在の人の美食度がこれまでの最小値より小さい場合、更新
        else
            min_arr(i) = min_arr(i - 1)
        end if
    end do

    ! 各寿司を処理
    do i = 1, M
        Ans(i) = find_first(min_arr, N, B(i))
    end do

    ! 結果の出力
    do i = 1, M
        write (*, *) Ans(i)
    end do

contains
    ! 二分探索で条件を満たす最小の位置を探す関数
    integer(16) function find_first(min_arr, N, B_i)
        integer(16), intent(in) :: min_arr(:)
        integer(16), intent(in) :: N
        integer(16), intent(in) :: B_i
        integer(16) :: left, right, mid

        left = 1
        right = N
        find_first = -1 ! 誰も食べない場合は-1を返す

        do while (left <= right)
            mid = (left + right)/2
            if (min_arr(mid) <= B_i) then
                find_first = mid ! 条件を満たす最小の位置を更新
                right = mid - 1
            else
                left = mid + 1
            end if
        end do
    end function find_first

end program abc382c
