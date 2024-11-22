program abc381d
    ! n         : 数列 A の長さ
    ! a         : 数列 A を格納する配列
    ! last      : 各値が最後に現れた奇数位置または偶数位置を記録する配列
    ! l         : 現在の部分列の始まりを示すインデックス
    ! ans       : 最長の1122数列の長さ
    implicit none
    integer, parameter :: maxN = 200000
    integer :: n, i, l, ans
    integer :: a(maxN)
    integer :: last(maxN + 1)

    ! 入力
    read (*, *) n
    read (*, *) (a(i), i=1, n)

    ! 奇数項目から始まる連続部分列を探索
    ans = 0; last = -2; l = 1
    do i = 1, n - 1, 2
        if (a(i) /= a(i + 1)) then
            l = i + 2
        else
            l = max(l, last(a(i)) + 2)
        end if
        ans = max(ans, i + 2 - l)
        last(a(i)) = i
    end do

    ! 偶数項目から始まる連続部分列を探索
    last = -2; l = 2
    do i = 2, n - 1, 2
        if (a(i) /= a(i + 1)) then
            l = i + 2
        else
            l = max(l, last(a(i)) + 2)
        end if
        ans = max(ans, i + 2 - l)
        last(a(i)) = i
    end do

    ! 結果の出力
    print *, ans

end program abc381d
