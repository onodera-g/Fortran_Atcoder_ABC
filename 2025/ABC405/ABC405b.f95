program ABC405b
    ! N: 整数列 A の長さ
    ! M: 1 以上 M 以下のすべての整数を含む必要がある範囲の上限
    ! A: 長さ N の整数列
    ! seen: 1 から M までの各整数が出現したかのフラグ
    ! distinct: 現在までに出現した異なる整数の種類数
    ! r: 先頭から調べて 1..M がすべて揃う最小の位置
    ! ans: 末尾から削除すべき最小回数

    implicit none
    integer :: N, M, i, distinct, r, ans
    integer, allocatable :: A(:)
    logical, allocatable :: seen(:)

    ! 入力
    read (*, *) N, M
    allocate (A(N), seen(M))
    read (*, *) (A(i), i=1, N)
    seen = .false.
    distinct = 0
    r = -1

    ! 先頭から走査して 1..M が揃う位置を探す
    do i = 1, N
        if (.not. seen(A(i))) then
            seen(A(i)) = .true.
            distinct = distinct + 1
        end if
        if (distinct == M) then
            r = i
            exit
        end if
    end do

    if (r == -1) then
        ! 全体で 1..M が揃わない場合は操作不要
        ans = 0
    else
        ! プレフィックス長を r-1 以下にするために N-(r-1) 回削除
        ans = N - r + 1
    end if

    ! 結果の出力
    print *, ans
end program ABC405b
