program ABC395c
    ! N        : 整数列 A の要素数
    ! A        : 長さ N の整数列
    ! last_pos : 各値が「最後に」出現した位置を格納する配列
    ! i        : ループ用カウンタ
    ! x        : 現在注目している A(i) の値
    ! best_len : 条件を満たす最短部分列の長さ（初期値は大きな値）

    use, intrinsic :: iso_fortran_env
    implicit none

    integer(int32), parameter :: MAXV = 1000000
    integer(int32) :: N, i, x
    integer(int32), allocatable :: A(:)
    integer(int32), allocatable :: last_pos(:)
    integer(int32) :: best_len

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    allocate (last_pos(0:MAXV))
    last_pos = 0

    ! best_len を N+1 に初期化（十分大きい値）
    best_len = N + 1

    ! 各位置 i を走査し，A(i) の直前出現位置を調べる
    do i = 1, N
        x = A(i)
        if (last_pos(x) /= 0) then
            ! 直前出現位置 last_pos(x) から i までの部分列に重複あり
            best_len = min(best_len, i - last_pos(x) + 1)
        end if
        ! A(i) の最後の出現位置を更新
        last_pos(x) = i
    end do

    ! 結果の出力
    if (best_len == N + 1) then
        print *, -1
    else
        print *, best_len
    end if

end program ABC395c
