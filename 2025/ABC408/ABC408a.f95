program ABC394a
    ! N       : 肩叩きを行う回数 (配列 T の要素数)
    ! S       : 最後に肩を叩かれてから長老が眠るまでの許容秒数
    ! T(i)    : i 回目の肩叩きが行われる「現在からの経過秒数」
    ! i       : ループ用カウンタ
    ! gap     : 直前の肩叩きから今回の肩叩きまでの経過秒数

    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32) :: N, S
    integer(int32), allocatable :: T(:)
    integer(int32) :: i, gap

    ! 入力
    read (*, *) N, S
    allocate (T(N))
    read (*, *) (T(i), i=1, N)

    ! 最初の肩叩きまでの経過秒数を確認 (0 秒時点に直前の肩叩きがある想定)
    gap = T(1)
    if (gap > S) then
        print *, "No"
        stop
    end if
    ! 2 回目以降の間隔を順にチェック
    do i = 2, N
        gap = T(i) - T(i - 1)
        if (gap > S) then
            print *, "No"
            stop
        end if
    end do
    print *, "Yes"
end program ABC394a
