program ABC409b
    ! N            入力される整数列の長さ
    ! A(i)         入力された非負整数列の要素
    ! x            評価する候補の非負整数
    ! cnt          A において x 以上の要素が何個あるかを数える
    ! i            ループ用カウンタ
    ! answer       条件を満たす最大の x
    use, intrinsic :: iso_fortran_env
    implicit none

    integer(int32) :: N
    integer(int32), allocatable :: A(:)
    integer(int32) :: x, cnt, i, answer

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)

    ! 条件を満たす最大の x を求める
    answer = 0
    do x = 0, N
        cnt = 0
        do i = 1, N
            if (A(i) >= x) then
                cnt = cnt + 1
            end if
        end do
        if (cnt >= x) then
            answer = x
        end if
    end do

    ! 結果の出力
    print *, answer

end program ABC409b
