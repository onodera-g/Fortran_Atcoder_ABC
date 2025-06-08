program ABC395a
    ! N       : 整数列 A の要素数
    ! A(i)    : 入力される整数列の i 番目の要素
    ! i       : ループ用カウンタ
    ! isStrict: A が狭義単調増加かを示す論理値

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    integer(int32) :: N
    integer(int32), allocatable :: A(:)
    integer(int32) :: i
    logical :: isStrict

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    isStrict = .true.

    ! 隣接要素をチェック
    do i = 1, N - 1
        if (A(i) >= A(i + 1)) then
            isStrict = .false.
            exit
        end if
    end do

    ! 結果の出力
    if (isStrict) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program ABC395a
