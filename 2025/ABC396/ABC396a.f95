program ABC396a
    ! N        : 整数列 A の要素数
    ! A(i)     : 入力される整数列の各要素
    ! i        : ループ用カウンタ
    ! found    : 同じ要素が3連続する箇所が見つかったかの論理フラグ

    use, intrinsic :: iso_fortran_env
    implicit none

    integer(int32) :: N, i
    integer(int32), allocatable :: A(:)
    logical :: found

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    found = .false.

    ! 3連続を検出
    do i = 1, N - 2
        if (A(i) == A(i + 1) .and. A(i + 1) == A(i + 2)) then
            found = .true.
            exit
        end if
    end do

    ! 結果の出力
    if (found) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program ABC396a
