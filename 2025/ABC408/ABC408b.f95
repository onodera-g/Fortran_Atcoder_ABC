program ABC408b
    ! N           : 入力される整数列 A の要素数 (1 ≤ N ≤ 100)
    ! A(i)        : 入力された整数列の i 番目の要素 (1 ≤ A(i) ≤ 100)
    ! present(v)  : 値 v が A に出現したかどうかを論理型で記録するフラグ配列 (1 ≤ v ≤ 100)
    ! val         : 値 1～100 を走査するループ用変数
    ! M           : 重複を除いた後の要素数 (present が .true. の個数)
    ! result(j)   : present が .true. の値を小さい順に格納する配列 (1 ≤ j ≤ M)
    ! countVals   : result 配列に格納した要素数をカウントする変数
    ! i           : 各種ループ用カウンタ

    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int32)            :: N
    integer(int32), allocatable :: A(:)
    logical, allocatable      :: present(:)
    integer(int32)            :: val
    integer(int32)            :: M
    integer(int32), allocatable :: result(:)
    integer(int32)            :: countVals
    integer(int32)            :: i

    ! 入力
    read (*, *) N
    allocate (A(N))
    allocate (present(100))
    present = .false.
    read (*, *) (A(i), i=1, N)
    do i = 1, N
        present(A(i)) = .true.
    end do

    ! present が .true. の値を数えて M を求める
    M = 0
    do val = 1, 100
        if (present(val)) then
            M = M + 1
        end if
    end do

    ! result(M) 配列を確保し、小さい順に present の値を格納
    allocate (result(M))
    countVals = 0
    do val = 1, 100
        if (present(val)) then
            countVals = countVals + 1
            result(countVals) = val
        end if
    end do

    ! 結果の出力
    write (*, '(I0)') M
    if (M > 0) then
        write (*, '(I0)', advance='no') result(1)
        do i = 2, M
            write (*, '(1X,I0)', advance='no') result(i)
        end do
    end if

end program ABC408b
