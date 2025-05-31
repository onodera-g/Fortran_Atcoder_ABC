program ABC394b
    ! N        : 入力される文字列の個数
    ! S(i)     : i 番目に読み込んだ文字列（最大長 100 を想定）
    ! L(i)     : S(i) の有効長（len_trim の結果）
    ! i, j     : ループ変数
    ! tempStr  : 文字列の交換用一時変数
    ! tempLen  : 長さ交換用一時変数

    implicit none
    integer :: N, i, j
    character(len=100), allocatable :: S(:)
    integer, allocatable           :: L(:)
    character(len=100)             :: tempStr
    integer                        :: tempLen

    ! 入力
    read (*, *) N
    allocate (S(N))
    allocate (L(N))
    do i = 1, N
        read (*, '(A)') S(i)
        S(i) = adjustl(S(i))
        L(i) = len_trim(S(i))
    end do

    ! 長さ L(i) の昇順で S をソート(交換ソート)
    do i = 1, N - 1
        do j = i + 1, N
            if (L(i) > L(j)) then
                ! S(i) と S(j) を交換
                tempStr = S(i)
                S(i) = S(j)
                S(j) = tempStr
                ! 長さ L(i) と L(j) を交換
                tempLen = L(i)
                L(i) = L(j)
                L(j) = tempLen
            end if
        end do
    end do

    ! ソート後の順にすべての S(i) を連結し、結果を出力
    do i = 1, N
        write (*, '(A)', advance='no') S(i) (1:L(i))
    end do
end program ABC394b
