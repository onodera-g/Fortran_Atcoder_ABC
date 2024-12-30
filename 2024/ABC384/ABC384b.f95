program abc384b
    ! N: 参加するARCの回数
    ! R: 初期のレーティング
    ! D(:): 各ARCのDivision (1または2)
    ! A(:): 各ARCでの成績
    implicit none
    integer i
    integer N, R
    integer, allocatable :: D(:), A(:)

    ! 入力
    read (*, *) N, R
    allocate (D(N), A(N))
    do i = 1, N
        read (*, *) D(i), A(i)
    end do

    ! レーティングの更新
    do i = 1, N
        if (D(i) == 2 .and. R >= 1200 .and. R <= 2399) then
            R = R + A(i)
        elseif (D(i) == 1 .and. R >= 1600 .and. R <= 2799) then
            R = R + A(i)
        end if
    end do

    ! 結果の出力
    write (*, *) R

end program abc384b

