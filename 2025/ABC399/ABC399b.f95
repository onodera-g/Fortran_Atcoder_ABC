program ABC399b
    ! N           : 参加者の人数
    ! P           : 各参加者の得点を格納する配列
    ! rank        : 各参加者の順位を格納する配列
    ! i, j        : ループ用インデックス
    ! countHigher : ある参加者より得点が高い人数

    implicit none
    integer :: N, i, j, countHigher
    integer, allocatable :: P(:), rank(:)

    ! 入力
    read (*, *) N
    allocate (P(N), rank(N))
    read (*, *) (P(i), i=1, N)

    ! 各参加者の順位を計算
    do i = 1, N
        countHigher = 0
        do j = 1, N
            if (P(j) > P(i)) then
                countHigher = countHigher + 1
            end if
        end do
        rank (i) = countHigher + 1
    end do

    ! 結果の出力
    do i = 1, N
        write (*, '(I0)') rank(i)
    end do

end program ABC399b
