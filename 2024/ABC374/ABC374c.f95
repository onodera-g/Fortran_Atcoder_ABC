program abc374c
    implicit none
    ! N: 部署の数
    ! sumA, sumB: グループAとグループBに割り当てられた人数の合計
    ! bit: 部署の割り当てをビット列で表現
    ! minAB: 同時に昼休みを取る人数の最大値の最小値
    ! K: 各部署に所属する人数を格納する配列
    ! total_sum: 全ての部署の合計人数
    ! max_mask: 部署の割り当てを表現するビット列の最大値
    integer(16) ::  j
    integer(16) :: N, sumA, sumB, bit, minAB
    integer(16), allocatable :: K(:)
    integer(16) :: total_sum, max_mask

    !入力
    read (*, *) N
    allocate (K(N))
    read (*, *) K

    ! N=2 の特別処理
    if (N == 2) then
        write (*, *) maxval(K)
        stop
    end if

    ! 総人数を計算し、最小値の初期値を設定
    total_sum = sum(K)
    minAB = total_sum
    max_mask = ishft(1, N) - 1

    ! 部分集合の列挙と評価
    do bit = 1, max_mask
        sumA = 0; sumB = 0
        do j = 1, N
            if (btest(bit, j - 1)) then
                sumA = sumA + K(j)
            else
                sumB = sumB + K(j)
            end if
            ! 現在の max(sumA, sumB) が既に minAB 以上なら評価を中断
            if (max(sumA, sumB) >= minAB) exit
        end do
        ! グループAとグループBの両方に少なくとも1つの部署が割り当てられている場合
        if (sumA /= 0 .and. sumB /= 0) then
            minAB = min(minAB, max(sumA, sumB))
        end if
    end do

    ! 結果の出力
    write (*, *) minAB
end program abc374c
