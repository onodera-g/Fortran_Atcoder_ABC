program abc383b
    ! H, W             : オフィス(床＋机)がH行W列のマス目で表される
    ! D                : マンハッタン距離の閾値
    ! S(i,j)           : マス(i,j)の状態を表す文字('.'は床、'#'は机)
    ! kasitu_max       : 加湿される床マス数の最大値
    ! count            : ある組み合わせで加湿される床マス数
    ! x1,y1,x2,y2      : 加湿器1, 加湿器2を置く床マスの候補座標
    ! dist       　    : マス(i,j)が加湿器からのマンハッタン距離
    ! kasitu_masu(i,j) : 加湿されるマスを論理型で記録(.true.なら加湿済)
    implicit none
    integer :: i, j, H, W, D
    integer :: kasitu_max, count
    integer :: x1, y1, x2, y2, dist
    character, allocatable :: S(:, :)
    logical, allocatable :: kasitu_masu(:, :)

    ! 入力
    read (*, *) H, W, D
    allocate (S(H, W))
    allocate (kasitu_masu(H, W))
    do i = 1, H
        do j = 1, W
            read (*, '(A1)', advance='no') S(i, j)
        end do
        read (*, *)
    end do

    ! 2つの異なる床マスの組み合わせを調べる
    kasitu_max = 0
    do x1 = 1, H
        do y1 = 1, W
            ! 1つ目の加湿器を床マスに置く
            if (S(x1, y1) /= '.') cycle

            do x2 = 1, H
                do y2 = 1, W
                    ! 2つ目の加湿器を、1つ目と異なる床マスに置く
                    if (S(x2, y2) /= '.' .or. (x1 == x2 .and. y1 == y2)) cycle

                    ! 加湿器1から加湿されるマスを探索
                    kasitu_masu = .false.
                    count = 0
                    do i = 1, H
                        do j = 1, W
                            dist = abs(i - x1) + abs(j - y1)
                            if (dist <= D .and. S(i, j) == '.') then
                                ! 未加湿の場合のみカウント増加
                                if (.not. kasitu_masu(i, j)) then
                                    count = count + 1
                                end if
                                kasitu_masu(i, j) = .true.
                            end if
                        end do
                    end do

                    ! 加湿器2から加湿されるマスを探索
                    do i = 1, H
                        do j = 1, W
                            dist = abs(i - x2) + abs(j - y2)
                            if (dist <= D .and. S(i, j) == '.') then
                                ! 未加湿の場合のみカウント増加
                                if (.not. kasitu_masu(i, j)) then
                                    count = count + 1
                                end if
                                kasitu_masu(i, j) = .true.
                            end if
                        end do
                    end do

                    ! 加湿されるマス数の最大値を更新
                    kasitu_max = max(kasitu_max, count)
                end do
            end do
        end do
    end do

    ! 結果の出力
    write (*, *) kasitu_max
end program abc383b
