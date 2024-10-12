program abc375c
    ! N: グリッドのサイズ (偶数)
    ! first, last: 現在処理している層の最初と最後のインデックス
    ! offset: 現在のオフセット
    ! grid: N x N のグリッドを表す配列
    ! temp: 一時的に値を保持するための変数
    ! line: 一行分のグリッドのデータを入力するための変数
    implicit none
    integer :: N, i, j, first, last, offset
    character(len=1), allocatable :: grid(:, :)
    character(len=1) :: temp
    character(len=1000) :: line

    ! 入力
    read (*, *) N
    allocate (grid(N, N))
    do i = 1, N
        read (*, '(A)') line
        do j = 1, N
            grid(i, j) = line(j:j)
        end do
    end do

    ! グリッドの変換操作：i=1からN/2まで層ごとに処理
    do i = 1, N/2
        first = i
        last = N + 1 - i
        do j = first, last - 1
            offset = j - first
            ! 層iのセル (first, j) を反時計回りに4つのセルを回転させる
            temp = grid(first, j)
            grid(first, j) = grid(j, last)
            grid(j, last) = grid(last, last - offset)
            grid(last, last - offset) = grid(last - offset, first)
            grid(last - offset, first) = temp
        end do
    end do

    ! 結果の出力
    do i = 1, N
        do j = 1, N
            write (*, '(A1)', advance='no') grid(i, j)
        end do
    end do

end program abc375c
