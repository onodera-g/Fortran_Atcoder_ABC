program abc
    ! N: ボタンを押す回数 (1 ≤ N ≤ 100)
    ! C: 飴をもらえるまでの待機時間 (1 ≤ C ≤ 1000)
    ! cnt: もらえる飴の個数をカウントする変数
    ! tmp: 最後に飴をもらった時間を保持する変数
    ! T: ボタンを押した時間の配列 (サイズは N)
    ! i: ループカウンタ、ボタンを押す各時刻を処理するために使用
    implicit none
    integer i
    integer N, C, cnt, tmp
    integer, allocatable :: T(:)

    ! 入力
    read (*, *) N, C
    allocate (T(N))
    read (*, *) T

    ! 飴をもらえる回数の初期化 (最初のボタン押しでは必ずもらえる)
    cnt = 1
    tmp = T(1)
    do i = 2, N
        if (T(i) - tmp >= C) then
            cnt = cnt + 1
            tmp = T(i)
        end if
    end do

    ! 結果の出力
    print *, cnt

end program abc
