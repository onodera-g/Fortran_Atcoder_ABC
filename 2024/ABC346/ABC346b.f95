program abc346b
    !W   ：白鍵の数
    !B   ：黒鍵の数
    !WB  ：白鍵と黒鍵の合計
    !S   ：ピアノの鍵盤の並び例
    !cnt ：白鍵の個数カウント
    !flag：結果の判定用
    implicit none
    integer i, j, tmp
    integer W, B, cnt
    logical flag
    character(:), allocatable::WB
    character(12) S

    ! 入力
    read (*, *) W, B
    allocate (character(W + B) :: WB)
    S = 'wbwbwwbwbwbw'

    !判定
    flag = .false.
    do i = 1, 12
        cnt = 0
        do j = 1, W + B
            tmp = mod(i + j, 12) + 1
            if (S(tmp:tmp) == 'w') cnt = cnt + 1
        end do
        if (cnt == W) then
            flag = .true.
            exit
        end if
    end do

    !結果の出力
    if (flag .eqv. .true.) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program abc346b
