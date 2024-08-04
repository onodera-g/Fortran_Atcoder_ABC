program abc363b
    !N  ：人数
    !T  ：髪の長さの閾値
    !P  ：カウントしたい髪の長さ
    !cnt：髪の長さがTcm以上の人数
    !Ans：日数のカウント
    implicit none
    integer N, T, P, cnt, i, Ans
    integer, allocatable::L(:)

    !入力
    read (*, *) N, T, P
    allocate (L(N))
    read (*, *) L

    !髪を1日ずつ伸ばす
    ans = 0
    do
        ! write (*, '(*(i0,1x))') L
        cnt = 0
        do i = 1, N
            if (L(i) >= T) cnt = cnt + 1
        end do
        ! write (*, *)
        if (cnt >= P) exit
        L = L + 1
        ans = ans + 1
    end do

    !結果の出力
    write (*, *) Ans

end program abc363b

