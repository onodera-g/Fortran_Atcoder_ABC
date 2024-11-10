program abc379b
    ! N   : 高橋君の歯の本数
    ! K   : 丈夫な歯が連続している本数（この本数を使ってイチゴを1個食べることができる）
    ! cnt : 現在連続している丈夫な歯の本数をカウントする変数
    ! ans : 高橋君が最大で食べることができるイチゴの個数
    ! S   : 高橋君の歯の状態を表す文字列（'O'が丈夫、'X'が虫歯）
    implicit none
    integer i, N, K, cnt, ans
    character(100) S

    ! 入力
    read (*, *) N, K
    read (*, *) S(1:N)

    ! 最大で食べることができるイチゴの個数を計算
    cnt = 0
    ans = 0
    do i = 1, N
        if (S(i:i) == 'O') then
            cnt = cnt + 1
        else
            cnt = 0
        end if
        if (cnt >= K) then
            ans = ans + 1
            cnt = 0
        end if
    end do
    if (cnt >= K) ans = ans + 1

    ! 結果の出力
    write (*, *) ans

end program abc379b
