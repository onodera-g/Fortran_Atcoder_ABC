program abc340c
    !PannacottaFGKさんの回答を参考にしています。
    !提出 #50159317： https://atcoder.jp/contests/abc340/submissions/50159317
    implicit none
    integer(16) i
    integer(16) N, Ans, k, m

    !入力
    read (*, *) N

    !支払った金額の総和
    k = N; m = 2; i = 1
    do while (k >= 2) !これ以上分けられないなら終了
        k = k/2
        m = m*2
        i = i + 1
    end do
    Ans = n*(i + 1) - m

    !結果の出力
    write (*, *) Ans

end program abc340c
