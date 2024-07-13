program abc357c
    !PannacottaFGKさんの回答を参考にしています。
    !https://atcoder.jp/contests/abc357/submissions/54357727
    !do iとkのループで全マス回す
    !各レベルの#の判定をdo k=n,1,-1のループで判定している
    !l=1 or 0で出力文字を制御している.
    integer i, j, k, pi, qi, pj, qj, n, l
    read *, n
    do i = 1, 3**n
        do j = 1, 3**n
            qi = i
            qj = j
            l = 0
            do k = n, 1, -1
                pi = (qi - 1)/(3**(k - 1))
                qi = mod(qi, 3**(k - 1))
                pj = (qj - 1)/(3**(k - 1))
                qj = mod(qj, 3**(k - 1))
                if ((pi == 1) .and. (pj == 1)) then
                    write (*, "(a1)", advance="no") "."
                    l = 1
                    exit
                end if
            end do
            if (l == 0) write (*, "(a1)", advance="no") "#"
        end do
        print *
    end do
end program
