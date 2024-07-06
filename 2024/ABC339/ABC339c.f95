program abc339c
    !A  ：各停留所での乗降数
    !Ans：最終的なバスの乗車人数
    !cnt：各停留所間の乗降人数の差分(不足人数が初期の乗車数となる)
    implicit none
    integer(8) N, Ans, i, cnt
    integer(8), allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)
    cnt = 0; Ans = 0

    !バスの乗降人数カウント
    do i = 1, N
        cnt = cnt + A(i)
        if (cnt < 0) then
            Ans = Ans - cnt
            cnt = 0
        end if
    end do

    !結果の出力
    Ans = sum(A) + Ans
    write (*, *) Ans
end program abc339c
