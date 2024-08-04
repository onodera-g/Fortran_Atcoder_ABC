program abc350c
    !N：数列の長さ
    !A：数列
    !Ans：入れ替えた履歴の保存
    !cnt：Ansの保存先管理用
    implicit none
    integer N, i, tmp, cnt
    integer, allocatable::A(:), Ans(:, :)

    !入力
    read (*, *) N
    allocate (A(N), Ans(N, 2))
    read (*, *) A

    !並び替え
    cnt = 0
    do i = 1, N
        if (A(i) == i) cycle
        do
            cnt = cnt + 1
            tmp = A(i)
            A(i) = A(tmp)
            A(tmp) = tmp
            Ans(cnt, 1) = i
            Ans(cnt, 2) = tmp
            if (A(i) == i) exit
        end do
    end do

    !結果の出力
    write (*, '(i0)') cnt
    do i = 1, cnt
        write (*, '(i0,1x,i0)') ans(i, 1), ans(i, 2)
    end do
end program abc350c
