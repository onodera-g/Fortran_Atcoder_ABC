program abc353b
    !N  ：グループの数
    !K  ：アトラクションの定員
    !A  ：各グループの人数
    !cnt：アトラクションの運行回数
    implicit none
    integer N, K, tmp
    integer i, j, cnt
    integer, allocatable ::A(:)

    !入力
    read (*, *) N, K
    allocate (A(N))
    read (*, *) A(:)

    !カートに積み込み作業
    tmp = 0; cnt = 0; i = 1
    do
        cnt = cnt + 1
        tmp = A(i)
        !定員になるまで詰め込む
        do j = i + 1, N
            if (tmp + A(j) <= K) then
                tmp = tmp + A(j)
            else
                exit
            end if
        end do
        i = j
        if (i > N) exit
    end do

    !結果の出力
    write (*, *) cnt
end program abc353b
