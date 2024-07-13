program abc362c
    !N：整数の組みの数
    !L：各整数の組(最小値)
    !R：各整数の組(最大値)
    !X：整数列
    !sumX：Xの合計
    implicit none
    integer i
    integer(16) N, sumX
    integer(16), allocatable::L(:), R(:), X(:)

    !入力
    read (*, *) N
    allocate (L(N), R(N), X(N))
    do i = 1, N
        read (*, *) L(i), R(i)
    end do

    !条件１から最小はLiである必要がある。
    if (sum(L(1:N)) == 0) then !最小値は全部L(i)のパターン
        write (*, '(a)') 'Yes'
        write (*, '(*(i0,1x))') L(1:N)
        stop
    elseif (sum(L(1:N)) > 0 .or. sum(R(1:N)) < 0) then !L,Rの範囲によって答えが成立しないパターン
        write (*, '(a)') 'No'
        stop
    end if

    !ΣX=0の探索
    X = L
    sumX = sum(X(1:N))
    do i = 1, N
        if (R(i) - L(i) < -sumX) then
            X(i) = R(i)
            sumX = sumX + (R(i) - L(i))
        else
            X(i) = L(i) - sumx
            exit
        end if
    end do

    !結果の出力
    write (*, '(a)') 'Yes'
    write (*, '(*(i0,1x))') X(1:N)

end program abc362c
