program abc366a
    !N   ：有効票
    !T   ：高橋氏の票数
    !A   ：青木氏の票数
    !diff：未開票の票数
    implicit none
    integer N, T, A, diff

    !入力
    read (*, *) N, T, A

    !勝敗の確定判定
    diff = N - T - A
    if (T > A) then !高橋氏が優勢
        if (A + diff < T) then
            write (*, '(a)') 'Yes'
            stop
        end if
    else !青木氏が優勢
        if (T + diff < A) then
            write (*, '(a)') 'Yes'
            stop
        end if
    end if
    write (*, '(a)') 'No'
end program abc366a
