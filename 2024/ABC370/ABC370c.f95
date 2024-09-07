program abc370c
    !S    ：元の文字列
    !T    ：書き換え後の文字列
    !lenST：文字列S,Tの長さ
    !X    ：書き換え過程の文字列
    !cnt  ：文字列の書き換え回数
    !flag ：SとTの一致する文字数
    implicit none
    integer i, j
    integer lenST, cnt, flag
    character(100) S, T
    character(:), allocatable::X(:)

    !入力
    read (*, *) S
    read (*, *) T
    lenST = len_trim(S)
    allocate (character(lenST)::X(lenST))

    !操作不要(SとTが完全に一致)
    if (S == T) then
        print *, 0
        stop
    end if

    !不一致箇所の検索
    cnt = 1
    do
        flag = 0
        do j = 1, lenST
            if (S(j:j) == T(j:j)) flag = flag + 1
            if (S(j:j) /= T(j:j) .and. ichar(S(j:j)) > ichar(T(j:j))) then
                S(j:j) = T(j:j)
                X(cnt) = S
                cnt = cnt + 1
            end if
        end do
        if (flag == lenST) then
            exit
        else
            do j = lenST, 1, -1
                if (S(j:j) /= T(j:j)) then
                    S(j:j) = T(j:j)
                    X(cnt) = S
                    cnt = cnt + 1
                end if
            end do
        end if
    end do

    write (*, '(i0)') cnt - 1
    do i = 1, cnt - 1
        write (*, '(a)') X(i)
    end do
end program abc370c
