program ABC337b
    !S    ：文字列S
    !len_S：Sの文字数
    implicit none
    character(100) S
    integer len_S, i, check

    !入力
    read (*, *) S
    len_S = len_trim(S)
    check = 1

    !拡張文字の判定
    do i = 2, len_S
        if (S(i - 1:i) == 'BA' .or. S(i - 1:i) == 'CA' .or. S(i - 1:i) == 'CB') then
            check = 0
            exit
        end if
    end do

    !結果の出力
    if (check == 1) then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if
end program ABC337b
