program abc357b
    !S    ：文字列
    !Ans  ：Sを変換した文字列
    !small：小文字の数
    !large：大文字の数
    !n    ：文字列Sの文字数
    implicit none
    character(99) S, Ans
    integer i
    integer small, large, n

    !入力
    read (*, *) S
    n = len_trim(S)
    small = 0
    large = 0

    !大文字小文字の分類
    do i = 1, n
        if (ichar(S(i:i)) < 97) then
            large = large + 1
        else
            small = small + 1
        end if
    end do

    !どちらをおきくするか
    if (small > large) then
        do i = 1, n
            if (ichar(S(i:i)) < 97) then
                Ans(i:i) = char(ichar(S(i:i)) + 32)
            else
                Ans(i:i) = S(i:i)
            end if
        end do
    else
        do i = 1, n
            if (ichar(S(i:i)) > 96) then
                Ans(i:i) = char(ichar(S(i:i)) - 32)
            else
                Ans(i:i) = S(i:i)
            end if
        end do
    end if

    !結果の出力
    write (*, *) Ans(1:n)

end program abc357b

