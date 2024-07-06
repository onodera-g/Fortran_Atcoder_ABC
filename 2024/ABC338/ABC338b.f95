program abc338b
    !S       ：文字列
    !char_num：変換後のASCIIコード
    !char_cnt：各アルファベットの出現回数を記録
    !char_max：再頻出の文字の出現回数
    implicit none
    character(1000) S
    integer i, char_num, char_cnt(127), char_max

    !入力
    read (*, *) S

    !各アルファベットの出現回数を記録
    char_cnt = 0
    do i = 1, len_trim(S)
        char_num = ichar(S(i:i))
        char_cnt(char_num) = char_cnt(char_num) + 1
    end do

    !再頻出の文字の出力
    char_max = maxval(char_cnt)
    do i = 1, 127
        if (char_max == char_cnt(i)) then
            write (*, '(a)') char(i)
            stop
        end if
    end do
end program abc338b
