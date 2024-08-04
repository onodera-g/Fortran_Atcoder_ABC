program abc352b
    !S；入力したかった文字
    !T：誤って入力した文字
    implicit none
    character(2*10**5) S, T
    integer i, j

    !入力
    read (*, *) S
    read (*, *) T

    !結果の出力
    !１文字ずつ文字が一致しているか確かめる
    j = 1
    do i = 1, len(T)
        !一致している場合
        if (S(i:i) == T(j:j)) then
            write (*, '(i0,1x)', advance='no') j
            j = j + 1
            !一致していない場合
        else
            !次に一致する位置まで進める
            do
                if (S(i:i) /= T(j:j)) then
                    j = j + 1
                else
                    write (*, '(i0,1x)', advance='no') j
                    j = j + 1
                    exit
                end if
            end do
        end if
        if (S(i + 1:i + 1) == "") stop
    end do
end program abc352b
