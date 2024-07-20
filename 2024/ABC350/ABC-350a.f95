program abc350a
    !num：コンテストの番号
    !ABC：文字列ABC
    implicit none
    integer :: num
    character(3) :: ABC

    !入力
    read (*, '(a3,i3)') ABC, num

    !結果の出力
    if (num == 316) then !316は未開催
        write (*, '(a)') 'No'
    elseif (0 < num .and. num < 350) then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if

end program abc350a
