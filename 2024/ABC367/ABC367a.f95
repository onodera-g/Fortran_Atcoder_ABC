program abc367a
    !A：たこ焼きへの愛を叫ぶ時刻
    !B：就寝時刻
    !C：起床時刻
    implicit none
    integer i, A, B, C

    !入力
    read (*, *) A, B, C

    !愛を叫べるか判定、結果の出力
    if (C > B) then !日を跨がない場合
        do i = B, C
            if (i == A) then
                write (*, '(a)') 'No'
                stop
            end if
        end do
    else if (B > C) then !日を跨ぐ場合
        do i = B, 23 !24時まで
            if (i == A) then
                write (*, '(a)') 'No'
                stop
            end if
        end do
        do i = 0, C !24時以降
            if (i == A) then
                write (*, '(a)') 'No'
                stop
            end if
        end do
    end if
    write (*, '(a)') 'Yes'
end program abc367a
