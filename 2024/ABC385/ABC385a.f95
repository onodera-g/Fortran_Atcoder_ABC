program abc385a
    ! A,B,C : 整数A,B,C
    implicit none
    integer A, B, C

    ! 入力
    read (*, *) A, B, C

    ! A,B,Cの組み合わせを全部試す
    if (A + B == C) then
        write (*, '(a)') 'Yes'
    elseif (A + C == B) then
        write (*, '(a)') 'Yes'
    else if (B + C == A) then
        write (*, '(a)') 'Yes'
    else if (B == C .and. C == A) then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if

end program abc385a
