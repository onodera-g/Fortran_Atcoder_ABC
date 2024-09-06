program abc360a
    !S    ：料理の並び順
    !R_num：ご飯の設置位置
    !M_num：味噌汁の設置位置

    implicit none
    integer i
    integer R_num, M_num
    character(3) S

    !入力
    read (*, *) S

    !サラダ、ご飯、味噌汁の位置決定
    do i = 1, 3
        if (S(i:i) == 'R') then
            R_num = i
        else if (S(i:i) == 'M') then
            M_num = i
        end if
    end do

    !結果の出力
    if (R_num > M_num) then
        print *, 'No'
    else
        print *, 'Yes'
    end if

end program abc360a
