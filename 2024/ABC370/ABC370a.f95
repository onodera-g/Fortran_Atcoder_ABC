program abc370a
    !L：左手の位置(1：挙げる、０：下げる)
    !R：右手の位置(1：挙げる、０：下げる)
    implicit none
    integer L, R

    !入力
    read (*, *) L, R

    !結果の出力
    if (L == 1 .and. R == 1) then
        print *, 'Invalid '
    else if (L == 1 .and. R == 0) then
        print *, 'Yes'
    else if (L == 0 .and. R == 1) then
        print *, 'No'
    else if (L == 0 .and. R == 0) then
        print *, 'Invalid '
    end if
end program abc370a
