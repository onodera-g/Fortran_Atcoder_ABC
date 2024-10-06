program abc374a
    ! S    : 文字列S(sanを含む)
    ! lenS : S の長さ
    implicit none
    integer lenS
    character(30) S

    !入力
    read (*, *) S
    lenS = len_trim(S)

    !出力
    if (S(lenS - 2:lenS) == "san") then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program abc374a
