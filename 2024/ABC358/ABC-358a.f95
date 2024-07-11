program abc358a
    !S：文字列
    !T：文字列
    implicit none
    character(10) S, T

    !入力
    read (*, *) S, T

    !SとTが一致するかを判定
    if (S == "AtCoder" .and. T == "Land") then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if
end program abc358a
