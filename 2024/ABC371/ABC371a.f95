program abc371a
    !SAB:A と B の年齢関係を示す。'<'ならAはBより若い、'>'ならAはBより年上。
    !SAC:A と C の年齢関係を示す。'<'ならAはCより若い、'>'ならAはCより年上。
    !SBC:B と C の年齢関係を示す。'<'ならBはCより若い、'>'ならBはCより年上。
    character(1) SAB, SAC, SBC
    character(1) middle

    !入力
    read (*, *) SAB, SAC, SBC

    !各パターンに基づいて次男を決定する
    if (SAB == '<' .and. SAC == '<' .and. SBC == '<') then
        middle = 'B'
    elseif (SAB == '<' .and. SAC == '<' .and. SBC == '>') then
        middle = 'C'
    elseif (SAB == '<' .and. SAC == '>' .and. SBC == '<') then
        middle = 'A'
    elseif (SAB == '<' .and. SAC == '>' .and. SBC == '>') then
        middle = 'A'
    elseif (SAB == '>' .and. SAC == '<' .and. SBC == '<') then
        middle = 'A'
    elseif (SAB == '>' .and. SAC == '<' .and. SBC == '>') then
        middle = 'A'
    elseif (SAB == '>' .and. SAC == '>' .and. SBC == '<') then
        middle = 'C'
    elseif (SAB == '>' .and. SAC == '>' .and. SBC == '>') then
        middle = 'B'
    end if

    !結果の出力
    print *, middle
end program abc371a
