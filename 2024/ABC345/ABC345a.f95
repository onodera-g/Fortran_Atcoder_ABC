
program bidirectional_arrow
    !S    ：文字列
    !len_S：文字列Sの文字数
    implicit none
    character(len=100) :: S
    logical :: ans
    integer :: len_S, i

    ! 文字列を入力
    read (*, *) S

    ! 文字列の長さを取得
    len_S = len_trim(S)

    ! 判定を行う関数を呼び出し
    ans = .false.

    !<,=,>の判定
    if (S(1:1) == '<' .and. S(len_S:len_S) == '>') then
        ans = .true.
        do i = 2, len_S - 1 !間に=以外がないか
            if (S(i:i) /= '=') then
                ans = .false.
                exit
            end if
        end do
    end if

    ! 結果を出力
    if (ans .eqv. .true.) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program bidirectional_arrow
