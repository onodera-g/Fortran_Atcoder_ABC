program ABC412b
    ! S           : 入力文字列 S
    ! T           : 入力文字列 T
    ! i           : ループ用インデックス
    ! lenS        : S の実際の長さ
    ! prev_char   : S の i-1 文字目
    ! curr_char   : S の i   文字目
    ! pos         : T 中の文字位置 (0: 未発見)

    implicit none
    character(len=100) :: S, T
    integer :: i, lenS, pos
    character :: prev_char, curr_char

    ! 入力
    read (*, '(A)') S
    read (*, '(A)') T

    !判定
    lenS = len_trim(S)
    do i = 2, lenS
        curr_char = S(i:i)
        ! 英大文字かどうか判定
        if (ichar(curr_char) >= ichar('A') .and. ichar(curr_char) <= ichar('Z')) then
            prev_char = S(i - 1:i - 1)
            ! T に含まれるか検索
            pos = index(T, prev_char)
            if (pos == 0) then
                print *, 'No'
                stop
            end if
        end if
    end do

    !結果の出力
    print *, 'Yes'

end program ABC412b
