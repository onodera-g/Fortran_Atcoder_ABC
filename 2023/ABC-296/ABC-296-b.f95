program ABC296B
    implicit none
    integer i, j
    integer tate, yoko
    character(8) S(8), alphabet, kazu
    alphabet = 'abcdefgh'
    kazu = '87654321'

    !入力
    do i = 1, 8
        read (*, *) S(i)
    end do

    !コマの位置検索
    do i = 8, 1, -1
        do j = 1, 8
            if (S(i) (j:j) == '*') then
                yoko = j
                tate = i
            end if
        end do
    end do

    !結果の出力
    write (*, '(a1,a1)') alphabet(yoko:yoko), kazu(tate:tate)
end program ABC296B

