program abc349b
    !S   ：文字列S
    !mozi：Sの中に含まれる各アルファベットの出現回数
    !cnt ：各アルファベットの出現回数の出現回数
    implicit none
    integer i, j
    integer mozi(127), cnt(100)
    character(100) S

    !入力
    read (*, *) S
    mozi = 0; cnt = 0

    !各アルファベットの出現回数
    do i = 1, len_trim(S)
        do j = 97, 122 !'a'=97,122='z'
            if (S(i:i) == char(j)) then
                mozi(j) = mozi(j) + 1
                exit
            end if
        end do
    end do

    !出現回数の出現回数の計上
    do i = 97, 122 !'a'=97,122='z'
        cnt(mozi(i)) = cnt(mozi(i)) + 1
    end do

    !結果の出力
    do i = 1, 100
        if (cnt(i) == 0 .or. cnt(i) == 2) then
            cycle
        else
            print *, 'No'
            stop
        end if
    end do
    print *, 'Yes'
end program abc349b
