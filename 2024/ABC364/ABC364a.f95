program abc364a
    !N  ：料理の総数
    !S  ：各料理の味
    !cnt：お腹いっぱいになった時の食事数
    implicit none
    integer i
    integer N, cnt
    character(5), allocatable::S(:)

    !入力
    read (*, *) N
    allocate (S(N))
    do i = 1, N
        read (*, *) S(i)
    end do

    !食事のシミュレーション
    cnt = N
    do i = 1, N - 1
        !２回連続甘い食事であるか
        if (S(i) == "sweet" .and. S(i + 1) == "sweet") then
            cnt = i + 1
            exit
        end if
    end do

    !結果の出力
    if (cnt == N) then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if

end program abc364a
