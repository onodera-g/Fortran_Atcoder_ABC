program aABC98a
    implicit none
    integer i, j, k
    integer N, cnt_o, cnt_x
    character(:), allocatable ::S

    !入力
    read (*, *) N
    allocate (character(N) :: S)
    read (*, *) S

    !合否判定
    cnt_o = 0; cnt_x = 0
    do i = 1, N
        if (S(i:i) == 'o') then
            cnt_o = cnt_o + 1
        else if (S(i:i) == 'x') then
            cnt_x = cnt_x + 1
        end if
    end do

    !結果の出力
    if (cnt_x == 0 .and. cnt_o > 0) then
        print *, 'Yes'
    else
        print *, 'No'
    end if
end program
