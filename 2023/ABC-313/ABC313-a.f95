program ABC313a
    !N    ：人数
    !max_P：プログラミング力の最大値
    !max_i：最大のプログラミング値を持つ人のナンバー
    implicit none
    integer i
    integer N, max_i, max_P
    integer, allocatable::P(:)

    !入力
    read (*, *) N
    allocate (P(N))
    read (*, *) P

    !レートの最大値の検索
    max_P = P(1); max_i = 1
    do i = 2, N
        if (max_P <= P(i)) then
            max_P = P(i)
            max_i = i
        end if
    end do

    !結果の出力
    if (max_i == 1) then
        print *, 0
    else
        print *, max_P - P(1) + 1
    end if
end program ABC313a
