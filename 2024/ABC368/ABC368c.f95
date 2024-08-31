program abc368c
    !N：敵の人数
    !H：それぞれ敵のHP
    !T：合計の攻撃回数
    implicit none
    integer(16) i
    integer(16) N, T, r, tmp1, tmp2
    integer(16), allocatable::H(:)

    !入力
    read (*, *) N
    allocate (H(N))
    read (*, *) H

    tmp1 = 3; tmp2 = 5
    T = 0
    do i = 1, N
        T = T + H(i)/5*3 !1,1,3を1セットで考えて、何セット作れるか
        r = mod(H(i), tmp2) !上記のセットの端数
        do while (r > 0) !端数分の集計
            T = T + 1
            if (mod(T, tmp1) == 0) then
                r = r - 3
            else
                r = r - 1
            end if
        end do
    end do

    !結果の出力
    write (*, *) T
end program abc368c
