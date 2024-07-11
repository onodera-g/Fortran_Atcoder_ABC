program abc358b
    !N：チケットの購入人数
    !A：チケット購入にかかる時間
    !T：チケット売り場に訪れる時間
    !ans：合計でかかった時間
    implicit none
    integer N, A
    integer ans, i
    integer, allocatable::T(:)

    !入力
    read (*, *) N, A
    allocate (T(N))
    read (*, *) T(:)
    ans = 0

    !待ち列の処理
    do i = 1, N
        if (ans > T(i)) then
            ans = ans + A
        else
            ans = T(i) + A
        end if
        write (*, *) ans
    end do

end program abc358b
