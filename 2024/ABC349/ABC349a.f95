program abc349a
    !N  ：ゲームの参加人数
    !A  ：参加者の持ち点
    !ans：人Nの持ち点
    implicit none
    integer i
    integer N, ans
    integer, allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(N - 1))
    read (*, *) A

    !人Nの持ち点計算
    ans = 0
    do i = 1, N - 1
        ans = ans + A(i)
    end do
    ans = -ans

    !結果の出力
    print *, ans
end program abc349a
