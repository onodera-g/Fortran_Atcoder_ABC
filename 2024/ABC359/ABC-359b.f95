program abc359b
    !N：並んでいる人の数２N
    !A：並んでいる人が綺麗る服の色
    !cnt：同じ色の服の人に挟まれている人の人数
    implicit none
    integer N
    integer i, cnt
    integer, allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(2*N))
    read (*, *) A(:)
    cnt = 0

    !同じ色の服の人に挟まれている人の判定
    do i = 1, 2*N - 2
        if (A(i) == A(i + 2)) then
            cnt = cnt + 1
        end if
    end do

    !結果の出力
    write (*, *) cnt

end program abc359b
