program abc359a
    !S  ：文字列(TakahashiかAokiが入っている
    !N  ：文字列Sの個数
    !cnt：Takahashiの出現回数
    implicit none
    character(10), allocatable:: S(:)
    integer N, i, cnt

    !入力
    read (*, *) N
    allocate (S(N))
    do i = 1, N
        read (*, *) S(i)
    end do
    cnt = 0

    !Takahashi or Aoki の判定
    do i = 1, N
        if (trim(S(i)) == "Takahashi") then
            cnt = cnt + 1
        end if
    end do

    !結果の出力
    write (*, *) cnt

end program abc359a
