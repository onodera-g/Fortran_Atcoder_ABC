program abc356b
    !N  ：食べた食品の数
    !M  ：栄養の種類
    !A  ：各栄養素の1日の摂取目標
    !X  ：各食品から摂取した栄養
    !Ans：目標達成の有無、0ならNo、1ならYes
    implicit none
    integer i, j
    integer N, M
    integer, allocatable::A(:), X(:, :), Ans(:)

    !入力
    read (*, *) N, M
    allocate (A(M), X(N, M), Ans(M))
    read (*, *) A(:)
    do i = 1, N
        read (*, *) (X(i, j), j=1, M)
    end do
    Ans = 0

    !接種した各栄養素の合計
    do i = 1, N
        do j = 1, M
            Ans(j) = Ans(j) + X(i, j)
        end do
    end do

    !各栄養素が足りているかを判定
    do i = 1, M
        !write (*, *) Ans(i)
        if (Ans(i) < A(i)) then
            write (*, '(a)') 'No'
            stop
        end if
    end do

    !結果の出力
    write (*, '(a)') 'Yes'

end program abc356b
