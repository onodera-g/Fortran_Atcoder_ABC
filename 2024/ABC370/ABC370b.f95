program abc370b
    !N：元素の種類数
    !A：Aに並べられた元素
    implicit none
    integer i, j
    integer N
    integer, allocatable::A(:, :)

    !入力
    read (*, *) N
    allocate (A(N, N))
    do i = 1, N
        read (*, *) (A(i, j), j=1, i)
    end do

    !元素の剛性
    i = A(1, 1)
    do j = 2, N
        if (i > j) then
            i = A(i, j)
        else
            i = A(j, i)
        end if
    end do

    !結果の出力
    print *, i

end program abc370b

