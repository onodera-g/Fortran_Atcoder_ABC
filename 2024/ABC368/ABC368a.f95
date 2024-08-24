program abc368a
    !N  ：カードの枚数
    !K  ：下から取り出すカードの枚数
    !A  ：カードに書かれている数字
    !B  ：並び替えた後のカードの数字
    !cnt：
    implicit none
    integer i, tmp
    integer N, K, cnt
    integer, allocatable::A(:), B(:)

    !入力
    read (*, *) N, K
    allocate (A(N), B(N))
    read (*, *) A

    !カードの並び替え
    cnt = 1
    tmp = N - K + 1
    !K枚カードを上にする
    do i = tmp, N
        B(cnt) = A(i)
        cnt = cnt + 1
    end do
    !残りを配置
    do i = 1, tmp - 1
        B(cnt) = A(i)
        cnt = cnt + 1
    end do

    !結果の出力
    write (*, '(*(i0,1x))') B

end program abc368a
