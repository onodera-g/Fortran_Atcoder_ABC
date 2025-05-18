program abc388b
    ! N       : ヘビの本数
    ! D       : 長さを増やす日数の回数
    ! T(i)    : i番目のヘビの厚さ
    ! L(i)    : i番目のヘビの長さ
    ! max_w   : 現在の日数 k における最重量
    ! w       : 各ヘビの重量
    implicit none
    integer :: N, D
    integer, allocatable :: T(:), L(:)
    integer :: i, k
    integer :: max_w, w

    ! 入力
    read (*, *) N, D
    allocate (T(N), L(N))
    do i = 1, N
        read (*, *) T(i), L(i)
    end do

    ! 各 k = 1…D で全ヘビの重量を計算し、最大値を出力
    do k = 1, D
        max_w = 0
        do i = 1, N
            w = T(i)*(L(i) + k)
            if (w > max_w) then
                max_w = w
            end if
        end do
        print *, max_w
    end do

end program abc388b
