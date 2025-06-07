program ABC409a
    ! N         整数列の長さ
    ! T         高橋君の欲しい商品を表す文字列 ('o' または 'x')
    ! A         青木君の欲しい商品を表す文字列 ('o' または 'x')
    ! i         ループ用インデックス (1 ≤ i ≤ N)
    ! found     ふたりとも欲しがる商品が見つかったかを示す論理値

    use, intrinsic :: iso_fortran_env
    implicit none

    integer(int32) :: N
    character(len=:), allocatable :: T, A
    integer(int32) :: i
    logical :: found

    ! 入力
    read (*, *) N
    allocate (character(len=N) :: T)
    allocate (character(len=N) :: A)
    read (*, '(A)') T
    read (*, '(A)') A
    found = .false.

    ! ふたりとも 'o' の位置があるかチェック
    do i = 1, N
        if (T(i:i) == 'o' .and. A(i:i) == 'o') then
            found = .true.
            exit
        end if
    end do

    ! 結果の出力
    if (found) then
        print *, "Yes"
    else
        print *, "No"
    end if

end program ABC409a
