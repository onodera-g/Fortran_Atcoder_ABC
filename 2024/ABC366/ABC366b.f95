program abc366b
    !N：文字列の総数
    !M：文字列の最大長さ
    !S：横書きの文字列
    !T：縦書きの文字列
    implicit none
    integer i, j
    integer N, M
    character(100), allocatable::S(:)
    character(1), allocatable::T(:, :)

    !入力
    read (*, *) N
    allocate (S(N))
    M = 0
    do i = 1, N
        read (*, *) S(i)
        M = max(M, len_trim(S(i)))
    end do

    !縦書きに変換
    allocate (T(N, M))
    T = "*" !一旦*で埋めておく
    do i = 1, N
        do j = 1, M
            if (S(i) (j:j) /= "") T(i, j) = S(i) (j:j)
        end do
    end do
    call array_rotate(T, 90) !縦に変換

    !結果の出力
    do i = 1, M
        do j = N, 1, -1
            if (T(i, j) /= "*") exit
        end do
        write (*, '(*(a1))') T(i, 1:j)
    end do
contains
    subroutine array_rotate(array, angle)
        implicit none
        character(len=*), allocatable, intent(inout) :: array(:, :)
        integer, intent(in) :: angle
        character(len=len(array(1, 1))), allocatable :: temp(:, :)
        integer :: i, j, nrows, ncols, new_nrows, new_ncols
        nrows = size(array, 1)
        ncols = size(array, 2)
        select case (mod(angle, 360))
        case (90, 270)
            new_nrows = ncols
            new_ncols = nrows
        case (180)
            new_nrows = nrows
            new_ncols = ncols
        case default
            return
        end select
        allocate (temp(new_nrows, new_ncols))
        select case (mod(angle, 360))
        case (90)
            do i = 1, new_nrows
                do j = 1, new_ncols
                    temp(i, j) = array(nrows - j + 1, i)
                end do
            end do
        case (180)
            do i = 1, new_nrows
                do j = 1, new_ncols
                    temp(i, j) = array(nrows - i + 1, ncols - j + 1)
                end do
            end do
        case (270)
            do i = 1, new_nrows
                do j = 1, new_ncols
                    temp(i, j) = array(j, ncols - i + 1)
                end do
            end do
        end select
        deallocate (array)
        allocate (array(new_nrows, new_ncols))
        array = temp
    end subroutine array_rotate
end program abc366b
