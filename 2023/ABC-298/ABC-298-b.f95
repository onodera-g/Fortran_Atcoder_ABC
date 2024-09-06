program ABC298b
    implicit none
    integer i, j, k
    integer N, tmp
    integer, allocatable ::A(:, :), B(:, :)
    logical flag

    read (*, *) N
    allocate (A(N, N), B(N, N))
    do i = 1, N
        read (*, *) (A(i, j), j=1, N)
    end do
    do i = 1, N
        read (*, *) (B(i, j), j=1, N)
    end do

    !判定
    tmp = -90
    do i = 1, 4
        !回転
        tmp = tmp + 90
        call array_rotate(A, tmp)
        flag = .true.
        !
        do j = 1, N
            do k = 1, N
                if (A(j, k) == 1 .and. B(j, k) == 0) flag = .false.
            end do
        end do
        !判定
        if (flag .eqv. .true.) then
            print *, 'Yes'
            stop
        end if
    end do
    print *, 'No'
contains
    subroutine array_rotate(array, angle)
        implicit none
        integer, allocatable, intent(inout) :: array(:, :)
        integer, intent(in) :: angle
        integer, allocatable :: temp(:, :)
        integer i, j, nrows, ncols, new_nrows, new_ncols

        nrows = size(array, 1)
        ncols = size(array, 2)

        ! 新しい配列のサイズを決定
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

        ! 一時配列を割り当て
        allocate (temp(new_nrows, new_ncols))

        ! 配列を回転
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

        ! 元の配列を再割り当てして結果を保存
        deallocate (array)
        allocate (array(new_nrows, new_ncols))
        array = temp

        ! メモリ解放
        deallocate (temp)
    end subroutine array_rotate
end program
