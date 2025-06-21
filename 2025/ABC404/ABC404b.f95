program ABC404b
    ! N              : グリッドのサイズ
    ! S(i,j), T(i,j) : 入力グリッド

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    integer(int32) :: N, i, j, rot
    character(len=100) :: line
    character(len=1), allocatable :: S(:, :), T(:, :)
    integer(int32) :: best, mismatches, total_ops
    character(len=1) :: c

    ! 入力
    read (*, *) N
    allocate (S(N, N), T(N, N))
    do i = 1, N
        read (*, '(A)') line
        do j = 1, N
            S(i, j) = line(j:j)
        end do
    end do
    do i = 1, N
        read (*, '(A)') line
        do j = 1, N
            T(i, j) = line(j:j)
        end do
    end do

    ! 各回転で試す (0, 90, 180, 270 度)
    best = huge(0_int32)
    do rot = 0, 3
        mismatches = 0
        do i = 1, N
            do j = 1, N
                select case (rot)
                case (0)
                    c = S(i, j)
                case (1)
                    c = S(N - j + 1, i)
                case (2)
                    c = S(N - i + 1, N - j + 1)
                case (3)
                    c = S(j, N - i + 1)
                end select
                if (c /= T(i, j)) mismatches = mismatches + 1
            end do
        end do
        total_ops = rot + mismatches
        if (total_ops < best) best = total_ops
    end do

    ! 結果の出力
    write (*, '(I0)') best

end program ABC404b
