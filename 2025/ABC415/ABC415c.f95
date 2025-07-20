program abc415c
    ! T              : テストケースの数
    ! N              : 薬品の種類数
    ! S              : 状態の危険情報を格納する文字列
    ! ok(2**18)      : 部分集合ごとの安全性を表す配列
    ! dp(2**18)      : DP配列、状態に到達可能かどうか
    ! full           : 全薬品が混ざった状態のビットマスク
    ! mask           : 部分集合のビットマスク
    ! i, j, k        : ループ変数

    implicit none
    integer :: T, N, i, j, k, full, mask
    character(len=262144) :: S
    logical, allocatable :: ok(:), dp(:)

    ! テストケース数
    read (*, *) T

    do i = 1, T
        ! 入力
        read (*, *) N
        read (*, *) S(1:(2**N - 1))

        allocate (ok(0:2**N - 1))
        allocate (dp(0:2**N - 1))
        ok = .true.
        dp = .false.

        ! 危険状態をマーク
        do j = 1, 2**N - 1
            if (S(j:j) == '1') then
                ok(j) = .false.
            end if
        end do

        dp(0) = .true.

        ! DP で順に状態を拡張
        do mask = 0, 2**N - 1
            if (.not. dp(mask)) cycle
            do k = 0, N - 1
                if (iand(mask, ishft(1, k)) /= 0) cycle
                if (ok(ior(mask, ishft(1, k)))) then
                    dp(ior(mask, ishft(1, k))) = .true.
                end if
            end do
        end do

        full = 2**N - 1
        if (dp(full)) then
            print *, 'Yes'
        else
            print *, 'No'
        end if

        deallocate (ok, dp)
    end do
end program abc415c
