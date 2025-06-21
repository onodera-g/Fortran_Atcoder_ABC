program ABC406c
    ! N              : 項目数 (P の長さ)
    ! P              : 順列データ配列
    ! S              : P の隣接要素大小関係を表す文字列 ('<' または '>')
    ! run_char       : ランレングス圧縮後の文字 ('<' または '>')
    ! run_len        : ランレングス圧縮後の各連続長
    ! run_count      : ランレングス圧縮後の要素数
    ! i, r, k        : ループ用インデックス
    ! ans            : パターン "<+>+<+" の個数 (64bit)

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer :: N, i, r, run_count, k
    integer(int64) :: ans
    integer, allocatable :: P(:)
    character(len=1), allocatable :: S(:)
    integer, allocatable :: run_len(:)
    character(len=1), allocatable :: run_char(:)

    ! 入力
    read (*, *) N
    allocate (P(N))
    read (*, *) (P(i), i=1, N)

    ! N < 4 のときは 0
    if (N < 4) then
        print *, 0
        stop
    end if

    ! S(i) = '<' if P(i)<P(i+1) else '>'
    allocate (S(N - 1))
    do i = 1, N - 1
        if (P(i) < P(i + 1)) then
            S(i) = '<'
        else
            S(i) = '>'
        end if
    end do

    ! ランレングス圧縮
    allocate (run_len(N - 1), run_char(N - 1))
    run_count = 0
    i = 1
    do while (i <= N - 1)
        r = 1
        do while (i + r <= N - 1 .and. S(i + r) == S(i))
            r = r + 1
        end do
        run_count = run_count + 1
        run_char(run_count) = S(i)
        run_len(run_count) = r
        i = i + r
    end do

    ! パターン "<+>+<+" の数を数える
    ans = 0_int64
    do k = 2, run_count - 1
        if (run_char(k - 1) == '<' .and. run_char(k) == '>' .and. run_char(k + 1) == '<') then
            ans = ans + int(run_len(k - 1), int64)*int(run_len(k + 1), int64)
        end if
    end do

    ! 結果の出力
    print *, ans
end program ABC406c
