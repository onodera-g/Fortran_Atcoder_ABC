program ABC402a
    ! S_raw    : 入力される文字列（英大文字・小文字混在、最大長100）
    ! n        : 実際の文字列長
    ! i        : 走査用ループ変数
    ! m        : 抽出した大文字の個数（結果文字列の長さ）
    ! UpStr    : 大文字のみを連結して格納するバッファ

    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
    character(len=100) :: S_raw
    integer(int32)   :: n, i, m
    character(len=100) :: UpStr

    ! 入力
    read (*, '(A)') S_raw
    n = len_trim(S_raw)

    ! 大文字抽出
    m = 0
    do i = 1, n
        if (iachar(S_raw(i:i)) >= iachar('A') .and. iachar(S_raw(i:i)) <= iachar('Z')) then
            m = m + 1
            UpStr(m:m) = S_raw(i:i)
        end if
    end do

    ! 結果の出力
    if (m > 0) then
        write (*, '(A)') UpStr(1:m)
    else
        ! 大文字が一つもなければ空行を出力
        write (*, *)
    end if

end program ABC402a
