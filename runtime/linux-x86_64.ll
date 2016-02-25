; Linux x86_64 target runtime library
target triple = "x86_64-unknown-linux"

@__NR_read = private constant i64 0
@__NR_write = private constant i64 1
@__NR_open = private constant i64 2
@__NR_exit = private constant i64 60

@STDOUT_FILENO = private constant i32 1

define private i64 @syscall1(i64 %nr, i64 %p1) inlinehint {
    %1 = call i64 asm sideeffect "syscall",
        "={rax},{rax},{rdi}"(i64 %nr, i64 %p1)
    ret i64 %1
}

define private i64 @syscall2(i64 %nr, i64 %p1, i64 %p2) inlinehint {
    %1 = call i64 asm sideeffect "syscall",
        "={rax},{rax},{rdi},{rsi}"(i64 %nr, i64 %p1, i64 %p2)
    ret i64 %1
}

define private i64 @syscall3(i64 %nr, i64 %p1, i64 %p2, i64 %p3) inlinehint {
    %1 = call i64 asm sideeffect "syscall",
        "={rax},{rax},{rdi},{rsi},{rdx}"(i64 %nr, i64 %p1, i64 %p2, i64 %p3)
    ret i64 %1
}

; ===

define private i32 @open(i8* %path0, i32 %flags0) {
    %nr = load i64, i64* @__NR_open
    %path = ptrtoint i8* %path0 to i64
    %flags = zext i32 %flags0 to i64
    %out0 = call i64 @syscall2(i64 %nr, i64 %path, i64 %flags)
    %out = trunc i64 %out0 to i32
    ret i32 %out
}

define private i64 @read(i32 %fd0, i8* %buf0, i64 %len) {
    %nr = load i64, i64* @__NR_read
    %fd = zext i32 %fd0 to i64
    %buf = ptrtoint i8* %buf0 to i64
    %out = call i64 @syscall3(i64 %nr, i64 %fd, i64 %buf, i64 %len)
    ret i64 %out
}

define private void @exit(i32 %code0) noreturn {
    %nr_exit = load i64, i64* @__NR_exit
    %code = zext i32 %code0 to i64
    call i64 @syscall1(i64 %nr_exit, i64 %code)
    unreachable
}

; ===

declare void @main()
declare void @rand_init([4 x i32]*)

@urandom = private unnamed_addr constant [13 x i8] c"/dev/urandom\00"

define void @_start() noreturn {
    ; Initialize RNG
    %rb = alloca [4 x i32]
    %rb1 = bitcast [4 x i32]* %rb to i8*
    ;int fd = open("/dev/urandom", O_RDONLY)
    %path = getelementptr inbounds [13 x i8], [13 x i8]* @urandom, i64 0, i64 0
    %fd = call i32 @open(i8* %path, i32 0)
    ;read(fd, rb1, 16), bail if we failed to read
    %read_res = call i64 @read(i32 %fd, i8* %rb1, i64 16)
    %read_failed = icmp ne i64 %read_res, 16
    br i1 %read_failed, label %exit_fail, label %invoke_main

; Actually do rand initialization and call main()
invoke_main:
    call void @rand_init([4 x i32]* %rb)
    call void @main()
    call void @exit(i32 0)
    unreachable

exit_fail:
    call void @exit(i32 1)
    unreachable
}

; Runtime support function to print characters to stdout
define hidden void @print(i8* %buf0, i8 %len0) inlinehint {
    %nr = load i64, i64* @__NR_write
    %fileno0 = load i32, i32* @STDOUT_FILENO
    %fileno = zext i32 %fileno0 to i64
    %buf = ptrtoint i8* %buf0 to i64
    %len = zext i8 %len0 to i64
    call i64 @syscall3(i64 %nr, i64 %fileno, i64 %buf, i64 %len)
    ret void
}
