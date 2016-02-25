
@x = private unnamed_addr global i64 0

define hidden void @rand_init(i64 %seed) {
    store i64 %seed, i64* @x
    ret void
}

; 64-bit xorshift*
define hidden i64 @rand() {
    %x0 = load i64, i64* @x
;   x ^= x >> 12;
    %xr12 = lshr i64 %x0, 12
    %x1 = xor i64 %x0, %xr12
;   x ^= x << 25;
    %xl25 = shl i64 %x1, 25
    %x2 = xor i64 %x1, %xl25
;   x ^= x >> 27;
    %xr27 = lshr i64 %x2, 27
    %x3 = xor i64 %x2, %xr27
    store i64 %x3, i64* @x

;   return x * 2685821657736338717;
    %out = mul i64 %x3, 2685821657736338717
    ret i64 %out
}

define hidden i8 @rand_inrange(i8 %max0) {
    %1 = call i64 @rand()
    %2 = uitofp i64 %1 to double
    %3 = fdiv fast double %2, uitofp(i65 shl(i65 1, i65 64) to double)
    ; %3 is in range [0, 1)
    %max = uitofp i8 %max0 to double
    %4 = fmul fast double %3, %max
    %5 = fptoui double %4 to i8
    ret i8 %5
}

declare void @print(i8*, i8)

define hidden void @rand_string(i8 %len) {
    %charmem = alloca i8, i8 %len
    br label %loop

loop:
    %idx = phi i8 [0, %0], [%idxinc, %loop_cont]
    %equal = icmp eq i8 %idx, %len
    br i1 %equal, label %done, label %loop_cont

loop_cont:
    ; Generate and store a character
    %char_ = call i8 @rand_inrange(i8 26)
    %char = add i8 %char_, 97

    %charp = getelementptr inbounds i8, i8* %charmem, i8 %idx
    store i8 %char, i8* %charp

    %idxinc = add i8 %idx, 1
    br label %loop

done:
    call void @print(i8* %charmem, i8 %len)
    ret void
}
