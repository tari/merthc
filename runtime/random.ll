
@x = private unnamed_addr global i32 0
@y = private unnamed_addr global i32 0
@z = private unnamed_addr global i32 0
@w = private unnamed_addr global i32 0

define hidden void @rand_init([4 x i32]* %buf) {
    %b0 = getelementptr inbounds [4 x i32], [4 x i32]* %buf, i32 0, i32 0
    %b1 = getelementptr inbounds [4 x i32], [4 x i32]* %buf, i32 0, i32 1
    %b2 = getelementptr inbounds [4 x i32], [4 x i32]* %buf, i32 0, i32 2
    %b3 = getelementptr inbounds [4 x i32], [4 x i32]* %buf, i32 0, i32 3
    %1 = load i32, i32* %b0
    %2 = load i32, i32* %b1
    %3 = load i32, i32* %b2
    %4 = load i32, i32* %b3
    store i32 %1, i32* @x
    store i32 %2, i32* @y
    store i32 %3, i32* @z
    store i32 %4, i32* @w
    ret void
}

define hidden i32 @rand() {
;    uint32_t t = x;
    %t0 = load i32, i32* @x
    %tsl11 = shl i32 %t0, 11
    %tsr8 = lshr i32 %t0, 8
;    t ^= t << 11;
    %t1 = xor i32 %t0, %tsl11
;    t ^= t >> 8;
    %t2 = xor i32 %t1, %tsr8
;    x = y;
    %1 = load i32, i32* @y
    store i32 %1, i32* @x
;    y = z;
    %2 = load i32, i32* @z
    store i32 %2, i32* @y
;    z = w;
    %3 = load i32, i32* @w
    store i32 %2, i32* @z
;    w ^= w >> 19;
    %w0 = load i32, i32* @w
    %wsr19 = lshr i32 %w0, 19
    %w1 = xor i32 %w0, %wsr19
;    w ^= t;
    %w2 = xor i32 %w1, %t2
    store i32 %w2, i32* @w
;    return w;
    ret i32 %w2
}
