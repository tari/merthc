#include <stdint.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm-c/Core.h>

typedef int (*cb_t)(const void *, size_t, void *);

class raw_callback_ostream : public llvm::raw_ostream {
    uint64_t offset;
    cb_t callback;
    void *callback_data;

public:
    raw_callback_ostream(cb_t cb, void *cb_data)
        : offset(0), callback(cb), callback_data(cb_data) { }

    ~raw_callback_ostream() {
        flush();
    }

private:
    void write_impl(const char *p, size_t sz) override {
        callback(p, sz, callback_data);
        offset += sz;
    }

    uint64_t current_pos() const override {
        return offset;
    }
};

extern "C" void PrintModuleIR(LLVMModuleRef M,
                              cb_t cb,
                              void *cb_data) {
    raw_callback_ostream out(cb, cb_data);
    out << *llvm::unwrap(M);
}
