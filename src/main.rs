extern crate libc;
extern crate llvm_sys as llvm;
extern crate phf;

use std::{mem, slice, ptr};
use std::fs::File;

use libc::size_t;

use llvm::core::*;
use llvm::ir_reader::LLVMParseIRInContext;
use llvm::target::{
    LLVM_InitializeNativeTarget,
    LLVM_InitializeNativeAsmPrinter,
    LLVM_InitializeNativeAsmParser
};
use llvm::prelude::*;


fn main() {
    unsafe {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();
    }
    let ctxt = unsafe { LLVMContextCreate() };

    // Load and optimize the runtime files.
    let target = "x86_64-uknown-linux";
    let optimize = true;
    let runtime_modules = load_runtime_for_target(ctxt, target, optimize).expect("Failed to load runtime");

    // Build main()
    let main_module = unsafe {
        let ty_void = LLVMVoidType();
        let main_module = LLVMModuleCreateWithNameInContext(b"main\0".as_ptr() as *const _, ctxt);
        let ty_fn_main = LLVMFunctionType(ty_void, ptr::null_mut(), 0, 0);
        let main_function = LLVMAddFunction(main_module, b"main\0".as_ptr() as *const _, ty_fn_main);
        let bb_main = LLVMAppendBasicBlockInContext(ctxt, main_function, b"\0".as_ptr() as *const _);
        let b = LLVMCreateBuilderInContext(ctxt);
        LLVMPositionBuilderAtEnd(b, bb_main);
        LLVMBuildRetVoid(b);

        main_module
    };

    // Combine main() and runtime with LTO
    let main_module = optimize_lto(main_module, runtime_modules.into_iter());

    let obj_file = File::create("out.o").expect("Could not open output file for writing");
    unsafe {
        LLVMDumpModule(main_module);
        write_target_code(main_module, obj_file).unwrap();
        LLVMContextDispose(ctxt);
    }
}

#[derive(Debug)]
enum IRLoadError {
    IOError(std::io::Error),
    LLVMError(String)
}

impl From<std::io::Error> for IRLoadError {
    fn from(x: std::io::Error) -> IRLoadError {
        IRLoadError::IOError(x)
    }
}

fn module_from_blob(ctxt: LLVMContextRef, code: &[u8]) -> Result<LLVMModuleRef, IRLoadError> {
    use std::ffi::CStr;

    println!("module_from_blob {}", std::str::from_utf8(code).unwrap());
    unsafe {
        let mbuf = LLVMCreateMemoryBufferWithMemoryRange(code.as_ptr() as *const _,
                                                         code.len() as size_t,
                                                         b"\0".as_ptr() as *const _, 0);

        let mut module: LLVMModuleRef = mem::uninitialized();
        let mut err_msg: *mut i8 = mem::uninitialized();

        // ParseIRInContext takes ownership of the input memory buffer.
        // We should not dispose of it.
        let result = LLVMParseIRInContext(ctxt, mbuf, &mut module, &mut err_msg);

        if result != 0 {
            Err(IRLoadError::LLVMError(
                CStr::from_ptr(err_msg).to_string_lossy().into_owned()
            ))
        } else {
            Ok(module)
        }
    }
}

// This takes ownership of the modules
fn link_modules<I: IntoIterator<Item=LLVMModuleRef>>(main: LLVMModuleRef, iter: I) -> LLVMModuleRef {
    use llvm::linker::LLVMLinkModules;
    use llvm::linker::LLVMLinkerMode::*;

    for llmod in iter {
        unsafe {
            LLVMLinkModules(main, llmod, LLVMLinkerDestroySource, ptr::null_mut());
        }
    }
    main
}

fn optimize_lto<I: IntoIterator<Item=LLVMModuleRef>>(llmod: LLVMModuleRef, mods: I) -> LLVMModuleRef {
    use llvm::transforms::pass_manager_builder::*;

    let llmod = link_modules(llmod, mods);

    unsafe {
        let pm = LLVMCreatePassManager();
        let pmb = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderPopulateLTOPassManager(pmb, pm, /*internalize=*/1, /*runinliner=*/1);
        LLVMPassManagerBuilderDispose(pmb);

        LLVMRunPassManager(pm, llmod);
        LLVMDisposePassManager(pm);
    }
    llmod
}

fn optimize_module(llmod: LLVMModuleRef) -> LLVMModuleRef {
    use llvm::transforms::pass_manager_builder::*;

    unsafe {
        // Per clang and rustc, we want to use both kinds.
        let fpm = LLVMCreateFunctionPassManagerForModule(llmod);
        let mpm = LLVMCreatePassManager();

        // Populate the pass managers with passes
        let pmb = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderSetOptLevel(pmb, 2);
        LLVMPassManagerBuilderUseInlinerWithThreshold(pmb, 225);   // magic threshold from Clang for -O2
        LLVMPassManagerBuilderPopulateModulePassManager(pmb, mpm);
        LLVMPassManagerBuilderPopulateFunctionPassManager(pmb, fpm);
        LLVMPassManagerBuilderDispose(pmb);

        // Iterate over functions, running the FPM over each
        LLVMInitializeFunctionPassManager(fpm);
        let mut func = LLVMGetFirstFunction(llmod);
        while func != ptr::null_mut() {
            LLVMRunFunctionPassManager(fpm, func);
            func = LLVMGetNextFunction(func);
        }
        LLVMFinalizeFunctionPassManager(fpm);

        // Run the MPM over the module
        LLVMRunPassManager(mpm, llmod);

        // Clean up managers
        LLVMDisposePassManager(fpm);
        LLVMDisposePassManager(mpm);

        llmod
    }
}

fn write_target_code<W: std::io::Write>(llmod: LLVMModuleRef, mut w: W) -> std::io::Result<()> {
    use llvm::target_machine::*;
    use llvm::target_machine::LLVMCodeGenFileType::*;
    use llvm::target_machine::LLVMCodeGenOptLevel::*;
    use llvm::target_machine::LLVMRelocMode::*;
    use llvm::target_machine::LLVMCodeModel::*;

    unsafe {
        let triple = LLVMGetDefaultTargetTriple();
        let mut target = mem::uninitialized();
        LLVMGetTargetFromTriple(triple, &mut target, ptr::null_mut());
        let tm = LLVMCreateTargetMachine(target, triple,
                                         b"\0".as_ptr() as *const _, b"\0".as_ptr() as *const _,
                                         LLVMCodeGenLevelAggressive, LLVMRelocDefault,
                                         LLVMCodeModelDefault);

        let mut mbuf: LLVMMemoryBufferRef = mem::uninitialized();
        LLVMTargetMachineEmitToMemoryBuffer(tm, llmod, LLVMObjectFile, ptr::null_mut(), &mut mbuf);
        let out = w.write_all(membuf_as_slice(mbuf));

        LLVMDisposeTargetMachine(tm);
        out
    }
}

unsafe fn membuf_as_slice<'a>(mbuf: LLVMMemoryBufferRef) -> &'a [u8] {
    let p = LLVMGetBufferStart(mbuf);
    let len = LLVMGetBufferSize(mbuf);
    slice::from_raw_parts(p as *const _, len as usize)
}

// PHF mapping for target->runtime module files
include!(concat!(env!("OUT_DIR"), "/rt_map.rs"));
// Non-target-dependent files
static RT_SOURCES: &'static [&'static [u8]] = &[
    include_bytes!("../runtime/random.ll")
];

#[derive(Debug)]
enum RuntimeLoadError {
    InvalidIR(IRLoadError),
    NoSuchPlatform
}

impl From<IRLoadError> for RuntimeLoadError {
    fn from(x: IRLoadError) -> RuntimeLoadError {
        RuntimeLoadError::InvalidIR(x)
    }
}

fn load_runtime_for_target(ctxt: LLVMContextRef, target: &str, optimize: bool) -> Result<Vec<LLVMModuleRef>,
                                                                                         RuntimeLoadError> {
    let mut rt_modules = Vec::with_capacity(RT_SOURCES.len() + 1);
    
    for irmod in RT_SOURCES {
        let mut llmod = try!(module_from_blob(ctxt, irmod));
        if optimize {
            llmod = optimize_module(llmod)
        }
        rt_modules.push(llmod);
    }
    println!("Loaded target-independent runtime IR");

    let target_rt_source = match RT_TARGET_SOURCES.get(target) {
        Some(s) => *s,
        None => return Err(RuntimeLoadError::NoSuchPlatform)
    };
    let mut target_rt = try!(module_from_blob(ctxt, target_rt_source));
    if optimize {
        target_rt = optimize_module(target_rt);
    }

    rt_modules.push(target_rt);
    Ok(rt_modules)
}
