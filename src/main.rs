extern crate libc;
extern crate llvm_sys as llvm;
extern crate phf;

use std::{mem, slice, ptr};
use std::fs::File;
use std::ffi::{CStr, CString};

use libc::size_t;

use llvm::core::*;
use llvm::ir_reader::LLVMParseIRInContext;
use llvm::target::{
    LLVM_InitializeNativeTarget,
    LLVM_InitializeNativeAsmPrinter,
    LLVM_InitializeNativeAsmParser
};
use llvm::target_machine::LLVMGetDefaultTargetTriple;
use llvm::LLVMLinkage;
use llvm::prelude::*;

type LLVMError = String;

fn main() {
    unsafe {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();
    }
    let ctxt = unsafe { LLVMContextCreate() };

    // Load and optimize the runtime files.
    let target = unsafe {
        CString::from_raw(LLVMGetDefaultTargetTriple())
    };
    let optimize = false;
    let runtime_modules = load_runtime_for_target(ctxt, &target, optimize).expect("Failed to load runtime");

    // Build main()
    let (main_module, main_function) = unsafe {
        let ty_void = LLVMVoidType();
        let main_module = LLVMModuleCreateWithNameInContext(b"main\0".as_ptr() as *const _, ctxt);
        let ty_fn_main = LLVMFunctionType(ty_void, ptr::null_mut(), 0, 0);
        let main_function = LLVMAddFunction(main_module, b"main\0".as_ptr() as *const _, ty_fn_main);

        (main_module, main_function)
    };
    compile_merthese(ctxt, main_function, main_module, "trte".chars());

    // Combine main() and runtime
    link_modules(main_module, runtime_modules);
    // Do LTO if desired
    if optimize {
        optimize_lto(main_module, &[b"_start"]);
    }

    let obj_file = File::create("out.o").expect("Could not open output file for writing");
    unsafe {
        LLVMDumpModule(main_module);
        write_target_code(&target, main_module, obj_file).unwrap();
        LLVMContextDispose(ctxt);
    }
}

fn ptr_to_const(llmod: LLVMModuleRef, ty: LLVMTypeRef, value: LLVMValueRef, name: &[u8]) -> LLVMValueRef {
    unsafe {
        let g = LLVMAddGlobal(llmod, ty, name.as_ptr() as *const _);
        LLVMSetInitializer(g, value);
        LLVMSetGlobalConstant(g, 1);
        LLVMConstInBoundsGEP(g, [LLVMConstInt(LLVMIntType(8), 0, 0)].as_ptr() as *mut _, 0)
    }
}

fn compile_merthese<I: Iterator<Item=char>>(ctxt: LLVMContextRef, llfn: LLVMValueRef,
                                            llmod: LLVMModuleRef, mut code: I) {
    unsafe {
        let bb_main = LLVMAppendBasicBlockInContext(ctxt, llfn, b"\0".as_ptr() as *const _);
        let b = LLVMCreateBuilderInContext(ctxt);
        LLVMPositionBuilderAtEnd(b, bb_main);

        // Types
        let ty_void = LLVMVoidType();
        let ty_i8 = LLVMIntType(8);
        let ty_i8p = LLVMPointerType(ty_i8, 0);
        let ty_rt_rand_inrange = LLVMFunctionType(ty_i8, [ty_i8].as_ptr() as *mut _, 1, 0);
        let ty_rt_rand_string = LLVMFunctionType(ty_void, [ty_i8].as_ptr() as *mut _, 1, 0);
        let ty_rt_print = LLVMFunctionType(ty_void, [ty_i8p, ty_i8].as_ptr() as *mut _, 2, 0);
        // Runtime functions
        let rt_rand_inrange = LLVMAddGlobal(llmod, ty_rt_rand_inrange, b"rand_inrange\0".as_ptr() as *const _);
        let rt_rand_string = LLVMAddGlobal(llmod, ty_rt_rand_string, b"rand_string\0".as_ptr() as *const _);
        let rt_print = LLVMAddGlobal(llmod, ty_rt_print, b"print\0".as_ptr() as *const _);

        // Constant ints
        let v_0i8 = LLVMConstInt(ty_i8, 0, 0);
        let v_1i8 = LLVMConstInt(ty_i8, 1, 0);
        let v_5i8 = LLVMConstInt(ty_i8, 5, 0);
        let v_10i8 = LLVMConstInt(ty_i8, 10, 0);
        // Parameter for print in 'm'
        let v_merth = LLVMBuildGlobalStringPtr(b, b"merth\0".as_ptr() as *const _,
                                               b"MERTH\0".as_ptr() as *const _);
        // Parameter for print in 'e'
        let v_newline = ptr_to_const(llmod, ty_i8, LLVMConstInt(ty_i8, 10, 0), b"NEWLINE\0");
        // Parameter for print in 'r'
        let v_space = ptr_to_const(llmod, ty_i8, LLVMConstInt(ty_i8, 32, 0), b"SPACE\0");

        while let Some(c) = code.next() {
            match c {
                'm' => {
                    LLVMBuildCall(b, rt_print, [v_merth, v_5i8].as_ptr() as *mut _,
                                  2, b"\0".as_ptr() as *const _);
                }
                'e' => {
                    LLVMBuildCall(b, rt_print, [v_newline, v_1i8].as_ptr() as *mut _,
                                  2, b"\0".as_ptr() as *const _);
                }
                'r' => {
                    LLVMBuildCall(b, rt_print, [v_space, v_1i8].as_ptr() as *mut _,
                                  2, b"\0".as_ptr() as *const _);
                }
                't' => {
                    let v_len = LLVMBuildCall(b, rt_rand_inrange,
                                              [LLVMConstAdd(
                                                  LLVMConstFPToUI(
                                                      LLVMConstReal(LLVMFloatType(), 13.4), ty_i8
                                                  ),
                                                  v_1i8
                                              )].as_ptr() as *mut _,
                                              1, b"\0".as_ptr() as *const _);
                    LLVMBuildCall(b, rt_rand_string, [v_len].as_ptr() as *mut _,
                                  1, b"\0".as_ptr() as *const _);
                }
                'h' => {
                    loop {
                        match code.next() {
                            Some('h') | None => break,
                            _ => continue
                        }
                    }
                }
                _ => unimplemented!()
            }
        }

        LLVMBuildRetVoid(b);
    }
}

fn module_from_blob(ctxt: LLVMContextRef, code: &[u8]) -> Result<LLVMModuleRef, LLVMError> {
    // ParseIRInContext seems to assume a null-terminated buffer, so sadly
    // we must reallocate.
    let mut code: Vec<u8> = code.into();
    code.push(0);

    unsafe {
        let mbuf = LLVMCreateMemoryBufferWithMemoryRange(code.as_ptr() as *const _,
                                                         code.len() as size_t,
                                                         b"\0".as_ptr() as *const _, 1);

        let mut module: LLVMModuleRef = mem::uninitialized();
        let mut err_msg: *mut i8 = mem::uninitialized();

        // ParseIRInContext takes ownership of the input memory buffer.
        // We should not dispose of it.
        let result = LLVMParseIRInContext(ctxt, mbuf, &mut module, &mut err_msg);

        if result != 0 {
            Err(
                CStr::from_ptr(err_msg).to_string_lossy().into_owned()
            )
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

fn optimize_lto(llmod: LLVMModuleRef, externals: &[&[u8]]) -> LLVMModuleRef {
    use llvm::transforms::pass_manager_builder::*;

    // Mark all functions except externals as private
    unsafe {
        let mut func = LLVMGetFirstFunction(llmod);
        while func != ptr::null_mut() {
            let func_name = CStr::from_ptr(LLVMGetValueName(func));
            if !externals.contains(&func_name.to_bytes()) {
                LLVMSetLinkage(func, LLVMLinkage::LLVMPrivateLinkage);
            }

            func = LLVMGetNextFunction(func);
        }
    }

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

fn write_target_code<W: std::io::Write>(triple: &CStr, llmod: LLVMModuleRef, mut w: W) -> std::io::Result<()> {
    use llvm::target_machine::*;
    use llvm::target_machine::LLVMCodeGenFileType::*;
    use llvm::target_machine::LLVMCodeGenOptLevel::*;
    use llvm::target_machine::LLVMRelocMode::*;
    use llvm::target_machine::LLVMCodeModel::*;

    let triple = triple.as_ptr();
    unsafe {
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
    InvalidIR(LLVMError),
    NoSuchPlatform
}

impl From<LLVMError> for RuntimeLoadError {
    fn from(x: LLVMError) -> RuntimeLoadError {
        RuntimeLoadError::InvalidIR(x)
    }
}

fn load_runtime_for_target(ctxt: LLVMContextRef, target: &CStr, optimize: bool)
        -> Result<Vec<LLVMModuleRef>, RuntimeLoadError> {
    let mut rt_modules = Vec::with_capacity(RT_SOURCES.len() + 1);
    
    for irmod in RT_SOURCES {
        let mut llmod = try!(module_from_blob(ctxt, irmod));
        if optimize {
            llmod = optimize_module(llmod)
        }
        rt_modules.push(llmod);
    }

    let target_rt_source = match RT_TARGET_SOURCES.get(&*target.to_string_lossy()) {
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
