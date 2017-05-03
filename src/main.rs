extern crate docopt;
extern crate env_logger;
extern crate libc;
#[macro_use]
extern crate log;
extern crate llvm_sys as llvm;
extern crate phf;
extern crate rustc_serialize;
extern crate tempfile;

use docopt::Docopt;
use std::{mem, slice, ptr};
use std::fs::{File, OpenOptions};
use std::ffi::{CStr, CString};
use std::io::Read;
use std::process::Command;
use tempfile::NamedTempFile;

use llvm::core::*;
use llvm::ir_reader::LLVMParseIRInContext;
use llvm::target::{LLVM_InitializeNativeTarget, LLVM_InitializeNativeAsmPrinter,
                   LLVM_InitializeNativeAsmParser};
use llvm::target_machine::{LLVMCodeGenFileType, LLVMGetDefaultTargetTriple};
use llvm::target_machine::LLVMCodeGenFileType::*;
use llvm::LLVMLinkage;
use llvm::prelude::*;

extern "C" {
    /// Get the textual representation of a module's IR, passed by chunks to
    /// a callback for usage.
    ///
    /// Calls `cb` with a pointer to data, the length of that data and a pointer
    /// to some user-defined data (passed to the initial function as `cb_data`).
    fn PrintModuleIR(m: LLVMModuleRef,
                     cb: extern "C" fn(*const u8, libc::size_t, *mut libc::c_void) -> libc::c_int,
                     cb_data: *mut libc::c_void);
}

const USAGE: &'static str = "
Usage: merthc [options] [<source>]
       merthc --help

    If <source> is not specified, read source code from standard input.

Options:
    -h, --help      Show this message.
    --emit TYPE     Build output of TYPE. Valid values: asm, ir, obj, link.
                    Default: link.
    --no-opt        Disable optimization (default enabled).
    -o OUTFILE      Write output to OUTFILE.
    --no-rt         Do not link the runtime library. Default enabled,
                    incompatible with --emit link.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    flag_emit: Option<Emit>,
    flag_no_opt: Option<bool>,
    flag_no_rt: Option<bool>,
    arg_source: Option<String>,
    arg_outfile: Option<String>,
}

#[derive(PartialEq, Eq, Debug, RustcDecodable)]
enum Emit {
    Asm,
    Ir,
    Obj,
    Link,
}

fn main() {
    env_logger::init().unwrap();
    let Args { flag_no_opt: no_optimize,
               flag_no_rt: no_rt,
               flag_emit: emit,
               arg_source: inpath,
               arg_outfile: outpath } = Docopt::new(USAGE)
        .expect("USAGE string is invalid")
        .help(true)
        .argv(std::env::args())
        .decode()
        .unwrap_or_else(|e| e.exit());

    let emit = emit.unwrap_or(Emit::Link);
    let optimize = !no_optimize.unwrap_or(false);
    let use_runtime = !no_rt.unwrap_or(false);
    if emit == Emit::Link && !use_runtime {
        panic!("--no-rt and --emit link options may not be used together");
    };

    let outpath = if outpath.is_none() {
        // Select a default output filename
        let inpath_base: String = if emit == Emit::Link && inpath.is_none() {
            (if cfg!(target_os = "windows") {
                     "a.exe"
                 } else {
                     "a.out"
                 })
                .into()
        } else {
            inpath.clone().map_or("out".into(),
                                  |s| s[0..s.rfind('.').unwrap_or(s.len())].into())
        };
        let outpath_suffix = match emit {
            Emit::Asm => ".S",
            Emit::Ir => ".ll",
            Emit::Obj => ".o",
            Emit::Link => {
                if cfg!(target_os = "windows") {
                    ".exe"
                } else {
                    ""
                }
            }
        };
        inpath_base + outpath_suffix
    } else {
        outpath.unwrap()
    };

    let mut s = String::new();
    let mut infile: Box<Read> = match inpath {
        None => Box::new(std::io::stdin()),
        Some(p) => Box::new(File::open(p).expect("Failed to open input file")),
    };
    infile.read_to_string(&mut s).expect("Failed to read source code");

    let mut oo = OpenOptions::new();
    oo.create(true).write(true);
    if cfg!(unix) && emit == Emit::Link {
        // Make the file executable if linking on unix (let umask handle
        // any exclusions the user wants).
        use std::os::unix::fs::OpenOptionsExt;
        oo.mode(0o777);
    }
    let outfile = oo.open(outpath).expect("Failed to open output file for writing");

    do_compile(s.chars(), outfile, emit, optimize, use_runtime);
}

/// Do compilation of source, emitting the requested output format to `out`.
fn do_compile<I, W>(source: I, mut out: W, emit: Emit, optimize: bool, use_runtime: bool)
    where I: Iterator<Item = char>,
          W: std::io::Write
{
    unsafe {
        LLVM_InitializeNativeTarget();
        LLVM_InitializeNativeAsmPrinter();
        LLVM_InitializeNativeAsmParser();
    }
    let ctxt = unsafe { LLVMContextCreate() };

    let target = unsafe { CString::from_raw(LLVMGetDefaultTargetTriple()) };

    let runtime_modules = if use_runtime {
        // Load runtime files and maybe optimize them individually.
        Some(match load_runtime_for_target(ctxt, &target, optimize) {
                 Ok(x) => x,
                 Err(RuntimeLoadError::NoSuchPlatform) => {
            println!("Runtime for platform '{}' has no implementation. Sorry!",
                     target.to_string_lossy());
            return;
        }
                 e => panic!("Invalid IR while loading runtime: {:?}", e),
             })
    } else {
        None
    };

    // Build main()
    let (main_module, main_function) = unsafe {
        let ty_void = LLVMVoidTypeInContext(ctxt);
        let main_module = LLVMModuleCreateWithNameInContext(b"main\0".as_ptr() as *const _, ctxt);
        let ty_fn_main = LLVMFunctionType(ty_void, ptr::null_mut(), 0, 0);
        let main_function =
            LLVMAddFunction(main_module, b"main\0".as_ptr() as *const _, ty_fn_main);

        (main_module, main_function)
    };

    compile_merthese(ctxt, main_function, main_module, source);
    verify_module(main_module);

    if let Some(rt) = runtime_modules {
        for rtm in rt {
            link_modules(main_module, rtm);
        }
        // Do LTO if desired
        if optimize {
            optimize_lto(main_module, &[b"_start"]);
        }
    }

    match emit {
        Emit::Obj => write_target_code(&target, main_module, LLVMObjectFile, out)
            .expect("Failed to write object code"),
        Emit::Asm => write_target_code(&target, main_module, LLVMAssemblyFile, out)
            .expect("Failed to write assembly file"),
        Emit::Ir => {
            dump_module_ir(main_module, out);
        }
        Emit::Link => {
            let mut obj = NamedTempFile::new().expect("Failed to create temporary object file");
            let mut temp_bin =
                NamedTempFile::new().expect("Failed to create temporary binary file");

            // Kind of a mess. First write object code to a temporary file.
            write_target_code(&target, main_module, LLVMObjectFile, &mut obj)
                .expect("Failed to write object code to temporary file");

            // Then invoke the linker, writing to another temporary file.
            let res = Command::new("ld")
                .arg("-o")
                .arg(temp_bin.path())
                .arg(obj.path())
                .status()
                .expect("could not invoke ld for linking");
            // Permit removal of the object file. We don't need it anymore.
            let _ = obj;
            if !res.success() {
                panic!("linking failed: {}", res);
            }

            // Now move the temp binary to output.
            std::io::copy(&mut temp_bin, &mut out).expect("Unable to write output binary");
        }
    }

    unsafe {
        LLVMContextDispose(ctxt);
    }
}

/// Get a value which is a pointer to a static constant.
fn ptr_to_const(llmod: LLVMModuleRef,
                ty: LLVMTypeRef,
                value: LLVMValueRef,
                name: &[u8])
                -> LLVMValueRef {
    unsafe {
        let g = LLVMAddGlobal(llmod, ty, name.as_ptr() as *const _);
        LLVMSetInitializer(g, value);
        LLVMSetGlobalConstant(g, 1);
        LLVMConstInBoundsGEP(g,
                             [LLVMConstInt(LLVMIntTypeInContext(LLVMGetModuleContext(llmod),
                                                                8),
                                           0,
                                           0)]
                                     .as_ptr() as *mut _,
                             0)
    }
}

/// Compile a string of Merthese code in a new (assumed empty) function.
fn compile_merthese<I: Iterator<Item = char>>(ctxt: LLVMContextRef,
                                              llfn: LLVMValueRef,
                                              llmod: LLVMModuleRef,
                                              mut code: I) {
    unsafe {
        let bb_main = LLVMAppendBasicBlockInContext(ctxt, llfn, b"\0".as_ptr() as *const _);
        let b = LLVMCreateBuilderInContext(ctxt);
        LLVMPositionBuilderAtEnd(b, bb_main);

        // Types
        let ty_void = LLVMVoidTypeInContext(ctxt);
        let ty_i8 = LLVMIntTypeInContext(ctxt, 8);
        let ty_i8p = LLVMPointerType(ty_i8, 0);
        let ty_rt_rand_inrange = LLVMFunctionType(ty_i8, [ty_i8].as_ptr() as *mut _, 1, 0);
        let ty_rt_rand_string = LLVMFunctionType(ty_void, [ty_i8].as_ptr() as *mut _, 1, 0);
        let ty_rt_print = LLVMFunctionType(ty_void, [ty_i8p, ty_i8].as_ptr() as *mut _, 2, 0);
        // Runtime functions
        let rt_rand_inrange = LLVMAddFunction(llmod,
                                              b"rand_inrange\0".as_ptr() as *const _,
                                              ty_rt_rand_inrange);
        let rt_rand_string = LLVMAddFunction(llmod,
                                             b"rand_string\0".as_ptr() as *const _,
                                             ty_rt_rand_string);
        let rt_print = LLVMAddFunction(llmod, b"print\0".as_ptr() as *const _, ty_rt_print);

        // Constant ints
        let v_1i8 = LLVMConstInt(ty_i8, 1, 0);
        let v_5i8 = LLVMConstInt(ty_i8, 5, 0);
        // Parameter for print in 'm'
        let v_merth = LLVMBuildGlobalStringPtr(b,
                                               b"merth\0".as_ptr() as *const _,
                                               b"MERTH\0".as_ptr() as *const _);
        // Parameter for print in 'e'
        let v_newline = ptr_to_const(llmod, ty_i8, LLVMConstInt(ty_i8, 10, 0), b"NEWLINE\0");
        // Parameter for print in 'r'
        let v_space = ptr_to_const(llmod, ty_i8, LLVMConstInt(ty_i8, 32, 0), b"SPACE\0");

        while let Some(c) = code.next() {
            match c {
                'm' => {
                    // Print "merth"
                    LLVMBuildCall(b,
                                  rt_print,
                                  [v_merth, v_5i8].as_ptr() as *mut _,
                                  2,
                                  b"\0".as_ptr() as *const _);
                }
                'e' => {
                    // Print a newline
                    LLVMBuildCall(b,
                                  rt_print,
                                  [v_newline, v_1i8].as_ptr() as *mut _,
                                  2,
                                  b"\0".as_ptr() as *const _);
                }
                'r' => {
                    // Print a space
                    LLVMBuildCall(b,
                                  rt_print,
                                  [v_space, v_1i8].as_ptr() as *mut _,
                                  2,
                                  b"\0".as_ptr() as *const _);
                }
                't' => {
                    // Print a random string of length in range [0, 13.4]
                    let v_len = LLVMBuildCall(b, rt_rand_inrange,
                                              [LLVMConstAdd(
                                                  LLVMConstFPToUI(
                                                      LLVMConstReal(LLVMFloatTypeInContext(ctxt),
                                                                    13.4),
                                                      ty_i8
                                                  ),
                                                  v_1i8
                                              )].as_ptr() as *mut _,
                                              1, b"\0".as_ptr() as *const _);
                    LLVMBuildCall(b,
                                  rt_rand_string,
                                  [v_len].as_ptr() as *mut _,
                                  1,
                                  b"\0".as_ptr() as *const _);
                }
                'h' => {
                    // Skip characters through the next 'h', or to end of input.
                    loop {
                        match code.next() {
                            Some('h') | None => break,
                            _ => continue,
                        }
                    }
                }
                _ => { /* Ignore all other characters */ }
            }
        }

        LLVMBuildRetVoid(b);
        LLVMDisposeBuilder(b);
    }
}

type LLVMError = String;

/// Load a blob of textual IR and build a module.
fn module_from_blob(ctxt: LLVMContextRef, code: &[u8]) -> Result<LLVMModuleRef, LLVMError> {
    // ParseIRInContext seems to assume a null-terminated buffer, so sadly
    // we must reallocate.
    let mut code: Vec<u8> = code.into();
    code.push(0);

    unsafe {
        // Buffer length excludes the null terminator, confusingly.
        let mbuf = LLVMCreateMemoryBufferWithMemoryRange(code.as_ptr() as *const _,
                                                         code.len() - 1 as libc::size_t,
                                                         b"\0".as_ptr() as *const _,
                                                         1);

        let mut module: LLVMModuleRef = mem::uninitialized();
        let mut err_msg: *mut i8 = mem::uninitialized();

        // ParseIRInContext takes ownership of the input memory buffer.
        // We should not dispose of it.
        let result = LLVMParseIRInContext(ctxt, mbuf, &mut module, &mut err_msg);

        if result != 0 {
            Err(CStr::from_ptr(err_msg).to_string_lossy().into_owned())
        } else {
            verify_module(module);
            Ok(module)
        }
    }
}

/// Link together two modules.
///
/// Merges `other` into `main`, returning `main`.
fn link_modules(main: LLVMModuleRef, other: LLVMModuleRef) -> LLVMModuleRef {
    use llvm::linker::LLVMLinkModules2;
    unsafe {
        LLVMLinkModules2(main, other);
    }
    main
}

/// Perform optimization of `llmod` with link-time optimizations.
///
/// Symbol names present in `externals` will have their visibility unchanged;
/// all others will be marked as private, enabling better inlining and eliminating
/// any symbols that need not be present in the output.
fn optimize_lto(llmod: LLVMModuleRef, externals: &[&[u8]]) -> LLVMModuleRef {
    use llvm::transforms::pass_manager_builder::*;

    // Mark all functions except externals as private
    unsafe {
        let mut func = LLVMGetFirstFunction(llmod);
        while func != ptr::null_mut() {
            let func_name = CStr::from_ptr(LLVMGetValueName(func));
            if !externals.contains(&func_name.to_bytes()) {
                debug!("Marking function {} as private in LTO",
                       &func_name.to_string_lossy());
                LLVMSetLinkage(func, LLVMLinkage::LLVMPrivateLinkage);
            }

            func = LLVMGetNextFunction(func);
        }

        let mut glob = LLVMGetFirstGlobal(llmod);
        while glob != ptr::null_mut() {
            let glob_name = CStr::from_ptr(LLVMGetValueName(glob));
            if !externals.contains(&glob_name.to_bytes()) {
                debug!("Marking global {} as private in LTO",
                       &glob_name.to_string_lossy());
                LLVMSetLinkage(glob, LLVMLinkage::LLVMPrivateLinkage);
            }

            glob = LLVMGetNextGlobal(glob);
        }
    }

    unsafe {
        let pm = LLVMCreatePassManager();
        let pmb = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderPopulateLTOPassManager(pmb,
                                                     pm,
                                                     1,  // Internalize
                                                     1); // Run inliner
        LLVMPassManagerBuilderDispose(pmb);

        LLVMRunPassManager(pm, llmod);
        LLVMDisposePassManager(pm);
    }
    llmod
}

/// Run single-module optimizations over `llmod`.
fn optimize_module(llmod: LLVMModuleRef) -> LLVMModuleRef {
    use llvm::transforms::pass_manager_builder::*;

    unsafe {
        // Per clang and rustc, we want to use both kinds.
        let fpm = LLVMCreateFunctionPassManagerForModule(llmod);
        let mpm = LLVMCreatePassManager();

        // Populate the pass managers with passes
        let pmb = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderSetOptLevel(pmb, 2);
        // Magic threshold from Clang for -O2
        LLVMPassManagerBuilderUseInlinerWithThreshold(pmb, 225);
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

/// Write code for a module with the given target triple to a `io::Write`.
fn write_target_code<W: std::io::Write>(triple: &CStr,
                                        llmod: LLVMModuleRef,
                                        ty: LLVMCodeGenFileType,
                                        mut w: W)
                                        -> std::io::Result<()> {
    use llvm::target_machine::*;
    use llvm::target_machine::LLVMCodeGenOptLevel::*;
    use llvm::target_machine::LLVMRelocMode::*;
    use llvm::target_machine::LLVMCodeModel::*;

    let triple = triple.as_ptr();
    unsafe {
        let mut target = mem::uninitialized();
        LLVMGetTargetFromTriple(triple, &mut target, ptr::null_mut());
        let tm = LLVMCreateTargetMachine(target,
                                         triple,
                                         b"\0".as_ptr() as *const _,
                                         b"\0".as_ptr() as *const _,
                                         LLVMCodeGenLevelAggressive,
                                         LLVMRelocDefault,
                                         LLVMCodeModelDefault);

        let mut mbuf: LLVMMemoryBufferRef = mem::uninitialized();
        LLVMTargetMachineEmitToMemoryBuffer(tm, llmod, ty, ptr::null_mut(), &mut mbuf);
        let out = w.write_all(membuf_as_slice(&mbuf));

        LLVMDisposeMemoryBuffer(mbuf);
        LLVMDisposeTargetMachine(tm);
        out
    }
}

/// Get a slice over the contents of a LLVM memory buffer.
fn membuf_as_slice<'a>(mbuf: &'a LLVMMemoryBufferRef) -> &'a [u8] {
    unsafe {
        let p = LLVMGetBufferStart(*mbuf);
        let len = LLVMGetBufferSize(*mbuf);
        slice::from_raw_parts(p as *const _, len as usize)
    }
}

// PHF mapping for target->runtime module files
include!(concat!(env!("OUT_DIR"), "/rt_map.rs"));
// Non-target-dependent files
static RT_SOURCES: &'static [&'static [u8]] = &[include_bytes!("../runtime/random.ll")];

#[derive(Debug)]
enum RuntimeLoadError {
    InvalidIR(LLVMError),
    NoSuchPlatform,
}

impl From<LLVMError> for RuntimeLoadError {
    fn from(x: LLVMError) -> RuntimeLoadError {
        RuntimeLoadError::InvalidIR(x)
    }
}

/// Load the runtime library for a given target triple.
///
/// Optionally optimizes the modules individually.
fn load_runtime_for_target(ctxt: LLVMContextRef,
                           target: &CStr,
                           optimize: bool)
                           -> Result<Vec<LLVMModuleRef>, RuntimeLoadError> {
    let mut rt_modules = Vec::with_capacity(RT_SOURCES.len() + 1);

    for irmod in RT_SOURCES {
        debug!("Loading a module from RT_SOURCES");
        let mut llmod = try!(module_from_blob(ctxt, irmod));
        if optimize {
            llmod = optimize_module(llmod)
        }
        rt_modules.push(llmod);
    }
    debug!("Loaded all from RT_SOURCES");

    debug!("Loading RT_TARGET_SOURCES for {}",
           &*target.to_string_lossy());
    let target_rt_source = match RT_TARGET_SOURCES.get(&*target.to_string_lossy()) {
        Some(s) => *s,
        None => return Err(RuntimeLoadError::NoSuchPlatform),
    };
    let mut target_rt = try!(module_from_blob(ctxt, target_rt_source));
    if optimize {
        target_rt = optimize_module(target_rt);
    }

    rt_modules.push(target_rt);
    Ok(rt_modules)
}

fn dump_module_ir<W: std::io::Write>(module: LLVMModuleRef, mut out: W) {
    extern "C" fn module_ir_printer<W: std::io::Write>(src: *const u8,
                                                       size: libc::size_t,
                                                       state: *mut libc::c_void)
                                                       -> libc::c_int {
        let (src, out) = unsafe { (slice::from_raw_parts(src, size), &mut *(state as *mut W)) };
        // Cannot unwrap here; panic must not cross FFI boundary.
        let _res = out.write_all(src);
        0
    }

    unsafe {
        PrintModuleIR(module,
                      module_ir_printer::<W>,
                      &mut out as *mut W as *mut libc::c_void);
    }
}

fn verify_module(llmod: LLVMModuleRef) {
    if cfg!(debug_assertions) {
        unsafe {
            use llvm::analysis::{LLVMVerifyModule, LLVMVerifierFailureAction};
            LLVMVerifyModule(llmod,
                             LLVMVerifierFailureAction::LLVMPrintMessageAction,
                             ptr::null_mut());
        }
    }
}
