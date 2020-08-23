//! This file contains the functions exported to Excel. My recommendation is
//! that any non-trivial logic within these functions is implemented
//! elsewhere, to keep this module clean.
//! 
//! We implement xlAutoOpen here, because it needs to register our exported
//! functions. Other xlAuto methods are exported by xladd.

use xladd::variant::Variant;
use xladd::xlcall::{LPXLOPER12, xlfCaller};
use xladd::registrator::Reg;
use xladd::entrypoint::excel12;
use process::{start_ghci, ghci_version, raw_command, raw_read, 
    raw_error, raw_write, raw_wait_read, raw_return, logging};
use haskell::{assign, show};

/// Shows version string. Note that in the Excel function wizard, this shows
/// as requiring one unnamed parameter. This is a longstanding Excel bug.
#[no_mangle]
pub extern "stdcall" fn hxVersion() -> LPXLOPER12 {
    let result = Box::new(Variant::from_str("Haxcel: version 0.1.0"));
    Box::into_raw(result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxGHCIVersion() -> LPXLOPER12 {
    let result = Box::new(Variant::from_str(&ghci_version()));
    Box::into_raw(result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxRaw(xl_command: LPXLOPER12) -> LPXLOPER12 {
    let result;
    if let Some(command) = Variant::from_xloper(xl_command).as_string() {
        result = raw_command(&command);
    } else {
        result = "Error: command could not be interpreted as a string".to_string();
    }

    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxRawRead() -> LPXLOPER12 {
    let result = raw_read();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxRawError() -> LPXLOPER12 {
    let result = raw_error();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxRawWaitRead() -> LPXLOPER12 {
    let result = raw_wait_read();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxRawWrite(xl_command: LPXLOPER12) -> LPXLOPER12 {
    let result;
    if let Some(command) = Variant::from_xloper(xl_command).as_string() {
        result = raw_write(&command);
    } else {
        result = "Error: command could not be interpreted as a string".to_string();
    }

    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxRawReturn() -> LPXLOPER12 {
    let result = raw_return();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxLoggingOn() -> LPXLOPER12 {
    logging(true);
    let box_result = Box::new(Variant::from_str("Logging on"));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxLoggingOff() -> LPXLOPER12 {
    logging(false);
    let box_result = Box::new(Variant::from_str("Logging off"));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxAssign(
        xl_name: LPXLOPER12, 
        xl_expr: LPXLOPER12,
        xl_arg0: LPXLOPER12,
        xl_arg1: LPXLOPER12,
        xl_arg2: LPXLOPER12,
        xl_arg3: LPXLOPER12,
        xl_arg4: LPXLOPER12,
        xl_arg5: LPXLOPER12) -> LPXLOPER12 {
    let result;
    if let (Some(name), Some(expr)) 
            = (Variant::from_xloper(xl_name).as_string()
            ,  Variant::from_xloper(xl_expr).as_string()) {

        let expression = unpack_args(expr, &[xl_arg0, xl_arg1, xl_arg2, xl_arg3, xl_arg4, xl_arg5]);
        result = assign(&name, &expression);
        
    } else {
        result = "Error: args must be strings".to_string();
    }

    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hxShow(
        xl_expr: LPXLOPER12,
        xl_arg0: LPXLOPER12,
        xl_arg1: LPXLOPER12,
        xl_arg2: LPXLOPER12,
        xl_arg3: LPXLOPER12,
        xl_arg4: LPXLOPER12,
        xl_arg5: LPXLOPER12) -> LPXLOPER12 {
    let result;
    if let Some(expr) = Variant::from_xloper(xl_expr).as_string() {
        let expression = unpack_args(expr, &[xl_arg0, xl_arg1, xl_arg2, xl_arg3, xl_arg4, xl_arg5]);

        // find out the dimensions of the array formula (if any) that invoked us
        let caller = excel12(xlfCaller, &mut []);
        result = show(&expression, caller.dim());
    } else {
        result = Variant::from_str("Error: args must be strings");
    }

    let box_result = Box::new(result);
    Box::into_raw(box_result) as LPXLOPER12
}

fn unpack_args(expr : String, xl_args: &[LPXLOPER12]) -> String {
    let mut result = String::new();
    let mut arg = 0;
    for section in expr.split("{}") {
        result += section;
        if arg < xl_args.len() {
            let mut variant : Variant = Variant::from_xloper(xl_args[arg]);
            // horrible code until we push is_missing method into xladd
            if variant.as_mut_xloper().xltype & 0x0080 != 0x0080 {
                result += &variant.to_string();
            }
            arg += 1;
        }
    }
    return result;
}

#[no_mangle]
pub extern "stdcall" fn xlAutoOpen() -> i32 {

    // start the process hosting Haskell
    start_ghci();  

    // register all the functions we are exporting to Excel
    let r = Reg::new();
    r.add("hxVersion", "Q", "", "Haxcel", "Displays addin version number as text.", &[]);
    r.add("hxGHCIVersion", "Q", "", "Haxcel", "Displays GHCI version number as text.", &[]);
    r.add("hxRaw", "QQ", "Command", "Haxcel", "Submits raw text into GHCI and returns the result", &[]);
    r.add("hxRawRead", "Q", "", "Haxcel", "Returns any raw text that is ready from GHCI", &[]);
    r.add("hxRawError", "Q", "", "Haxcel", "Returns any raw error text that is ready from GHCI", &[]);
    r.add("hxRawWrite", "QQ", "Command", "Haxcel", "Submits raw text into GHCI and returns the result", &[]);
    r.add("hxRawReturn", "Q", "", "Haxcel", "Submits a raw carriage return into GHCI", &[]);
    r.add("hxRawWaitRead", "Q", "", "Haxcel", "Waits for then returns raw text from GHCI", &[]);
    r.add("hxLoggingOn", "Q", "", "Haxcel", "Turns on logging", &[]);
    r.add("hxLoggingOff", "Q", "", "Haxcel", "Turns off logging", &[]);
    r.add("hxAssign", "QQQQQQQQQ", "Name, Expression, Arg, Arg, Arg, Arg, Arg", "Haxcel", "Gets Haskell to assign the variable, then returns its name", &[]);
    r.add("hxShow", "QQQQQQQQ", "Expression, Arg, Arg, Arg, Arg, Arg", "Haxcel", "Shows in as many cells as are needed the result of a Haskell expression", &[]);

    1
}

