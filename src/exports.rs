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
use process::{start_ghci, raw_command, raw_read, raw_error, raw_write, raw_wait_read, raw_return, logging};
use haskell::{assign, show, display};

/// Shows version string. Note that in the Excel function wizard, this shows
/// as requiring one unnamed parameter. This is a longstanding Excel bug.
#[no_mangle]
pub extern "stdcall" fn hkVersion() -> LPXLOPER12 {
    let result = Box::new(Variant::from_str("Hasxcel: version 0.1.0"));
    Box::into_raw(result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkRaw(xl_command: LPXLOPER12) -> LPXLOPER12 {
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
pub extern "stdcall" fn hkRawRead() -> LPXLOPER12 {
    let result = raw_read();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkRawError() -> LPXLOPER12 {
    let result = raw_error();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkRawWaitRead() -> LPXLOPER12 {
    let result = raw_wait_read();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkRawWrite(xl_command: LPXLOPER12) -> LPXLOPER12 {
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
pub extern "stdcall" fn hkRawReturn() -> LPXLOPER12 {
    let result = raw_return();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkLoggingOn() -> LPXLOPER12 {
    logging(true);
    let box_result = Box::new(Variant::from_str("Logging on"));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkLoggingOff() -> LPXLOPER12 {
    logging(false);
    let box_result = Box::new(Variant::from_str("Logging off"));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkAssign(
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
pub extern "stdcall" fn hkDisplay(
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
        result = display(&expression);
    } else {
        result = "Error: args must be strings".to_string();
    }

    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn hkShow(
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

    start_ghci();   // start the process hosting Haskell

    let r = Reg::new();
    r.add("hkVersion", "Q$", "", "Hasxcel", "Displays addin version number as text.", &[]);
    r.add("hkRaw", "QQ$", "Command", "Hasxcel", "Submits raw text into GHCI and returns the result", &[]);
    r.add("hkRawRead", "Q$", "", "Hasxcel", "Returns any raw text that is ready from GHCI", &[]);
    r.add("hkRawError", "Q$", "", "Hasxcel", "Returns any raw error text that is ready from GHCI", &[]);
    r.add("hkRawWrite", "QQ$", "Command", "Hasxcel", "Submits raw text into GHCI and returns the result", &[]);
    r.add("hkRawReturn", "Q$", "", "Hasxcel", "Submits a raw carriage return into GHCI", &[]);
    r.add("hkRawWaitRead", "Q$", "", "Hasxcel", "Waits for then returns raw text from GHCI", &[]);
    r.add("hkLoggingOn", "Q$", "", "Hasxcel", "Turns on logging", &[]);
    r.add("hkLoggingOff", "Q$", "", "Hasxcel", "Turns off logging", &[]);
    r.add("hkAssign", "QQQQQQQQQ$", "Name, Expression, Arg, Arg, Arg, Arg, Arg", "Hasxcel", "Gets Haskell to assign the variable, then returns its name", &[]);
    r.add("hkDisplay", "QQQQQQQQ$", "Expression, Arg, Arg, Arg, Arg, Arg", "Hasxcel", "Displays in a single cell the result of a Haskell expression", &[]);
    r.add("hkShow", "QQQQQQQQ$", "Expression, Arg, Arg, Arg, Arg, Arg", "Hasxcel", "Shows in as many cells as are needed the result of a Haskell expression", &[]);

    1
}

