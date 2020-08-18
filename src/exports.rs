//! This file contains the functions exported to Excel. My recommendation is
//! that any non-trivial logic within these functions is implemented
//! elsewhere, to keep this module clean.
//! 
//! We implement xlAutoOpen here, because it needs to register our exported
//! functions. Other xlAuto methods are exported by xladd.

use xladd::variant::Variant;
use xladd::xlcall::LPXLOPER12;
use xladd::registrator::Reg;
use process::{start_ghci, raw_command, raw_read, raw_write, raw_peek, raw_return};

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
pub extern "stdcall" fn hkRawPeek() -> LPXLOPER12 {
    let result = raw_peek();
    let box_result = Box::new(Variant::from_str(&result));
    Box::into_raw(box_result) as LPXLOPER12
}

#[no_mangle]
pub extern "stdcall" fn xlAutoOpen() -> i32 {

    start_ghci();   // start the process hosting Haskell

    let r = Reg::new();
    r.add("hkVersion", "Q$", "", "Hasxcel", "Displays addin version number as text.", &[]);
    r.add("hkRaw", "QQ$", "Command", "Hasxcel", "Submits raw text into GHCI and returns the result", &[]);
    r.add("hkRawRead", "Q$", "", "Hasxcel", "Waits for and returns raw text from GHCI", &[]);
    r.add("hkRawWrite", "QQ$", "Command", "Hasxcel", "Submits raw text into GHCI and returns the result", &[]);
    r.add("hkRawReturn", "Q$", "", "Hasxcel", "Submits a raw carriage return into GHCI", &[]);
    r.add("hkRawPeek", "Q$", "", "Hasxcel", "If raw text is available from GHCI, returns it", &[]);

    1
}

