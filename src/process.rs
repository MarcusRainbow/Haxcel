use winapi::um::fileapi::{WriteFile, ReadFile};
use winapi::shared::minwindef::{BOOL, DWORD, LPCVOID, LPDWORD, LPVOID};
use winapi::um::processthreadsapi::{CreateProcessA, STARTUPINFOA, PROCESS_INFORMATION};
use winapi::um::handleapi::{CloseHandle, SetHandleInformation};
use winapi::um::namedpipeapi::{CreatePipe, PeekNamedPipe};
use winapi::um::wincon::GenerateConsoleCtrlEvent;
// use winapi::um::winbase::handle_flag_inherit;
use winapi::um::winbase::STARTF_USESTDHANDLES;
use winapi::um::minwinbase::{SECURITY_ATTRIBUTES, LPOVERLAPPED};
use winapi::shared::ntdef::{NULL, TRUE};
use winapi::um::debugapi::OutputDebugStringA;
use winapi::um::errhandlingapi::GetLastError;
use winapi::um::winnt::HANDLE;
use std::ffi::CString;
use std::{mem, ptr, thread, time};

static mut GHCI_STDIN : HANDLE = NULL;
static mut GHCI_STDOUT : HANDLE = NULL;
static mut GHCI_STDERR : HANDLE = NULL;
static mut GHCI_INITIAL_RESPONSE : Option<String> = None;
static mut LOGGING : bool = false;

pub fn start_ghci() {

    unsafe {

        // Rather remarkable that the Windows TRUE definition is not of type BOOL
        let ok = TRUE as BOOL;

        // Set the bInheritHandle flag so pipe handles are inherited. 
        let mut sa_attr = SECURITY_ATTRIBUTES {
            nLength : mem::size_of::<SECURITY_ATTRIBUTES>() as DWORD,
            lpSecurityDescriptor : NULL,
            bInheritHandle: ok
        };

        let mut child_stdin : HANDLE = NULL;
        let mut child_stdout : HANDLE = NULL;
        let mut child_stderr : HANDLE = NULL;

        // Create a pipe for the child process's STDIN. 
        if CreatePipe(&mut child_stdin, &mut GHCI_STDIN, &mut sa_attr, 0) != ok  {
            error_exit("Stdin CreatePipe")
        }

        // Ensure the write handle to the pipe for STDIN is not inherited. 
        let handle_flag_inherit = 1 as DWORD;   // cannot find the real definition of this
        if SetHandleInformation(GHCI_STDIN, handle_flag_inherit, 0) != ok {
            error_exit("Stdin SetHandleInformation")
        }

        // Create a pipe for the child process's STDOUT. 
        if CreatePipe(&mut GHCI_STDOUT, &mut child_stdout, &mut sa_attr, 0) != ok { 
            error_exit("StdoutRd CreatePipe")
        }

        // Ensure the read handle to the pipe for STDOUT is not inherited.
        if SetHandleInformation(GHCI_STDOUT, handle_flag_inherit, 0) != ok {
            error_exit("Stdout SetHandleInformation") 
        }

        // Create a pipe for the child process's STDERR. 
        if CreatePipe(&mut GHCI_STDERR, &mut child_stderr, &mut sa_attr, 0) != ok { 
            error_exit("StderrRd CreatePipe")
        }

        // Ensure the read handle to the pipe for STDERR is not inherited.
        if SetHandleInformation(GHCI_STDERR, handle_flag_inherit, 0) != ok {
            error_exit("Stderr SetHandleInformation") 
        }
    
        let reserved: *mut i8 = ptr::null_mut();
        let ureserved: *mut u8 = ptr::null_mut();

        // Set up the startup info to specify the three handles for the child to inherit
        let mut startup_info = STARTUPINFOA {
            cb : mem::size_of::<STARTUPINFOA>() as DWORD,
            lpReserved : reserved,
            lpDesktop : reserved,
            lpTitle: reserved,
            dwX: 0,
            dwY: 0,
            dwXSize: 0,
            dwYSize: 0,
            dwXCountChars: 0,
            dwYCountChars: 0,
            dwFillAttribute: 0,
            dwFlags: STARTF_USESTDHANDLES,
            wShowWindow: 0,
            cbReserved2: 0,
            lpReserved2: ureserved,
            hStdInput : child_stdin,
            hStdOutput : child_stdout,
            hStdError : child_stderr
        };

        let mut process_info = PROCESS_INFORMATION {
            hProcess : NULL,
            hThread : NULL,
            dwProcessId : 0,
            dwThreadId : 0
        };
        
        let no_str: *mut i8 = ptr::null_mut();
        let no_sec: *mut SECURITY_ATTRIBUTES = ptr::null_mut();

        // CString is good at giving us a C-style string, but it must be
        // const. Unfortunately we must therefore painstakingly copy its buffer.
        let ghci = CString::new("GHCI").unwrap();
        let bytes = ghci.as_bytes();
        let mut buffer = Vec::<i8>::with_capacity(bytes.len() + 1);
        for i in bytes {
            buffer.push((*i) as i8);
        }
        buffer.push(0);

        if CreateProcessA(
                no_str,                    // lpApplicationName: LPCSTR, 
                buffer.as_mut_ptr(),       // lpCommandLine: LPSTR, 
                no_sec,                    // lpProcessAttributes: LPSECURITY_ATTRIBUTES, 
                no_sec,                    // lpThreadAttributes: LPSECURITY_ATTRIBUTES, 
                TRUE as BOOL,              // bInheritHandles: BOOL, 
                0x08000000,                // dwCreationFlags: DWORD, 
                NULL,                      // lpEnvironment: LPVOID, 
                no_str,                    // lpCurrentDirectory: LPCSTR, 
                &mut startup_info,         // lpstartup_info: LPstartup_infoA, 
                &mut process_info) != ok { // lpprocess_information: LPPROCESS_INFORMATION)
            error_exit("Failed to create child process")
        }

        CloseHandle(process_info.hProcess);
        CloseHandle(process_info.hThread);
        CloseHandle(child_stdin);
        CloseHandle(child_stdout);
        CloseHandle(child_stderr);

        // pull as much as we can from GHCI. This should be just version info
        // that GHCI outputs at startup.
        GHCI_INITIAL_RESPONSE = read_full_response();
    }
}

pub fn ghci_version() -> String {
    let ver = unsafe { GHCI_INITIAL_RESPONSE.clone() };
    ver.unwrap_or_default()
}

fn stdout_pipe() -> HANDLE {
    unsafe { GHCI_STDOUT }
}

fn stderr_pipe() -> HANDLE {
    unsafe { GHCI_STDERR }
}

fn stdin_pipe() -> HANDLE {
    unsafe { GHCI_STDIN }
}

fn error_exit(message: &str) {
    let cstr = CString::new(error_message(message)).unwrap();
    unsafe { OutputDebugStringA(cstr.as_ptr()) };
}

pub fn logging(on: bool) {
    unsafe { LOGGING = on };
    if on {
        log("Turning logging on");
    } else {
        log("Turning logging off")
    }
}

pub fn log(message: &str) {
    if unsafe { LOGGING } {
        always_log(message);
    }
}

fn always_log(message: &str) {
    let cstr = CString::new(message).unwrap();
    unsafe { OutputDebugStringA(cstr.as_ptr()) };
}

pub fn error_message(message: &str) -> String {
    let last_error = unsafe { GetLastError() };
    format!("{}: {}", message, last_error)
}

pub fn raw_command(command: &str) -> String {

    // read whatever was previously in the GHCI stdout pipe
    if let Some(prev) = read_pipe_nonblocking(stdout_pipe()) {
        log(&prev);
    } else {
        return error_message("Error: Cannot read from GHCI pipe")
    }

    // write the command into the GHCI stdin pipe, followed by a carriage return
    if ! write_pipe(command) || ! write_pipe("\n") {
        return error_message("Error: Cannot write to GHCI pipe")
    }

    if let Some(result) = read_full_response() {
        return result
    } else {
        return error_message("Error: Cannot read from GHCI pipe")
    }
}

pub fn read_full_response() -> Option<String> {
    // wait a short time for GHCI to respond, then longer periods
    let short_wait = time::Duration::from_millis(10);
    let long_wait = time::Duration::from_millis(200);
    let mut timeout = 10; // times long_wait
    thread::sleep(short_wait);

    let mut result = String::new();
    loop {
        if let Some(response) = read_pipe_nonblocking(stdout_pipe()) {
            log(&format!("read: {}", response));

            if response.ends_with("> ") {
                if let Some(pos) = response.rfind("\n") {
                    // if there is a response followed by a prompt, return the response
                    result += &response[..pos].to_string()
                }
                // we have seen a prompt, so ok to exit the loop
                break;
            } else if timeout > 0 {
                // if there is no prompt, then keep waiting
                result += &response;
                thread::sleep(long_wait);
                timeout -= 1;
            } else {
                // timed out. We don't want to just return anyway. We need to get
                // the GHCI process back to its command prompt somehow.
                // TODO: we cannot simply send a SIGINT like this. See https://social.msdn.microsoft.com/Forums/en-US/dc9586ab-1ee8-41aa-a775-cf4828ac1239/how-to-send-ctrlc-signal-to-detached-commandline-process?forum=windowsgeneraldevelopmentissues
                if unsafe { GenerateConsoleCtrlEvent(0, 0) } == 0 {
                    always_log(&error_message("Error: Unable to send Ctrl+C after timeout"));
                    return None
                }
                timeout = 10;
            }
        }
    }

    // We have seen the command prompt. Check whether there was
    // anything in stderr, and if so add it to whatever was in stdout
    // first see if there is anything in stderr
    if let Some(err_response) = read_pipe_nonblocking(stderr_pipe()) {
        log(&format!("stderr: {}", err_response));
        result += &err_response;
    }

    return Some(result)
}

pub fn raw_write(message: &str) -> String {
    // write the command into the GHCI stdin pipe
    if ! write_pipe(message) {
        return error_message("Error: Cannot write to GHCI pipe")
    }
    return "OK".to_string()
}

pub fn raw_return() -> String {
    // write the command into the GHCI stdin pipe
    if ! write_pipe("\n") {
        return error_message("Error: Cannot write to GHCI pipe")
    }
    return "OK".to_string()
}

pub fn raw_wait_read() -> String {
    if let Some(response) = read_pipe(stdout_pipe()) {
        log(&format!("wait then read: {}", response));
        return response;
    } else {
        return error_message("Error: Cannot read from GHCI pipe")
    }
}

pub fn raw_read() -> String {
    if let Some(response) = read_pipe_nonblocking(stdout_pipe()) {
        log(&format!("read: {}", response));
        return response;
    } else {
        return error_message("Error: Cannot read from GHCI pipe")
    }
}

pub fn raw_error() -> String {
    if let Some(response) = read_pipe_nonblocking(stderr_pipe()) {
        log(&format!("error: {}", response));
        return response;
    } else {
        return error_message("Error: Cannot read from GHCI stderr pipe")
    }
}

pub fn write_pipe(message: &str) -> bool {
    log(&format!("write: {}", message));
    let buffer = CString::new(message).unwrap();
    let raw_buffer = buffer.as_bytes();
    return unsafe { WriteFile(
            stdin_pipe(), 
            raw_buffer.as_ptr() as LPCVOID, 
            raw_buffer.len() as DWORD,
            NULL as LPDWORD,
            NULL as LPOVERLAPPED) == TRUE as BOOL }
}

fn read_pipe(pipe: HANDLE) -> Option<String> {
    let buffer_max = 1000;
    let mut read_buffer = Vec::<u8>::new();
    let mut bytes_read : DWORD = 0;
    read_buffer.resize(buffer_max, 0); 
    if unsafe { ReadFile(
            pipe, 
            read_buffer.as_mut_ptr() as LPVOID, 
            buffer_max as DWORD,
            &mut bytes_read,
            NULL as LPOVERLAPPED) != TRUE as BOOL } {
        return None;
    }

    let cstr_result = unsafe { CString::from_vec_unchecked(read_buffer) };
    return Some(cstr_result.into_string().unwrap());
}

fn read_pipe_nonblocking(pipe: HANDLE) -> Option<String> {

    // do nothing if there is nothing in the pipe. Just return None.
    let mut bytes_avail : DWORD = 0;
    if unsafe { PeekNamedPipe(
            pipe, 
            NULL, 
            0,
            NULL as LPDWORD,
            &mut bytes_avail,
            NULL as LPDWORD) != TRUE as BOOL } {
        always_log(&error_message("Error: PeekNamedPipe failed"));
        return None;
    }
    
    if bytes_avail == 0 {
        return Some("".to_string());
    }

    let buffer_max = bytes_avail as usize;
    let mut read_buffer = Vec::<u8>::new();
    let mut bytes_read : DWORD = 0;
    read_buffer.resize(buffer_max, 0); 
    if unsafe { ReadFile(
            pipe, 
            read_buffer.as_mut_ptr() as LPVOID, 
            buffer_max as DWORD,
            &mut bytes_read,
            NULL as LPOVERLAPPED) != TRUE as BOOL } {
        always_log(&error_message("Error: ReadFile failed"));
        return None;
    }

    let cstr_result = unsafe { CString::from_vec_unchecked(read_buffer) };
    return Some(cstr_result.into_string().unwrap());
}
