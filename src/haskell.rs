use xladd::variant::Variant;
use process::{write_pipe, error_message, read_full_response, log};

pub fn assign(name: &str, value: &str) -> String {
    let command = format!("{} = {}\n", name, value);
    if ! write_pipe(&command) {
        return error_message("Error: Cannot write to Haskell")
    }

    if let Some(result) = read_full_response() {
        if result.is_empty() {
            // this is what we expect. Successful assignment does not output anything
            return name.to_string()
        } else {
            return result   // error message or whatever -- just send it back to the user
        }
    } else {
        return error_message("Error: Cannot read from Haskell")
    }
}

pub fn show(value: &str, dim: (usize, usize)) -> Variant {

    log(&format!("show: dim={:?}", dim));

    // if this is not an array formula or a single cell, treat the same as display
    if dim.0 == 0 || dim.1 == 0 {
        return Variant::from_str(&display(value))
    }

    // we first assign the result of the expression to a temp variable
    // (maybe be a bit cleverer about the name, but it makes sense to
    // reuse the same name, so results get garbage collected).
    let temp = "hk_temp";
    let command = format!("{} = {}\n", temp, value);
    if ! write_pipe(&command) {
        return Variant::from_str(&error_message("Error: Cannot write to Haskell"))
    }
    if let Some(result) = read_full_response() {
        if ! result.is_empty() {
            // the result should be empty, but if it's not, return it
            log(&format!("imcomplete result: {}", result));
            return Variant::from_str(&result)
        }
    } else {
        return Variant::from_str(&error_message("Error: Cannot read from Haskell"))
    }

    // now take a peek at the type of the result
    if ! write_pipe(":t hk_temp\n") {
        return Variant::from_str("Error: Cannot ask Haskell the type")
    }
    let result_type;
    if let Some(result) = read_full_response() {
        if result.is_empty() {
            return Variant::from_str("Error: no type response from Haskell")
        } else {
            result_type = result.trim().to_string();
        }
    } else {
        return Variant::from_str(&error_message("Error: Cannot read from Haskell"))
    }

    // The results here might be something like "hk_temp :: (Num a, Enum a) => [a]"
    // or hk_temp :: [Integer]. We can tell whether this is a list or list of lists
    // by popping ] characters off the end.
    log(&format!("type string = '{}'", result_type));
    let mut type_iter = result_type.chars().rev();
    if type_iter.next().unwrap() == ']' {
        if type_iter.next().unwrap() == ']' {
            show_list_of_lists(temp, dim)
        } else {
            show_list(temp, dim)
        }
    } else {
        show_single(temp)
    }
}

fn show_single(var: &str) -> Variant {
    Variant::from_str(&display(var))
}

fn show_list(var: &str, dim: (usize, usize)) -> Variant {
    let cols = if dim.0 > 1 {dim.0} else {dim.1};
    if cols == 0 {
        return Variant::from_str("Error: cannot show empty list")
    }

    let value = format!("take {} {}", cols, var);
    let mut list = display(&value).trim().to_string();

    // remove first and last characters, which should be []
    let last = list.pop();
    let first = list.remove(0);
    if last != Some(']') || first != '[' {
        return Variant::from_str("Error: list is not surrounded by []")
    }

    let result_strings: Vec<&str> = list.split(',').collect();
    if result_strings.is_empty() {
        return Variant::missing()
    }

    let mut results = Vec::with_capacity(cols);
    for result in result_strings {
        results.push(Variant::from_str(result));
    }

    // for now, just return the first element of the list
    return Variant::from_array(dim.0, dim.1, &results)
}

fn show_list_of_lists(_var: &str, dim: (usize, usize)) -> Variant {
    let (cols, rows) = dim;
    if cols == 0 || rows == 0 {
        return Variant::from_str("Error: cannot show empty list")
    }
    return Variant::from_str("show_list_of_lists: TODO")
}

pub fn display(value: &str) -> String {
    let command = format!("{}\n", value);
    if ! write_pipe(&command) {
        return error_message("Error: Cannot write to Haskell")
    }

    if let Some(result) = read_full_response() {
        return result
    } else {
        return error_message("Error: Cannot read from Haskell")
    }
}
