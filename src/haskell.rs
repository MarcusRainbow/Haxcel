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

/// Returns a variant that contains the value
pub fn show(value: &str, dim: (usize, usize)) -> Variant {

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
            log(&format!("incomplete result: {}", result));
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
    Variant::from_str(&execute_command(&format!("{}\n", var)))
}

fn show_list(var: &str, dim: (usize, usize)) -> Variant {
    let cols = if dim.0 > 1 {dim.0} else {dim.1};
    if cols == 0 {
        return Variant::from_str("Error: destination of formula has zero size")
    }
    let value = format!("take {} {}\n", cols, var);
    let list = execute_command(&value).trim().to_string();
    let trimmed = trim_brackets(&list);

    let result_strings: Vec<&str> = trimmed.split(',').collect();
    if result_strings.is_empty() {
        return Variant::missing()
    }

    let mut results = Vec::with_capacity(cols);
    for result in result_strings {
        results.push(Variant::from_str(result));
    }

    return Variant::from_array(dim.0, dim.1, &results)
}

fn show_list_of_lists(var: &str, dim: (usize, usize)) -> Variant {
    if dim.0 == 0 || dim.1 == 0 {
        return Variant::from_str("Error: destination of formula has zero size")
    }
    let value = format!("take {} (map (take {}) {})\n", dim.1, dim.0, var);
    let list = execute_command(&value).trim().to_string();

    let result_strings: Vec<&str> = list.split(',').collect();
    if result_strings.is_empty() {
        return Variant::missing()
    }

    let mut results = Vec::with_capacity(dim.0 * dim.1);
    for result in result_strings {
        results.push(Variant::from_str(trim_brackets(result)));
    }

    return Variant::from_array(dim.0, dim.1, &results)
}

fn trim_brackets(text: &str) -> &str {
    text.trim_start_matches('[').trim_end_matches(']')
}

pub fn execute_command(command: &str) -> String {
    if ! write_pipe(&command) {
        return error_message("Error: Cannot write to Haskell")
    }

    if let Some(result) = read_full_response() {
        return result
    } else {
        return error_message("Error: Cannot read from Haskell")
    }
}
