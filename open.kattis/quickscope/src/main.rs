use std::collections::HashMap;
use std::io::{self, BufRead, BufReader};
use std::fmt;

const MAX_LINE_SIZE: u16 = 7 + 1 + 6 + 1 + 6 + 1; // DECLARE + ws + <decl> + ws + <identifier> + \n
const C_STR_SIZE: usize = 6;

fn main() -> io::Result<()>
{
  let nr_of_lines: usize = read_line_as_int() as usize;
  let lines: Vec<Line> = read_nr_of_lines(nr_of_lines);

  let mut variable_declarations: Vec<HashMap<Cstr, Cstr>> = vec![HashMap::new(); nr_of_lines];

  let mut variable_declaration_indices: HashMap<Cstr, Vec<u32>> = HashMap::with_capacity(nr_of_lines);
  let mut current_index: u32 = 0;

  let mut output: String = String::with_capacity(MAX_LINE_SIZE as usize * nr_of_lines);

  // Loop over read lines.
  for line in lines
  {
    match line
    {
      Line::OPENBL =>
      {
        current_index += 1;
        variable_declarations[current_index as usize].clear()
      }

      Line::CLOSEBL =>
      {
        for (key, _) in &variable_declarations[current_index as usize]
        {
          match variable_declaration_indices.get_mut(&key)
          {
            Some(indices) =>
            {
              if !indices.is_empty() && *indices.last().unwrap() == current_index
              {
                indices.pop();
              }
            },
            None => (),
          }
        }
        current_index -= 1;
      }

      Line::TYPEOF(key) =>
      {
        match variable_declaration_indices.get(&key)
        {
          Some(indices) =>
          {
            if indices.is_empty()
            {
              output.push_str("UNDECLARED");
              output.push('\n');
            }
            else
            {
              let index: usize = *indices.last().unwrap() as usize;
              let map = variable_declarations.get(index).unwrap();
              for c in map.get(&key).unwrap().str
              {
                output.push(c);
              }
              output.push('\n');
            }
          }
          None =>
          {
            output.push_str("UNDECLARED");
            output.push('\n');
          }
        }
      }

      Line::DECLARE(key, value) =>
      {
        match variable_declaration_indices.get_mut(&key)
        {
          Some(indices) =>
          {
            if !indices.is_empty() && *indices.last().unwrap() == current_index
            {
              println!("{output}MULTIPLE DECLARATION");
              return Ok(());
            }
            else
            {
              indices.push(current_index);
              variable_declarations[current_index as usize].insert(key, value);
            }
          }
          None =>
          {
            let mut indices = Vec::with_capacity(MAX_LINE_SIZE as usize);
            indices.push(current_index);
            variable_declaration_indices.insert(key, indices);
            variable_declarations[current_index as usize].insert(key, value);
          }
        }

      }

    } // match
  } // for

  println!("\n{output}");
  Ok(())
}

#[derive(Clone, Copy, Hash)]
struct Cstr
{
  str: [char; C_STR_SIZE],
}

impl PartialEq for Cstr
{
  fn eq(&self, other: &Self) -> bool
  {
    for i in 0..C_STR_SIZE
    {
      if self.str[i] != other.str[i]
      {
        return false;
      }
    }
    return true;
  }
}

impl Eq for Cstr {}

trait CstrObj
{
  fn new() -> Self;

  fn from_str(from: &str) -> Self;
}

impl CstrObj for Cstr
{
  fn new() -> Self
  {
    Cstr { str: ['\0'; C_STR_SIZE] }
  }

  fn from_str(from: &str) -> Self
  {
    let mut new: Cstr = Cstr::new();

    for (i, char) in from.chars().enumerate()
    {
      if i >= C_STR_SIZE { break }
      new.str[i] = char;
    }
    return new;
  }
}

impl fmt::Display for Cstr
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
  {
    let mut s: String = String::with_capacity(C_STR_SIZE);
    for c in self.str
    {
      if c == '\0'
      {
        break;
      }

      s.push(c);
    }
    write!(f, "{s}")
  }
}

enum Line
{
  OPENBL,
  CLOSEBL,
  TYPEOF (Cstr),
  DECLARE (Cstr, Cstr),
}

fn read_line() -> String
{
  let mut input: String = String::new();
  let stdin: io::Stdin = io::stdin(); // We get `Stdin` here.
  match stdin.read_line(&mut input)
  {
    Ok(_) => return input,
    Err(error) => panic!("Error {}", error.to_string()),
  }
}

fn read_line_as_int() -> u32
{
  return read_line().trim().parse::<u32>().unwrap();
}

fn read_nr_of_lines(nr_of_lines: usize) -> Vec<Line>
{
  let mut lines: Vec<Line> = Vec::with_capacity(nr_of_lines);

  let max_in_size: usize = MAX_LINE_SIZE as usize * nr_of_lines * 2;
  let buf_reader = BufReader::with_capacity(max_in_size, io::stdin().lock());

  for read_line in buf_reader.lines()
  {
    let line: &String = &read_line.unwrap();
    
    if line.is_empty()
    {
      break;
    }
    lines.push(string_to_enum(line));
  }

  return lines;
}

fn string_to_enum(s: &str) -> Line
{
  match s.chars().next()
  {
    Some('{') => Line::OPENBL,
    Some('}') => Line::CLOSEBL,
    Some('T') => Line::TYPEOF(Cstr::from_str(&s[7..])),
    Some('D') =>
    {
      let split: Vec<&str> = s[8..].split(" ").collect();
      let key = split.get(0).unwrap();
      let value = split.get(1).unwrap();

      return Line::DECLARE(Cstr::from_str(key), Cstr::from_str(value));
    }
    _ => panic!("Unexpected line! '{}'", s),
  }
}