use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, stdin};
use std::fmt;

const MAX_LINE_SIZE: u16 = 7 + 1 + 6 + 1 + 6 + 2 + 10; // DECLARE + ws + <decl> + ws + <identifier> + \r\n + bonus
const C_STR_SIZE: usize = 6;

fn main() ->  io::Result<()>
{
  let nr_of_lines: u32 = read_line_as_int();
  let lines: Vec<Line> = read_nr_of_lines(nr_of_lines);

  let mut decls: Vec<HashMap<Cstr, Cstr>> = Vec::with_capacity(nr_of_lines as usize);
  decls.push(HashMap::with_capacity(nr_of_lines as usize));

  let mut decls_index: HashMap<Cstr, Vec<u32>> = HashMap::with_capacity(nr_of_lines as usize);
  let mut current_index: u32 = 0;

  let mut output: String = String::with_capacity(MAX_LINE_SIZE as usize * nr_of_lines as usize);

  // Loop over read lines.
  for (i, line) in lines.iter().enumerate()
  {
    // Match for each type of line that is read.
    match line
    {
      Line::OPENBL =>
      {
        decls.push(HashMap::with_capacity(nr_of_lines as usize - i));
        current_index += 1;
      }

      Line::CLOSEBL =>
      {
        let mut keys_to_remove: Vec<Cstr> = Vec::with_capacity(decls_index.len());
        for (key, indices) in &mut decls_index
        {
          match indices.iter().position(|x| *x == current_index)
          {
            Some(index) =>
            {
              _ = indices.remove(index);
              if indices.is_empty()
              {
                keys_to_remove.push(*key);
              }
            },
            None => (),
          }
        }
        for key in keys_to_remove
        {
          decls_index.remove(&key);
        }
        _ = decls.pop();
        current_index -= 1;
      }

      Line::TYPEOF(key) =>
      {
        match decls_index.get(&key)
        {
          Some(indices) =>
          {
            let index: usize = *indices.last().unwrap() as usize;
            let map = decls.get(index).unwrap();
            for c in map.get(&key).unwrap().str
            {
              output.push(c);
            }
            output.push('\n');
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
        let map = decls.last_mut().unwrap();  
        match map.get(&key)
        {
          Some(_) =>
          {
            println!("{output}MULTIPLE DECLARATION");
            return Ok(());
          },
          None =>
          {
            map.insert(*key, *value);
            match decls_index.get_mut(&key)
            {
              Some(indices) => indices.push(current_index),
              None =>
              {
                let mut indices: Vec<u32> = Vec::with_capacity(MAX_LINE_SIZE as usize);
                indices.push(current_index);
                decls_index.insert(*key, indices);
              },
            }
          }
        }
      }

    } // match
  } // for

  println!("{output}");
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

fn new_c_str() -> Cstr
{
  Cstr { str: ['\0'; C_STR_SIZE] }
}

fn new_c_str_from_str(from: &str) -> Cstr
{
  let mut new: Cstr = new_c_str();

  for (i, char) in from.chars().enumerate()
  {
    if i >= C_STR_SIZE { break }
    new.str[i] = char;
  }
  return new;
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

fn read_nr_of_lines(nr_of_lines: u32) -> Vec<Line>
{
  let mut lines: Vec<Line> = Vec::with_capacity(nr_of_lines as usize);

  let stdin: io::Stdin = io::stdin(); // We get `Stdin` here.
  let max_in_size: usize = MAX_LINE_SIZE as usize * nr_of_lines as usize;
  let buf = BufReader::with_capacity(max_in_size, stdin);
  // let mut input = String::with_capacity(MAX_LINE_SIZE as usize);

  let buf_lines = buf.lines();
  for line in buf_lines
  {
    let l: &String = &line.unwrap();
    
    if l.is_empty()
    {
      break;
    }
    lines.push(string_to_enum(l.trim()));
  }
  /*for _ in 0..nr_of_lines
  {
    match buf.read_line(&mut input)
    {
      Ok(_) =>
      {
        lines.push(string_to_enum(input.trim()));
        input.clear();
      }
      Err(error) => panic!("Error {}", error.to_string())
    }
  }*/

  return lines;
}

fn string_to_enum(s: &str) -> Line
{
  match s.chars().next()
  {
    Some('{') => Line::OPENBL,
    Some('}') => Line::CLOSEBL,
    Some('T') => Line::TYPEOF(new_c_str_from_str(&s.trim()[7..])),
    Some('D') =>
    {
      let split: Vec<&str> = s[8..].trim().split(" ").collect();
      let key = split.get(0).unwrap();
      let value = split.get(1).unwrap();

      return Line::DECLARE(new_c_str_from_str(key), new_c_str_from_str(value));
    }
    _ => panic!("Unexpected line! '{}'", s),
  }
}