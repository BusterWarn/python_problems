use std::fs::File;
use std::io::{self, BufRead};

const FILENAME: &str = "input.txt";

fn main()
{
  let line = read_line();
  find_unique_code(&line, 4);
  find_unique_code(&line, 14);
}

fn find_unique_code(line: &String, len: usize)
{
  let mut queue: Vec<char> = Vec::with_capacity(len);
  let mut solution: u32 = 0;
  for (i, c) in line.chars().enumerate()
  {
    queue.push(c);
    
    if queue.len() < len
    {
      continue
    }

    if queue_is_unique(&queue)
    {
      solution = i as u32 + 1;
      break;
    }
    queue.remove(0);
  }
  println!("Solution to len: {len} = {solution}\n");
}

fn queue_is_unique(queue: &Vec<char>) -> bool
{
  for (i, c_1) in queue.iter().enumerate()
  {
    for (j, c_2) in queue.iter().enumerate()
    {
      if c_1 == c_2 && i != j
      {
        return false;
      }
    }
  }

  return true;
}

fn read_line() -> String
{
  let mut buf: String = String::new();
  let file = File::open(FILENAME).unwrap();
  _ = io::BufReader::new(file).read_line(&mut buf);

  return buf;
}
