use std::fs::File;
use std::io::{self, BufRead};

const FILENAME: &str = "input.txt";

fn main()
{
  let lines = read_lines();
  problem_1(&lines);
  problem_2(&lines);
}

fn problem_1(lines: &Vec<String>)
{
  println!("Problem 1");
  let mut sum: u64 = 0;
  for line in lines
  {
    let (first, second) = line.split_at(line.len() / 2);
    for item in first.chars()
    {
      if second.contains(item)
      {
        sum += char_to_u64(item);
        break;
      }
    }
  }
  println!("Sum: {sum}");
}

fn problem_2(lines: &Vec<String>)
{
  println!("\nProblem 2");

  let mut sum: u64 = 0;
  for group in lines.chunks(3)
  {
    for item in group.get(0).unwrap().chars()
    {
      let backpack_2 = group.get(1).unwrap();
      let backpack_3 = group.get(2).unwrap();
      if backpack_2.contains(item) && backpack_3.contains(item)
      {
        sum += char_to_u64(item);
        break;
      }
    }
  }
  println!("Sum: {sum}");
}

fn char_to_u64(c: char) -> u64
{
  return (c as u8 - if c as u8 > 96 { 96 } else { 38 }) as u64
}

fn read_lines() -> Vec<String>
{
  let file = File::open(FILENAME).unwrap();
  let read_lines = io::BufReader::new(file).lines();

  let mut lines: Vec<String> = Vec::new();
  for read_line in read_lines
  {
    let line: String = read_line.unwrap();
    if line.len() % 2 > 0 { panic!("Invalid input size {} {}", line.len(), line)}
    lines.push(line);
  }

  // Push empty line to help with edge case.
  // lines.push(String::new());
  return lines;
}
