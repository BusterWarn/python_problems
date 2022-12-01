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
  let mut max_cals: u64 = 0;
  let mut cur_cals: u64 = 0;
  for line in lines
  {
    if line.is_empty()
    {
      max_cals = if cur_cals > max_cals { cur_cals } else { max_cals };
      cur_cals = 0;
      continue;
    }
    
    cur_cals = cur_cals + line.parse::<u64>().unwrap();
  }
  println!("{max_cals}");
}

fn problem_2(lines: &Vec<String>)
{
  println!("\nProblem 2");
  let mut max_cals: [u64; 3] = [0; 3];
  let mut cur_cals: u64 = 0;
  for line in lines
  {
    if line.is_empty()
    {
      if cur_cals > max_cals[0]
      {
        max_cals[0] = cur_cals;
        max_cals.sort();

      }
      
      cur_cals = 0;
      continue;
    }
    
    cur_cals = cur_cals + line.parse::<u64>().unwrap();
  }

  println!("{}", max_cals[0] + max_cals[1] + max_cals[2]);
}

fn read_lines() -> Vec<String>
{
  let file = File::open(FILENAME).unwrap();
  let read_lines = io::BufReader::new(file).lines();

  let mut lines: Vec<String> = Vec::new();
  for read_line in read_lines
  {
    lines.push(String::from(read_line.unwrap()));
  }

  // Push empty line to help with edge case.
  lines.push(String::new());

  return lines;
}
