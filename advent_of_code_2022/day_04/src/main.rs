use std::fs::File;
use std::io::{self, BufRead};

const FILENAME: &str = "input.txt";

fn main()
{
  let lines = read_lines();
  solve_problem(&lines, &two_sections_completely_overlap);
  solve_problem(&lines, &two_sections_somewhat_overlap);
}

fn solve_problem(lines: &Vec<String>, has_overlap: &dyn Fn(Vec<&str>) -> bool)
{
  static mut PROBLEM_NR: u8 = 1;
  unsafe { println!("Problem {PROBLEM_NR}") };
  let mut overlapping_sections: u64 = 0;
  for line in lines
  {
    let sections: Vec<&str> = line.split(",").collect();
    if has_overlap(sections)
    {
      overlapping_sections += 1
    }
  }
  println!("Overlapping sections: {overlapping_sections}\n");
  unsafe { PROBLEM_NR += 1 };
}

fn two_sections_completely_overlap(sections: Vec<&str>) -> bool
{
  let sec_1 = parse_section(sections.get(0).unwrap());
  let sec_2 = parse_section(sections.get(1).unwrap());

  if sec_1.0 <= sec_2.0 && sec_1.1 >= sec_2.1
  {
    return true
  }

  if sec_2.0 <= sec_1.0 && sec_2.1 >= sec_1.1
  {
    return true
  }

  return false;
}

fn two_sections_somewhat_overlap(sections: Vec<&str>) -> bool
{
  let sec_1 = parse_section(sections.get(0).unwrap());
  let sec_2 = parse_section(sections.get(1).unwrap());

  if sec_1.1 < sec_2.0
  {
    return false
  }

  if sec_1.0 > sec_2.1
  {
    return false
  }

  return true;
}

fn parse_section(section: &str) -> (u32, u32)
{
  let temp: Vec<u32> = section.split('-')
                              .map(|s| s.parse::<u32>().unwrap())
                              .collect::<Vec<u32>>();

  return (*temp.get(0).unwrap(), *temp.get(1).unwrap());
}

fn read_lines() -> Vec<String>
{
  let file = File::open(FILENAME).unwrap();
  let read_lines = io::BufReader::new(file).lines();

  let mut lines: Vec<String> = Vec::new();
  for read_line in read_lines
  {
    let line: String = read_line.unwrap();
    assert!(line.contains('-') && line.contains(','));
    lines.push(line);
  }

  // Push empty line to help with edge case.
  // lines.push(String::new());
  return lines;
}
