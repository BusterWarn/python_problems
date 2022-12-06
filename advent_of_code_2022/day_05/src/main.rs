use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

const FILENAME: &str = "input.txt";
const INDICES: [u8; 9] = [1, 5, 9, 13, 17, 21, 25, 29, 33];

fn main()
{
  let (cargo, operations) = read_lines();

  problem_1(cargo.clone(), &operations);
  problem_2(cargo.clone(), &operations);
}

fn problem_1(mut cargo: HashMap<u8, Vec<char>>, operations: &Vec<[u8; 3]>)
{
  println!("Problem 1");
  
  for op in operations
  {
    for _  in 0..*op.get(0).unwrap()
    {
      let c = *cargo.get_mut(op.get(1).unwrap()).unwrap().last().unwrap();
      cargo.get_mut(op.get(1).unwrap()).unwrap().pop();
      cargo.get_mut(op.get(2).unwrap()).unwrap().push(c);
    }
  }

  for i in 1..=INDICES.len() as u8
  {
    print!("{}", cargo.get(&i).unwrap().last().unwrap());
  }
  println!();
}

fn problem_2(mut cargo: HashMap<u8, Vec<char>>, operations: &Vec<[u8; 3]>)
{
  println!("\nProblem 2");
  
  for op in operations
  {
    // Wait don't a queue make more sense here... And this is a string!
    let mut cargo_stack = String::with_capacity(*op.get(0).unwrap() as usize);
    {
      let from = cargo.get_mut(&op.get(1).unwrap()).unwrap();
      for _  in 0..*op.get(0).unwrap()
      {
        cargo_stack.push(*from.last().unwrap());
        from.pop();
      }
    }

    {
      let to = cargo.get_mut(&op.get(2).unwrap()).unwrap();
      while !cargo_stack.is_empty()
      {
        to.push(cargo_stack.chars().last().unwrap());
        cargo_stack.pop();
      }
    }
  }

  for i in 1..=INDICES.len() as u8
  {
    print!("{}", cargo.get(&i).unwrap().last().unwrap());
  }
}

fn read_lines() -> (HashMap<u8, Vec<char>>, Vec<[u8; 3]>)
{
  let file = File::open(FILENAME).unwrap();
  let read_lines = io::BufReader::new(file).lines();

  let mut cargo: HashMap<u8, Vec<char>> = HashMap::with_capacity(INDICES.len());
  for i in 1..=INDICES.len() as u8
  {
    cargo.insert(i, Vec::with_capacity(INDICES.len()));
  }
  let mut operations: Vec<[u8; 3]> = Vec::new();

  for read_line in read_lines
  {
    let line = read_line.unwrap();
    if line.contains('[') && line.contains(']')
    {
      for i in 1..=INDICES.len() as u8
      {
        match line.chars().nth(INDICES[i as usize - 1] as usize).unwrap()
        {
          ' ' => (),
          c => cargo.get_mut(&i).unwrap().insert(0, c)
        }
      }
    }
    else if line.contains("move")
    {
      let op: Vec<u8> = line
        .split_whitespace()
        .filter_map(|p| p.parse::<u8>().ok())
        .collect();
      
      assert!(op.len() == 3);

      operations.push([*op.get(0).unwrap(),
                       *op.get(1).unwrap(),
                       *op.get(2).unwrap()]);
    }
  }

  return (cargo, operations);
}
