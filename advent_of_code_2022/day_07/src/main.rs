use std::borrow::Borrow;
use std::fs::File;
use std::io::{self, BufRead};
use std::{cell::RefCell, rc::Rc};

const FILENAME: &str = "input.txt";

fn main()
{
  let _name: &str = "name";
  greet(_name);
  let lines: Vec<Line> = read_lines();

  problem_1(&lines);
  problem_2(&lines);
}

fn greet(_name: &str)
{

}

fn problem_1(lines: &Vec<Line>)
{
  println!("Problem 1");

  let mut dirs: Vec<Rc<RefCell<Dir>>> = Vec::new();
  let mut path: Vec<String> = Vec::new();

  for line in lines
  {
    match line
    {
      Line::Ls => (),
      Line::Cd(cd_to) =>
      {
        if cd_to == ".."
        {
          path.pop();
        }
        else
        {
          path.push(cd_to.clone());
          
          let dir = dirs.
            iter()
            .filter(|d|{
              let p: Dir = *d.borrow();
              p.name == *cd_to;
            })
            .next();
          if dir.is_none()
          {
            dirs.push(Rc::new(RefCell::new(Dir::new(cd_to.clone()))));
          }
        }
      },
      Line::Output(first, second) =>
      {
        let cur_dir = get_dir(&dirs, path.last().unwrap());
        match first.parse::<u64>()
        {
          Ok(size) =>
          {
            cur_dir.get_mut().files.push((second.clone(), size));
            //let stored_size = dirs.get_mut(path.last().unwrap().name.clone()).unwrap();
            //*stored_size = *stored_size + s;
          },
          _ =>
          {
            let other_dir = get_dir(&dirs, second);
            cur_dir.borrow_mut().dirs.push(other_dir);
          },
        }
      }
    }
  }

  if false
  {
    for dir in dirs.clone()
    {
      // DANGELOUS
      println!("dir {} - {}", dir.borrow().name, dir.borrow().get_size());
    }
  }

  let sum = dirs
    .iter()
    .filter(|dir| dir.get_size() <= 100000 as u64)
    .map(|dir| dir.get_size())
    .sum::<u64>();
  println!("Total disk space: {sum}");
}

fn problem_2(_lines: &Vec<Line>)
{
  println!("\nProblem 2");
}


struct Dir
{
  name: String,
  files: Vec<(String, u64)>,
  dirs: Vec<Rc<RefCell<Dir>>>
}

impl Dir
{
  pub fn new(name: String) -> Self
  {
    Dir
    {
      name: name,
      files: Vec::new(),
      dirs: Vec::new()
    }
  }

  pub fn get_size(&self) -> u64
  {
    let mut size = self.files
      .iter()
      .map(|(_, file_size)| file_size)
      .sum::<u64>();
    
    size = size + self.dirs
      .iter()
      .map(|dir| dir.get_size())
      .sum::<u64>();
    
    return size;
  }
}

fn get_dir(dirs: &Vec<Rc<RefCell<Dir>>>, dir_name: &String) -> Rc<RefCell<Dir>>
{
  let p =  dirs
      .iter()
      .filter(|d| d.borrow().name == *dir_name)
      .next()
      .unwrap();
  return *p;
}

enum Line
{
  Ls,
  Cd (String),
  Output(String, String)
}

impl Line
{
  pub fn from_string(from: String) -> Line
  {
    let mut from_split = from.split_whitespace();
    let mut next: &str = from_split.next().unwrap();

    if next != "$"
    {
      return Line::Output(String::from(next), String::from(from_split.next().unwrap()));
    }

    next = from_split.next().unwrap();
    if next == "ls"
    {
      return Line::Ls;
    }

    return Line::Cd(String::from(from_split.next().unwrap()))
  }
}

fn read_lines() -> Vec<Line>
{
  io::BufReader::new(File::open(FILENAME).unwrap())
    .lines()
    .map(|line| Line::from_string(line.unwrap()) )
    .collect::<Vec<Line>>()
}
