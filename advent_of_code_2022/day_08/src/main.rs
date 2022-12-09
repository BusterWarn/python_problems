use std::fs::File;
use std::io::{self, BufRead};

// Okay I sneaked peeked the input for this one it's 99x99
// const FOREST_SIZE: usize = 99;
const FOREST_SIZE: usize = 5;

type ForestT =  [[u8; FOREST_SIZE]; FOREST_SIZE];

const FILENAME: &str = "input.txt";

fn main()
{
  let forest: ForestT = read_lines();
  problem_1(&forest);
  problem_2(&forest);
}

fn problem_1(forest: &ForestT)
{
  println!("Problem 1");
  
  let mut covered_trees: u32 = 0;
  for x in 0..FOREST_SIZE
  {
    for y in 0..FOREST_SIZE
    {
      if tree_is_horizontally_covered(x, y, forest) &&
         tree_is_vertically_covered(x, y, forest)
      {
        covered_trees += 1;
      }
    }
  }
  println!("Covered trees: {covered_trees}");
}

fn problem_2(_forest: &ForestT)
{
  println!("\nProblem 2");
}

fn tree_is_horizontally_covered(tree_x: usize, tree_y: usize, forest: &ForestT) -> bool
{
  if tree_x == 0 || tree_x == FOREST_SIZE - 1
  {
    return false;
  }
  let tree_height: u8 = forest[tree_x][tree_y];
  let mut seen_from_left: bool = true;
  let mut seen_from_right: bool = true;

  for x in 0..FOREST_SIZE
  {
    for y in 0..FOREST_SIZE
    {
      if x != tree_x || y == tree_y { continue }

      if x < tree_x
      {
        if forest[x][y] >= tree_height
        {
          seen_from_left = false;
        }
      }
      else
      {
        if forest[x][y] >= tree_height
        {
          seen_from_right = false;
        }
      }
    }
  }
  return !seen_from_left && !seen_from_right;
}

fn tree_is_vertically_covered(tree_x: usize, tree_y: usize, forest: &ForestT) -> bool
{
  if tree_y == 0 || tree_y == FOREST_SIZE - 1
  {
    return false;
  }
  let tree_height: u8 = forest[tree_x][tree_y];
  let mut seen_from_above: bool = true;
  let mut seen_from_below: bool = true;

  for x in 0..FOREST_SIZE
  {
    for y in 0..FOREST_SIZE
    {
      if y != tree_y || x == tree_x { continue }

      if y < tree_y
      {
        if forest[x][y] > tree_height
        {
          seen_from_above = false;
        }
      }
      else
      {
        if forest[x][y] > tree_height
        {
          seen_from_below = false;
        }
      }
    }
  }
  return !seen_from_above && !seen_from_below;
}

fn read_lines() -> ForestT
{
  let mut forest: ForestT = [[0; FOREST_SIZE]; FOREST_SIZE];
  for (i, read_line) in io::BufReader::new(File::open(FILENAME).unwrap()).lines().enumerate()
  {
    for (j, tree) in read_line.unwrap().chars().map(|c| c as u8 - 48).enumerate()
    {
      forest[i][j] = tree;
    }
  }

  return forest;
}
