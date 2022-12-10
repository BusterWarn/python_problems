use std::fs::File;
use std::io::{self, BufRead};

// Okay I sneaked peeked the input for this one it's 99x99
const FOREST_SIZE: usize = 99;

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
  
  let mut visible_trees: u32 = 0;

  for x in 0..FOREST_SIZE
  {
    for y in 0..FOREST_SIZE
    {
      if !tree_is_horizontally_covered(x, y, forest) ||
         !tree_is_vertically_covered(x, y, forest)
      {
        visible_trees += 1;
      }
    }
  }
  println!("Visible trees: {visible_trees}");
}

fn problem_2(forest: &ForestT)
{
  println!("\nProblem 2");
  let mut scenic_score: u64 = 0;

  for x in 0..FOREST_SIZE
  {
    for y in 0..FOREST_SIZE
    {
      let mut tree_scenic_score: u64 = 1;
      tree_scenic_score *= tree_horizontal_scenic_score(x, y, forest);
      tree_scenic_score *= tree_vertical_scenic_score(x, y, forest);
      if tree_scenic_score > scenic_score
      {
        scenic_score = tree_scenic_score;
      }
    }
  }

  println!("Forest scenic score: {}", scenic_score);
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

  for y in 0..FOREST_SIZE
  {
    if y < tree_y
    {
      if forest[tree_x][y] >= tree_height
      {
        seen_from_left = false;
      }
    }
    else if y > tree_y
    {
      if forest[tree_x][y] >= tree_height
      {
        seen_from_right = false;
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
    if x < tree_x
    {
      if forest[x][tree_y] >= tree_height
      {
        seen_from_above = false;
      }
    }
    else if x > tree_x
    {
      if forest[x][tree_y] >= tree_height
      {
        seen_from_below = false;
      }
    }
  }
  return !seen_from_above && !seen_from_below;
}

fn tree_horizontal_scenic_score(tree_x: usize, tree_y: usize, forest: &ForestT) -> u64
{
  let tree_height: u8 = forest[tree_x][tree_y];

  let mut left_scenic_score: u64 = 0;
  for y in (0..tree_y).rev()
  {
    left_scenic_score += 1;
    if forest[tree_x][y] >= tree_height
    {
      break;
    }
  }

  let mut right_scenic_score: u64 = 0;
  for y in tree_y + 1..FOREST_SIZE
  {
    right_scenic_score += 1;
    if forest[tree_x][y] >= tree_height
    {
      break;
    }
  }

  return left_scenic_score * right_scenic_score;
}

fn tree_vertical_scenic_score(tree_x: usize, tree_y: usize, forest: &ForestT) -> u64
{
  let tree_height: u8 = forest[tree_x][tree_y];
  let mut above_scenic_score: u64 = 0;

  for x in (0..tree_x).rev()
  {
    above_scenic_score += 1;
    if forest[x][tree_y] >= tree_height
    {
      break;
    }
  }

  let mut below_scenic_score: u64 = 0;
  for x in tree_x + 1..FOREST_SIZE
  {
    below_scenic_score += 1;
    if forest[x][tree_y] >= tree_height
    {
      break;
    }
  }

  return above_scenic_score * below_scenic_score;
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
