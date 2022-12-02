use std::fs::File;
use std::io::{self, BufRead};


const FILENAME: &str = "input.txt";

#[derive(Copy, Clone)]
enum RPS
{
  Rock,
  Paper,
  Scissor
}

enum Game
{
  Win,
  Loose,
  Draw
}

fn main()
{
  problem_1();
  problem_2();
}

fn problem_1()
{
  println!("Problem 1");
  let rounds = read_lines_problem_1();
  let mut final_score: u64 = 0;
  for hand in rounds
  {
    let game_result: Game = play_round(&hand);
    final_score += calculate_round_score(&game_result, &hand.1) as u64;
  }
  println!("{final_score}");
}

fn problem_2()
{
  println!("Problem 2");
  let rounds = read_lines_problem_2();
  let mut final_score: u64 = 0;
  for round in rounds
  {
    let rhs = decrypt_rhs_hand(&round.1, &round.0);
    final_score += calculate_round_score(&round.1, &rhs) as u64;
  }
  println!("{final_score}");
}

fn read_lines_problem_1() -> Vec<(RPS, RPS)>
{
  let file = File::open(FILENAME).unwrap();
  let read_lines = io::BufReader::new(file).lines();

  let mut lines: Vec<(RPS, RPS)> = Vec::new();
  for read_line in read_lines
  {
    lines.push(string_to_rps(&read_line.unwrap()));
  }

  return lines;
}

fn string_to_rps(s: &String) -> (RPS, RPS)
{
  let rps_1 = char_to_rps(&s.as_str().chars().nth(0).unwrap());
  let rps_2 = char_to_rps(&s.as_str().chars().nth(2).unwrap());
  return (rps_1, rps_2);
}

fn read_lines_problem_2() -> Vec<(RPS, Game)>
{
  let file = File::open(FILENAME).unwrap();
  let read_lines = io::BufReader::new(file).lines();

  let mut lines: Vec<(RPS, Game)> = Vec::new();
  for read_line in read_lines
  {
    lines.push(string_to_rps_and_game(&read_line.unwrap()));
  }

  return lines;
}

fn string_to_rps_and_game(s: &String) -> (RPS, Game)
{
  let rps = char_to_rps(&s.as_str().chars().nth(0).unwrap());
  let game = char_to_game(&s.as_str().chars().nth(2).unwrap());
  return (rps, game);
}

fn char_to_rps(c: &char) -> RPS
{
  match c
  {
    'A' => RPS::Rock,
    'B' => RPS::Paper,
    'C' => RPS::Scissor,
    'X' => RPS::Rock,
    'Y' => RPS::Paper,
    'Z' => RPS::Scissor,
    _ => panic!("Invalid char: {}", c)
  }
}

fn char_to_game(c: &char) -> Game
{
  // Reversed here for stupid reason.
  match c
  {
    'X' => Game::Win,
    'Y' => Game::Draw,
    'Z' => Game::Loose,
    _ => panic!("Invalid char: {}", c)
  }
}

/**
 * Returns the result from left hand side
 */
fn play_round(hands: &(RPS, RPS)) -> Game
{
  match hands.0
  {
    RPS::Rock =>
    {
      match hands.1
      {
        RPS::Rock => return Game::Draw,
        RPS::Paper => return Game::Loose,
        RPS::Scissor => return Game::Win
      }
    }
    RPS::Paper =>
    {
      match hands.1
      {
        RPS::Rock => return Game::Win,
        RPS::Paper => return Game::Draw,
        RPS::Scissor => return Game::Loose
      }
    }
    RPS::Scissor =>
    {
      match hands.1
      {
        RPS::Rock => return Game::Loose,
        RPS::Paper => return Game::Win,
        RPS::Scissor => return Game::Draw
      }
    }
  }
}

fn calculate_round_score(game_result: &Game, right_side_hand: &RPS) -> u32
{
  let hand_points: u32 = match right_side_hand
  {
    RPS::Rock => 1,
    RPS::Paper => 2,
    RPS::Scissor => 3 
  };

  // This is from LHS perspective so reverese scores
  let score_points: u32 = match game_result
  {
    Game::Win => 0,
    Game::Draw => 3,
    Game::Loose => 6
  };

  return hand_points + score_points;
}

fn decrypt_rhs_hand(predicted_game_result: &Game, lfs_hand: &RPS) -> RPS
{
  match predicted_game_result
  {
    Game::Win => match lfs_hand
    {
      RPS::Rock => return RPS::Scissor,
      RPS::Paper => return RPS::Rock,
      RPS::Scissor => return RPS::Paper
    },
    Game::Draw => return lfs_hand.clone(),
    Game::Loose => match lfs_hand
    {
      RPS::Rock => return RPS::Paper,
      RPS::Paper => return RPS::Scissor,
      RPS::Scissor => return RPS::Rock
    },
  }
}