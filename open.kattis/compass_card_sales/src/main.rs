use std::{io::{self, BufRead, BufReader}, collections::{BTreeMap, HashMap}};

fn main()
{
  let nr_of_lines: usize = read_line_as_int() as usize;
  let mut cards: Vec<Card> = read_nr_of_lines(nr_of_lines);

  update_all_cards(&mut cards);


  while cards.len() > 1
  {
    let index_of_card = cards
      .iter()
      .position(|card| card.id == id_of_card_to_sell)
      .unwrap();
    cards.remove(index_of_card);
  }
  println!("{}", cards.first().unwrap().id);
 
  return items.first().unwrap().0;

}

fn get_index_of_card_to_sell(cards: &Vec<Card>) -> usize
{
  let mut items = cards
    .iter()
    .enumerate()
    .map(|(index, card)| (index, card.id, card.uniqueness))
    .collect::<Vec<(usize, u32, u16)>>();

  items.sort_by(|(_, k_1, v_1), (_, k_2,v_2)| {

    if v_1 < v_2 { return std::cmp::Ordering::Less }
    if v_1 > v_2 { return std::cmp::Ordering::Greater }

    if k_1 > k_2 { return std::cmp::Ordering::Less }
    if k_1 < k_2 { return std::cmp::Ordering::Greater }

    panic!("Keys are duplicate");
  });
  return items.first().unwrap().0;
}

#[derive(Debug, Clone)]
struct Card
{
  rgb: [u16; 3],
  id: u32,
  f_index: usize,
  b_index: usize,
  uniqueness: u16
}

fn update_all_cards(cards: &mut Vec<Card>)
{
  // let mut uniqueness: HashMap<u32, u16> = HashMap::with_capacity(cards.len());
  // let mut uniqueness: BTreeMap<u32, u16> = BTreeMap::new();

  for index in 0..cards.len()
  {
    let card_to_edit: &mut Card = &mut cards.get(index).unwrap();
    update_card(card_to_edit, cards);
  }
}

fn update_card(card: &mut Card, cards: &mut Vec<Card>)
{
  for rgb_index in 0..3 as usize
  {
    let (dist_f, dist_b) = get_closest_two_cards(card,
                                                           &cards,
                                                           rgb_index);
    assert!(dist_f <= 359);
    assert!(dist_b <= 359);
    card.uniqueness = dist_f + dist_b;
    assert!(card.uniqueness <= 360);
  }
}

/**
 * Return -> Forward card distance, forwrad card index, behind card distance, behind card index.
 */
fn get_closest_two_cards(card: &mut Card, cards: &Vec<Card>, rgb_index: usize) -> (u16, u16)
{
  let mut f_card_dist: u16 = u16::MAX;
  let mut b_card_dist: u16 = u16::MAX;
  
    for (index, other) in cards.iter().enumerate()
    {
      if card.id == other.id { continue; }
      if card.rgb[index] == other.rgb[index]
      {
        f_card_dist = 0;
        b_card_dist = 0;
        card.f_index = index;
        card.b_index = index;
      }
  
      let score_f = compute_distance_forward(&card.rgb[rgb_index], &other.rgb[rgb_index]);
      if score_f < f_card_dist
      {
        f_card_dist = score_f;
        card.f_index = index;
      }
      
      let score_b = compute_distance_backwards(&card.rgb[rgb_index], &other.rgb[rgb_index]);
      if score_b < b_card_dist
      {
        b_card_dist = score_b;
        card.b_index = index;
      }
    }

  return (f_card_dist, b_card_dist);
}

fn compute_distance_forward(from: &u16, to: &u16) -> u16
{
  if to >= from
  {
    return to - from;
  }
  return 360 + to - from;
}

fn compute_distance_backwards(from: &u16, to: &u16) -> u16
{
  if to <= from
  {
    return from - to;
  }
  return 360 + from - to;
}

fn read_line() -> String
{
  let mut input: String = String::new();
  let stdin: io::Stdin = io::stdin();
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

fn read_nr_of_lines(nr_of_lines: usize) -> Vec<Card>
{
  let mut lines: Vec<Card> = Vec::with_capacity(nr_of_lines);

  let buf_reader = BufReader::new(io::stdin().lock());
  for read_line in buf_reader.lines()
  {
    let line: &String = &read_line.unwrap();
    
    if line.is_empty()
    {
      break;
    }

    let p: Vec<u32> = line.trim()
                          .split_whitespace()
                          .map(|s| s.parse::<u32>().unwrap())
                          .collect();
    lines.push(Card { rgb: [*p.get(0).unwrap() as u16,
                            *p.get(1).unwrap() as u16,
                            *p.get(2).unwrap() as u16],
                      id: *p.get(3).unwrap(),
                      f_index: usize::MAX,
                      b_index: usize::MAX,
                    uniqueness: 0});
  }

  return lines;
}

#[cfg(test)]
mod tests {
  use std::vec;
  use crate::{compute_distance_forward, compute_distance_backwards, Card, update_all_cards};
  use rand::Rng;

  #[test]
  fn test_compute_distance_forward()
  {
    for i in 0..360 as u16
    {
      for j in 0..360 as u16
      {
        assert!(compute_distance_forward(&i, &j) <= 359);
      }
    }
  }

  #[test]
  fn test_compute_distance_backwards()
  {
    for i in 0..360 as u16
    {
      for j in 0..360 as u16
      {
        assert!(compute_distance_backwards(&i, &j) <= 359);
      }
    }
  }

  
  #[test]
  fn wooo()
  {
    for _ in 0..10000 as u16
    {
      let cards: Vec<Card> = vec![
        Card{ rgb: [rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)], id: 1, f_index: usize::MAX as usize, b_index: usize::MAX, uniqueness: 0},
        Card{ rgb: [rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)], id: 2, f_index: usize::MAX as usize, b_index: usize::MAX, uniqueness: 0},
        Card{ rgb: [rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)], id: 3, f_index: usize::MAX as usize, b_index: usize::MAX, uniqueness: 0},
        Card{ rgb: [rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)], id: 4, f_index: usize::MAX as usize, b_index: usize::MAX, uniqueness: 0},
        Card{ rgb: [rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)], id: 5, f_index: usize::MAX as usize, b_index: usize::MAX, uniqueness: 0},
        Card{ rgb: [rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)], id: 6, f_index: usize::MAX as usize, b_index: usize::MAX, uniqueness: 0}];

        println!("\n\n{cards:?}");
        // compute(&cards);
    }
  }
}