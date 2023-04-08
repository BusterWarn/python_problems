use std::io::{self, BufRead, BufReader};
use std::collections::HashSet;

const DEBUG: bool = false;
const CAPACITY: usize = 100;

fn main()
{
  let nr_of_lines: usize = read_line_as_int() as usize;
  let mut cards: Vec<Card> = read_nr_of_lines(nr_of_lines);
  let result = compute(&mut cards);
  println!("{}", result);
}

#[derive(Debug, Clone)]
struct Card
{
  id: u32,
  index: usize,
  rgb: [u16; 3],
  uniqueness: u16,
  sold: bool,
  f_rgb_index: [usize; 3],
  b_rgb_index: [usize; 3],
  rgb_duplicates: [HashSet<usize>; 3]
}

impl Card
{
  pub fn new(id: u32, r: u16, g: u16, b: u16) -> Self
  {
    Card
    {
      id: id,
      index: usize::MAX,
      rgb: [r, g, b],
      uniqueness: 0,
      sold: false,
      f_rgb_index: [usize::MAX, usize::MAX, usize::MAX],
      b_rgb_index: [usize::MAX, usize::MAX, usize::MAX],
      rgb_duplicates: [HashSet::<usize>::with_capacity(CAPACITY), HashSet::<usize>::with_capacity(CAPACITY), HashSet::<usize>::with_capacity(CAPACITY)]
    }
  }

  fn i_have_been_sold_update_cards(&mut self, cards: &mut Vec<Card>, rgb_index: usize)
  {
    let back_index = self.b_rgb_index[rgb_index];
    let front_index = self.f_rgb_index[rgb_index];

    let my_angle = self.rgb[rgb_index];
    let back_angle: u16;
    let front_angle: u16;
    unsafe
    {
      back_angle = cards.get_unchecked(back_index).rgb[rgb_index];
      front_angle = cards.get_unchecked(front_index).rgb[rgb_index];
    }

    // These values might not be used, iff the card being sold had at least 2 duplicates.
    let new_score_for_cards_behind: u16 = compute_distance_forward(my_angle, front_angle);
    let new_score_for_cards_in_front: u16 = compute_distance_backwards(my_angle, back_angle);

    // Update my duplicates
    let new_index_because_i_was_duplicate: Option<usize> =
      self.i_have_been_sold_update_my_duplicates(cards, rgb_index, new_score_for_cards_in_front + new_score_for_cards_behind);

    match new_index_because_i_was_duplicate
    {
      // I only need to update back and front card with new index, no other adjustments need to be made
      Some(new_index) => Self::update_duplicates_with_new_indices(cards, rgb_index, new_index, back_index, front_index),

      // Back and front cards need new indices and their uniqueness updated.
      None => self.update_adjacent_cards(rgb_index, cards, back_index, front_index, new_score_for_cards_behind, new_score_for_cards_in_front)
    }
  }

  fn i_have_been_sold_update_my_duplicates(&mut self, cards: &mut Vec<Card>, rgb_index: usize, new_uniqueness: u16) -> Option<usize>
  {
    if self.rgb_duplicates[rgb_index].is_empty()
    {
      return None;
    }

    let mut new_index_because_i_was_duplicate: Option<usize> = None;
    unsafe
    {
      let dup_indices: Vec<&usize> = self.rgb_duplicates[rgb_index].iter().collect();

      for duplicate_index in dup_indices
      {
        new_index_because_i_was_duplicate = Some(*duplicate_index);
        let duplicate_card = cards.get_unchecked_mut(*duplicate_index);
        duplicate_card.rgb_duplicates[rgb_index].remove(&self.index);

        // If this card is no longer a duplicate, we need to update its uniqueness.
        if duplicate_card.rgb_duplicates[rgb_index].is_empty()
        {
          duplicate_card.uniqueness += new_uniqueness;
        }
      }
    }

    return new_index_because_i_was_duplicate;
  }

  fn update_adjacent_cards(
    &mut self,
    rgb_index: usize,
    cards: &mut Vec<Card>,
    back_index: usize,
    front_index: usize,
    new_score_for_cards_behind: u16,
    new_score_for_cards_in_front: u16)
  {
    unsafe
    {
      // Update cards in front
      let front_card = cards.get_unchecked(front_index);
      let front_is_duplicate = if front_card.rgb_duplicates[rgb_index].is_empty() { false } else { true };
      let mut front_indices_to_update: Vec<usize> = Vec::with_capacity(CAPACITY);

      front_indices_to_update.push(front_index);
      front_indices_to_update.extend(front_card.rgb_duplicates[rgb_index].iter());

      for front_index_to_update in front_indices_to_update
      {
        let card_to_update = cards.get_unchecked_mut(front_index_to_update);
        card_to_update.b_rgb_index[rgb_index] = back_index;
      }

      if !front_is_duplicate
      {
        let front_card = cards.get_unchecked_mut(front_index);
        front_card.uniqueness += new_score_for_cards_in_front;
      }
    }

    unsafe
    {
      // Update cards behind
      let back_card = cards.get_unchecked(back_index);
      let back_is_duplicate = if back_card.rgb_duplicates[rgb_index].is_empty() { false } else { true };
      let mut back_indices_to_update: Vec<usize> = Vec::with_capacity(CAPACITY);

      back_indices_to_update.push(back_index);
      back_indices_to_update.extend(back_card.rgb_duplicates[rgb_index].iter());

      for back_index_to_update in back_indices_to_update
      {
        let card_to_update = cards.get_unchecked_mut(back_index_to_update);
        card_to_update.f_rgb_index[rgb_index] = front_index;
      }

      if !back_is_duplicate
      {
        let back_card = cards.get_unchecked_mut(back_index);
        back_card.uniqueness += new_score_for_cards_behind;
      }
    }
  }

  fn update_duplicates_with_new_indices(cards: &mut Vec<Card>, rgb_index: usize, new_index: usize, back_index: usize, front_index: usize)
  {
    let mut back_indices_to_update: Vec<usize> = Vec::with_capacity(10);
    back_indices_to_update.push(back_index);
    unsafe
    {
      let card_behind = cards.get_unchecked(back_index);
      back_indices_to_update.extend(card_behind.rgb_duplicates[rgb_index].iter());
    }

    let mut front_indices_to_update: Vec<usize> = Vec::with_capacity(5);
    front_indices_to_update.push(front_index);
    unsafe
    {
      let card_in_front = cards.get_unchecked(front_index);
      front_indices_to_update.extend(card_in_front.rgb_duplicates[rgb_index].iter());
    }

    unsafe
    {
      for index in back_indices_to_update
      {
        let card_behind = cards.get_unchecked_mut(index);
        card_behind.f_rgb_index[rgb_index] = new_index;
      }
    }
    unsafe
    {
      for index in front_indices_to_update
      {
        let card_in_front = cards.get_unchecked_mut(index);
        card_in_front.b_rgb_index[rgb_index] = new_index;
      }
    }
  }

}

fn compute(cards: &mut Vec<Card>) -> String
{
  let mut output: String = String::with_capacity(cards.len() * 10);
  if cards.len() == 1
  {
    output.push_str(&format!("{}\n", cards.first().unwrap().id));
    return output;
  }

  update_all_cards(cards);
  let mut cloned_cards: ClonedCards = get_cards_as_clone(cards);

  while cloned_cards.len() > 1
  {
    // debug_print_1(&cards, cloned_cards.len());
    sort_cards(&mut cloned_cards);
    let (_, card_to_sell) = cloned_cards.pop().unwrap();

    unsafe
    {
      output.push_str(&format!("{}\n", (*card_to_sell).id));

      if cloned_cards.len() == 1
      {
        output.push_str(&format!("{}\n", (*cloned_cards.get_unchecked(0).1).id));
        break;
      }

      (*card_to_sell).sold = true;
      for rgb_index in 0..3 as usize
      {
        (*card_to_sell).i_have_been_sold_update_cards(cards, rgb_index)
      }
    }
  }

  return output;
}

#[allow(dead_code)]
fn compute_slow(cards: &mut Vec<Card>) -> String
{
  let mut output: String = String::with_capacity(cards.len() * 10);
  if cards.len() == 1
  {
    output.push_str(&format!("{}\n", cards.first().unwrap().id));
    return output;
  }

  loop
  {
    update_all_cards(cards);
    let mut cloned_cards = get_cards_as_clone(cards);

    // debug_print_1(&cards, cloned_cards.len());
    
    sort_cards(&mut cloned_cards);
    let (index, card_to_sell) = cloned_cards.pop().unwrap();
    
    unsafe
    {
      {
        output.push_str(&format!("{}\n", (*card_to_sell).id));
        (*card_to_sell).sold = true;
  
        if cloned_cards.len() == 1
        {
          output.push_str(&format!("{}\n", (*cloned_cards.get_unchecked(0).1).id));
          break;
        }
      }
    }
    cards.remove(index);

  }
  return output;
}

#[allow(dead_code)]
fn debug_print_1(cards: &Vec<Card>, cloned_cards_len: usize)
{
  if !DEBUG { return; }

  println!("All cards [ ");
  for (_i, card) in cards.iter().enumerate()
  {
    println!("ID: {:>2} {:>2} Sold {:>6} uni {:>4} d? [{}{}{}] rgb[{:>3} {:>3} {:>3}] F[{:>3} {:>3} {:>3}] B[{:>3} {:>3} {:>3}]",
              card.id,
              _i,
              card.sold.to_string(),
              card.uniqueness,
              card.rgb_duplicates[0].len(),
              card.rgb_duplicates[1].len(),
              card.rgb_duplicates[2].len(),
              card.rgb[0], card.rgb[1], card.rgb[2],
              card.f_rgb_index[0], card.f_rgb_index[1], card.f_rgb_index[2],
              card.b_rgb_index[0], card.b_rgb_index[1], card.b_rgb_index[2]);
  }
  println!("] left? {}", cloned_cards_len);
}

type ClonedCards = Vec<(usize, *mut Card)>;

fn get_cards_as_clone(cards: &mut Vec<Card>) -> ClonedCards
{
  let cloned_cards = cards
    .iter_mut()
    .enumerate()
    .map(|(index, card)| (index, card as *mut Card))
    .collect::<ClonedCards>();

  return cloned_cards;
}

fn sort_cards(cards: &mut ClonedCards)
{
  cards.sort_by(|(_, c_1), (_, c_2)|
  {
    unsafe
    {
      if (**c_1).uniqueness < (**c_2).uniqueness { return std::cmp::Ordering::Greater }
      if (**c_1).uniqueness > (**c_2).uniqueness { return std::cmp::Ordering::Less }
  
      if (**c_1).id > (**c_2).id { return std::cmp::Ordering::Greater }
      if (**c_1).id < (**c_2).id { return std::cmp::Ordering::Less }
      }
    panic!("Keys are duplicate");
  });
}

/**
 * Update all cards with relevant values.
 */
fn update_all_cards(cards: &mut Vec<Card>)
{
  for index in 0..cards.len()
  {
    let card_to_edit = cards.get(index).unwrap();
    let (uniqueness, f_rgb_index, b_rgb_index, duplicates) = get_card_values(card_to_edit, cards);

    let card_to_edit = cards.get_mut(index).unwrap();
    card_to_edit.uniqueness = uniqueness;
    card_to_edit.f_rgb_index = f_rgb_index;
    card_to_edit.b_rgb_index = b_rgb_index;
    card_to_edit.rgb_duplicates = duplicates;
    card_to_edit.index = index;
  }
}

/**
 * Get uniqueness and indices of its nears cards in both directions.
 * 
 * @return - The uniqueness of that card,
 *           The rgb indices of the nearest cards forwards
 *           The rgb indices of the nearest cards backwards
 *           Indices of cards with duplicate rgb values 
 */
fn get_card_values(card: &Card, cards: &Vec<Card>) -> (u16, [usize; 3], [usize; 3], [HashSet<usize>; 3])
{
  let mut uniqueness: u16 = 0;
  let mut f_rgb_index: [usize; 3] = [usize::MAX; 3];
  let mut b_rgb_index: [usize; 3] = [usize::MAX; 3];
  let mut rgb_duplicates: [HashSet<usize>; 3] = [HashSet::<usize>::with_capacity(CAPACITY), HashSet::<usize>::with_capacity(CAPACITY), HashSet::<usize>::with_capacity(CAPACITY)];
  
  for rgb_index in 0..3 as usize
  {
    let (f_dist, f_index,  b_dist, b_index, new_duplicates) = 
      get_closest_two_cards(card, &cards, rgb_index);

    // assert!(f_dist <= 359);
    // assert!(b_dist <= 359);
    // assert!(f_index < cards.len());
    // assert!(b_index < cards.len());
    // assert!(_f_id <= 2_147_483_648); // 2^31
    // assert!(_b_id <= 2_147_483_648); // 2^31
    // assert!(f_dist + b_dist <= 360);

    f_rgb_index[rgb_index] = f_index;
    b_rgb_index[rgb_index] = b_index;

    if new_duplicates.is_empty()
    {
      uniqueness += f_dist + b_dist;
    }
    else
    {
      rgb_duplicates[rgb_index].extend(new_duplicates.iter())
    }
  }

  return (uniqueness, f_rgb_index, b_rgb_index, rgb_duplicates);
}

/**
 * Get the closest two cards in both directions from a card.
 * 
 * @ param[in]: Card - The card
 * @ param[in]: Cards - The deck of all cards
 * @ param[in]: rgb_index - What color are we looking at? 0 == r, 1 == green, 2 == blue
 * @ Return -> Forward card distance, forward card index,
 *             behind card distance, behind card index.
 *             Set with indices to card that have the same angles
 */
fn get_closest_two_cards(card: &Card, cards: &Vec<Card>, rgb_index: usize) -> (u16, usize, u16, usize, HashSet<usize>)
{
  let mut f_card_dist: u16 = u16::MAX;
  let mut b_card_dist: u16 = u16::MAX;
  
  let mut f_index: usize = usize::MAX;
  let mut b_index: usize = usize::MAX;

  let mut duplicates: HashSet<usize> = HashSet::with_capacity(CAPACITY);
  
  for (index, other) in cards.iter().enumerate()
  {
    if card.id == other.id { continue; }
    if other.sold { continue; }
    if card.rgb[rgb_index] == other.rgb[rgb_index]
    {
      duplicates.insert(index);
      continue;
    }

    let f_score = compute_distance_forward(card.rgb[rgb_index], other.rgb[rgb_index]);
    let b_score = compute_distance_backwards(card.rgb[rgb_index], other.rgb[rgb_index]);

    if f_score < f_card_dist
    {
      f_card_dist = f_score;
      f_index = index;
    }
    
    if b_score < b_card_dist
    {
      b_card_dist = b_score;
      b_index = index;
    }
  }

  // All cards looped over have been duplicates. Set values since they have not been set.
  if f_card_dist == u16::MAX
  {
    f_index = 0;
    b_index = 0;
    f_card_dist = 0;
    b_card_dist = 0;
  }

  return (f_card_dist, f_index, b_card_dist, b_index, duplicates);
}

fn compute_distance_forward(from: u16, to: u16) -> u16
{
  if to >= from
  {
    return to - from;
  }
  return 360 + to - from;
}

fn compute_distance_backwards(from: u16, to: u16) -> u16
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

    let input: Vec<u32> = line.trim()
                          .split_whitespace()
                          .map(|s| s.parse::<u32>().unwrap())
                          .collect();

    lines.push(Card::new(input[3], input[0] as u16, input[1] as u16, input[2] as u16));
  }

  return lines;
}

#[cfg(test)]
mod tests {
  use std::vec;
  use crate::{compute_distance_forward, compute_distance_backwards, Card, compute, compute_slow};
  use rand::Rng;

  #[test]
  fn test_compute_distance_forward()
  {
    for i in 0..360 as u16
    {
      for j in 0..360 as u16
      {
        assert!(compute_distance_forward(i, j) <= 359);
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
        assert!(compute_distance_backwards(i, j) <= 359);
      }
    }
  }

  #[test]
  fn input_1()
  {
    let mut cards: Vec<Card> = vec![
      Card::new(1, 42, 1, 1),
      Card::new(2, 90, 1, 1),
      Card::new(3, 110, 1, 1)
    ];

    let result = compute(&mut cards);
    assert!(result.eq("2\n3\n1\n"));
  }

  #[test]
  fn input_2()
  {
    let mut cards: Vec<Card> = vec![
      Card::new(0, 0, 0, 0),
      Card::new(120, 120, 120, 120),
      Card::new(240, 240, 240, 240),
      Card::new(2017, 120, 240, 0)
    ];

    let result = compute(&mut cards);
    assert!(result.eq("2017\n240\n120\n0\n"));
  }

  #[test]
  fn input_3()
  {
    let mut cards: Vec<Card> = vec![
      Card::new(32, 192, 78, 170),
      Card::new(33, 254, 9, 100),
      Card::new(34, 301, 165, 162),
      Card::new(35, 254, 9, 287),
      Card::new(36, 10, 294, 56),
      Card::new(37, 87, 241, 152)
    ];

    let result = compute(&mut cards);
    assert!(result == "33\n34\n37\n35\n36\n32\n");
  }

  #[test]
  fn input_4()
  {
    let mut cards: Vec<Card> = vec![
      Card::new(15, 200, 232, 355),
      Card::new(25, 300, 248, 207),
      Card::new(35, 0, 152, 276)
    ];

    let result = compute(&mut cards);
    assert!(result.eq("15\n35\n25\n"));
  }

  #[test]
  fn input_5()
  {
    let mut cards: Vec<Card> = vec![
      Card::new(103, 67, 274, 78),
      Card::new(104, 149, 130, 282),
      Card::new(106, 11, 98, 344),
      Card::new(107, 206, 130, 128),
      Card::new(108, 105, 172, 316)
    ];

    let result = compute(&mut cards);
    assert!(result == "108\n104\n103\n107\n106\n");
  }

  #[test]
  fn input_6()
  {
    let mut cards: Vec<Card> = vec![
      Card::new(3, 260, 126, 290),
      Card::new(5, 260, 187, 93),
      Card::new(7, 317, 331, 261),
      Card::new(9, 87, 30, 98),
      Card::new(11, 317, 127, 251),
      Card::new(13, 120, 57, 298)
    ];

    let result = compute(&mut cards);
    assert!(result == "3\n7\n13\n11\n9\n5\n");
  }

  #[test]
  fn no_crashes_1_card()
  {
    for i in 0..10000 as u32
    {
      let mut cards: Vec<Card> = vec![
        Card::new(1 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360))
      ];

      let mut cards_2 = cards.clone();
      let result = compute(&mut cards);
      let expected = compute_slow(&mut cards_2);

      assert!(result == expected);
    }
  }

  #[test]
  fn no_crashes_2_cards()
  {
    for i in 0..10000 as u32
    {
      let mut cards: Vec<Card> = vec![
        Card::new(1 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(2 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360))
      ];

      let mut cards_2 = cards.clone();
      let result = compute(&mut cards);
      let expected = compute_slow(&mut cards_2);

      assert!(result == expected);
    }
  }

  #[test]
  fn no_crashes_3_cards()
  {
    for i in 0..10000 as u32
    {
      let mut cards: Vec<Card> = vec![
        Card::new(10 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(20 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(30 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360))
      ];

      let mut cards_2 = cards.clone();
      let result = compute(&mut cards);
      let expected = compute_slow(&mut cards_2);

      assert!(result == expected);
    }
  }

  #[test]
  fn no_crashes_6_cards()
  {
    for i in 0..10000 as u32
    {
      let mut cards: Vec<Card> = vec![
        Card::new(1 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(2 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(3 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(4 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(5 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)),
        Card::new(6 + i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360))
      ];

      let mut cards_2 = cards.clone();
      let expected = compute_slow(&mut cards_2);
      let result = compute(&mut cards);
      
      assert!(result == expected);
    }
  }

  #[test]
  fn no_crashes_100_cards()
  {
    for _ in 0..100 as u32
    {
      let mut cards: Vec<Card> = Vec::with_capacity(100);
      for i in 0..101 as u32
      {
        cards.push(Card::new(i, rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360), rand::thread_rng().gen_range(0..360)));
      }

      let mut cards_2 = cards.clone();
      let expected = compute_slow(&mut cards_2);
      let result = compute(&mut cards);

      assert!(expected == result);
    }
  }
}