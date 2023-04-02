use std::{io::{self, BufRead}, collections::{BTreeMap}};

const BLACK: &'static str = "Black";
const BLUE: &'static str = "Blue";
const CHOCOLATE: &'static str = "Chocolate";
const LILAC: &'static str = "Lilac";
const RED: &'static str = "Red";
const CREAM: &'static str = "Cream";
const BLACK_RED_TORTIE: &'static str = "Black-Red Tortie";
const BLUE_CREAM_TORTIE: &'static str = "Blue-Cream Tortie";
const CHOCOLATE_RED_TORTIE: &'static str = "Chocolate-Red Tortie";
const LILAC_CREAM_TORTIE: &'static str = "Lilac-Cream Tortie";

type GenePair = [char; 2];
struct Genes
{
  black: GenePair,
  red: GenePair,
  dilution: GenePair,
}

fn main() -> io::Result<()>
{
  let cats: Vec<Genes> = read_lines();
  reproduce(cats.get(0).unwrap(), cats.get(1).unwrap());

  Ok(())
}

fn reproduce(f_cat: &Genes, m_cat: &Genes)
{
  // Calculate gene permutations
  let b_genes: [GenePair; 16] = get_kitten_genes(&f_cat.black, &m_cat.black, 'B');
  let d_genes: [GenePair; 16] = get_kitten_genes(&f_cat.dilution, &m_cat.dilution, 'D');
  let o_genes: [GenePair; 16] = get_kitten_genes_red(&f_cat.red, &m_cat.red);

  // Concatinate the o_genes into two separate vectors, 
  let f_o_genes = [&o_genes[0..8], &o_genes[0..8]].concat();
  let m_o_genes = [&o_genes[8..16], &o_genes[8..16]].concat();

  
  // Calculate gene permutations
  let mut count: BTreeMap<&str, u32> = BTreeMap::from([
    (BLACK, 0),
    (BLUE, 0),
    (CHOCOLATE, 0),
    (LILAC, 0),
    (RED, 0),
    (CREAM, 0),
    (BLACK_RED_TORTIE, 0),
    (BLUE_CREAM_TORTIE, 0),
    (CHOCOLATE_RED_TORTIE, 0),
    (LILAC_CREAM_TORTIE, 0)
  ]);
  let mut female: bool = true;
  let mut o_temp_genes = &f_o_genes;

  for _ in 0..2
  {
    for b_gene in b_genes
    {
      for d_gene in d_genes
      {
        for o_gene in o_temp_genes
        {
          let colour: &str = compute_kitten_colour(female, &b_gene, &d_gene, o_gene);
          let colour_count: &mut u32= count.get_mut(colour).unwrap();
          *colour_count += 1;
        }
      }
    }
    female = false;
    o_temp_genes = &m_o_genes;
  }

  // Sort and print answer
  const NR_OF_ITEMS: u32 = (2 as u32).pow(13); // 8192
  let mut items: Vec<_> = count.iter()
                                .filter(|s| *s.1 > 0)
                                .collect();
  items.sort_by_key(|item| !item.1);

  for (k, v) in items
  {
    let score: f64 = (*v as f64 / NR_OF_ITEMS as f64).into();
    println!("{} {:.9}", k, score);
  }

}

/**
 * Get kittens potential genes based on the parent genes. This will be a permutation parents mixed genes => 16 combinations.
 * @param[in] f_genes - The parent female genes
 * @param[in] m_genes - The parent male genes
 * @param[in] variant - What variant of genes are we looking at? Black (B) or Dilution (D)
 * @return The gene permutations.
 */
fn get_kitten_genes(f_genes: &GenePair, m_genes: &GenePair, variant: char) -> [GenePair; 16]
{
  assert!(variant == 'B' || variant == 'D');
  let mut f_permutations: [char; 4] = ['-'; 4];
  let mut m_permutations: [char; 4] = ['-'; 4];

  permute_genes(f_genes[0], variant, 0, &mut f_permutations);
  permute_genes(f_genes[1], variant, 2, &mut f_permutations);
  permute_genes(m_genes[0], variant, 0, &mut m_permutations);
  permute_genes(m_genes[1], variant, 2, &mut m_permutations);
  
  let mut kitten_permutations: [GenePair; 16] = [['-'; 2]; 16];

  let mut i: usize = 0;
  for f_gene in f_permutations
  {
    for m_gene in m_permutations
    {
      kitten_permutations[i] = [f_gene, m_gene];
      i += 1;
    }
  }
  return kitten_permutations;
}

/**
 * Permute a gene into two, outputting it into a two places of an array.
 * @param[in] gene - The gene we want to permutate
 * @param[in] variant - What variant of genes are we looking at? Black (B) or Dilution (D)
 * @param[in] gene_index - Where in the array do we input. Valid 
 * @param[out] genes - Array where gene permutation is put.
 * 
 * Examples
 * permute_genes(-, D, 0, [-, -, -, -]) -> [d, D, 0, 0]
 * permute_genes(B, B, 2, [b, b, -, -]) -> [b, b, B, B]
 */
fn permute_genes(gene: char, variant: char, gene_index: usize, genes: &mut [char; 4])
{
  assert!(gene_index == 0 || gene_index == 2);
  assert!(variant == 'B' || variant == 'D');
  match gene
  {
    '-' =>
    {
      genes[gene_index] = variant.to_ascii_lowercase();
      genes[gene_index + 1] = variant.to_ascii_uppercase();
    }
    c =>
    {
      genes[gene_index] = c;
      genes[gene_index + 1] = c;
    }
  }
}

/**
 * Does the same thing as get_kitten_genes() but applies special permutation logic to the red genes.
 * @param[in] f_genes - The parent female genes
 * @param[in] m_genes - The parent male genes
 * @param[in] variant - What variant of genes are we looking at? Black (B) or Dilution (D)
 * @return The gene permutations. First half will be female genes, next half will be male genes.
 */
fn get_kitten_genes_red(f_genes: &[char; 2], m_genes: &[char; 2]) -> [GenePair; 16]
{
  let f_permutations: [char; 4] = create_red_f_permutations(f_genes);
  let m_permutations: [char; 2] = create_red_m_permutations(m_genes[0]);

  let mut kitten_permutations: [GenePair; 16] = [['-'; 2]; 16];

  // "Loop" over female gene permutations
  kitten_permutations[0] = [f_permutations[0], m_permutations[0]];
  kitten_permutations[1] = [f_permutations[0], m_permutations[1]];
  kitten_permutations[2] = [f_permutations[1], m_permutations[0]];
  kitten_permutations[3] = [f_permutations[1], m_permutations[1]];
  kitten_permutations[4] = [f_permutations[2], m_permutations[0]];
  kitten_permutations[5] = [f_permutations[2], m_permutations[1]];
  kitten_permutations[6] = [f_permutations[3], m_permutations[0]];
  kitten_permutations[7] = [f_permutations[3], m_permutations[1]];

  // "Loop" over male gene permutations
  kitten_permutations[8]  = [f_permutations[0], '-'];
  kitten_permutations[9]  = [f_permutations[0], '-'];
  kitten_permutations[10] = [f_permutations[1], '-'];
  kitten_permutations[11] = [f_permutations[1], '-'];
  kitten_permutations[12] = [f_permutations[2], '-'];
  kitten_permutations[13] = [f_permutations[2], '-'];
  kitten_permutations[14] = [f_permutations[3], '-'];
  kitten_permutations[15] = [f_permutations[3], '-'];

  return kitten_permutations;
}

/**
 * From a pair of female red genes, create all possible permutations.
 * @param[in] f_genes - The parent female genes.
 * return The gene permutations.
 */
fn create_red_f_permutations(f_genes: &GenePair) -> [char; 4]
{
  let mut permutations: [char; 4] = ['-'; 4];
  for i in 0..2 as usize
  {
    match f_genes[i]
    {
      '-' =>
      {
        permutations[i * 2] = 'o';
        permutations[i * 2 + 1] = 'O';
      }
      c =>
      {
        permutations[i * 2] = c;
        permutations[i * 2 + 1] = c;
      }
    }
  }
  return permutations;
}

/**
 * From a male red gene, create all possible permutations.
 * @param[in] m_gene - The parent male gene.
 * return The gene permutations.
 */
fn create_red_m_permutations(m_gene: char) -> [char; 2]
{
  let mut permutations: [char; 2] = ['-'; 2];
  match m_gene
  {
    '-' =>
    {
      permutations[0] = 'o';
      permutations[1] = 'O';
    }
    c =>
    {
      permutations[0] = c;
      permutations[1] = c;
    }
  }
  return permutations;
}

/**
 * From kitten gender and genes, compute what kind of coulour that kitten will have.
 */
fn compute_kitten_colour(female: bool, black: &[char; 2], dilution: &[char; 2], red: &[char; 2]) -> &'static str
{
  let nr_of_black_dominant_genes: u8 = nr_of_dominant_genes(black);
  let nr_of_dilution_dominant_genes: u8 = nr_of_dominant_genes(dilution);
  let nr_of_red_dominant_genes: u8 = nr_of_dominant_genes(red);

  if nr_of_red_dominant_genes > 0
  {
    if !female || nr_of_red_dominant_genes == 2
    {
      if nr_of_dilution_dominant_genes == 0
      {
        return CREAM;
      }
      return RED
    }
    
    // Female cat with one red dominant gene
    if nr_of_black_dominant_genes > 0 && nr_of_dilution_dominant_genes > 0 { return BLACK_RED_TORTIE; }
    if nr_of_black_dominant_genes > 0 && nr_of_dilution_dominant_genes == 0 { return BLUE_CREAM_TORTIE; }
    if nr_of_black_dominant_genes == 0 && nr_of_dilution_dominant_genes > 0 { return CHOCOLATE_RED_TORTIE; }
    return LILAC_CREAM_TORTIE;
  }

  if nr_of_black_dominant_genes > 0 && nr_of_dilution_dominant_genes > 0 { return BLACK; }
  if nr_of_black_dominant_genes > 0 && nr_of_dilution_dominant_genes == 0 { return BLUE; }
  if nr_of_black_dominant_genes == 0 && nr_of_dilution_dominant_genes > 0 { return CHOCOLATE; }
  return LILAC;
}

fn nr_of_dominant_genes(genes: &[char; 2]) -> u8
{
  let mut nr_of_dominant_genes: u8 = 0;
  if genes[0].is_uppercase() { nr_of_dominant_genes += 1 }
  if genes[1].is_uppercase() { nr_of_dominant_genes += 1 }
  return nr_of_dominant_genes;
}

fn read_lines() -> Vec<Genes>
{
  const NR_OF_LINES: usize = 2;
  let mut lines: Vec<Genes> = Vec::with_capacity(NR_OF_LINES);

  let mut stdin = io::stdin().lock();
  let mut buf = String::new();
  for _ in 0..2
  {
    _ = stdin.read_line(&mut buf);
    match buf.trim()
    {
      BLACK =>                lines.push(Genes{ black: ['B', '-'], dilution: ['D', '-'],  red: ['o', 'o'] }),
      BLUE =>                 lines.push(Genes{ black: ['B', '-'], dilution: ['d', 'd'],  red: ['o', 'o'] }),
      CHOCOLATE =>            lines.push(Genes{ black: ['b', 'b'], dilution: ['D', '-'],  red: ['o', 'o'] }),
      LILAC =>                lines.push(Genes{ black: ['b', 'b'], dilution: ['d', 'd'],  red: ['o', 'o'] }),
      RED =>                  lines.push(Genes{ black: ['-', '-'], dilution: ['D', '-'],  red: ['O', 'O'] }),
      CREAM =>                lines.push(Genes{ black: ['-', '-'], dilution: ['d', 'd'],  red: ['O', 'O'] }),
      BLACK_RED_TORTIE =>     lines.push(Genes{ black: ['B', '-'], dilution: ['D', '-'],  red: ['O', 'o'] }),
      BLUE_CREAM_TORTIE =>    lines.push(Genes{ black: ['B', '-'], dilution: ['d', 'd'],  red: ['O', 'o'] }),
      CHOCOLATE_RED_TORTIE => lines.push(Genes{ black: ['b', 'b'], dilution: ['D', '-'],  red: ['O', 'o'] }),
      LILAC_CREAM_TORTIE =>   lines.push(Genes{ black: ['b', 'b'], dilution: ['d', 'd'],  red: ['O', 'o'] }),
      _ => panic!("Invalid input")
    }
    buf.clear();
  }

  return lines;
}