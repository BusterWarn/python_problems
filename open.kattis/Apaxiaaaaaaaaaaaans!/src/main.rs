use std::io;

const DEBUG: bool = false;

fn main() -> io::Result<()>
{
  let mut input: String = String::new();
  let stdin: io::Stdin = io::stdin(); // We get `Stdin` here.
  match stdin.read_line(&mut input)
  {
    Ok(n) =>
    {
      if DEBUG
      {
        println!("Read {n} bytes, input {} ", input);
      }
    },
    Err(error) =>
    {
      println!("Error {}", error.to_string());
      return Err(error);
    }
  }

  let mut output: String = String::from("");

  for c in input.chars()
  {
    if !output.ends_with(c)
    {
      output.push(c);
    }
  }

  println!("{output}");

  Ok(())
}
