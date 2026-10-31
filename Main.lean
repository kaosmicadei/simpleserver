def getFile (filename : System.FilePath) : IO (Option String) := do
  let existFile ← filename.pathExists
  if not existFile then
    IO.eprintln s!"File not found: {filename}"
    pure none
  else
    let content ← IO.FS.readFile filename
    pure content

def main (args : List String) : IO Unit :=
  match args with
  | [] => IO.eprintln "usage: simpleserver [filename]"
  | filename :: _ => do
    let content ← getFile filename
    match content with
    | none => pure ()
    | some content => IO.println content
