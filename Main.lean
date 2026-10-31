def getFile (filename : System.FilePath) : IO (Option String) := do
  let existFile ← filename.pathExists
  if not existFile then
    IO.eprintln s!"File not found: {filename}"
    pure none
  else
    let content ← IO.FS.readFile filename
    pure content

def getDirectoryContent (dirpath : System.FilePath) : IO (Array String) := do
  if not ((← dirpath.pathExists) || (← dirpath.isDir)) then
    IO.eprintln s!"{dirpath} is not a directory"
    pure #[]
  else
    let files ← dirpath.readDir
    -- `.isDir` is a IO operation, so we use `mapM` instead of `map` to propagate
    -- the monadic context.
    files.mapM fun entry => do
      if (← entry.path.isDir) then
        pure $ entry.fileName ++ "/"
      else
        pure entry.fileName

def makeHtmlLink (entry : String) : String := s!"<a href=\"{entry}\">{entry}</a>"

def makeHtmlList (entries : List String) : String :=
  let wrapper := fun s : String => s!"<li>{s}</li>"
  let content := "".intercalate (entries.map wrapper)
  s!"<ul>{content}</ul>"

def main (args : List String) : IO Unit :=
  match args with
  | [] => IO.eprintln "usage: simpleserver [dir]"
  | dirname :: _ => do
    let files ← getDirectoryContent ⟨dirname⟩
    IO.println ∘ makeHtmlList $ (files.map makeHtmlLink).toList
