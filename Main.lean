import Std.Http

open Std Async Http Server

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

structure Api

instance : Handler Api where
  onRequest _ req := do
    IO.eprintln s!"{req.line.method} {req.line.uri} {req.line.version}"
    Response.ok |>.text "Hello, Lean"

def startServer (port : UInt16) : IO Unit := Async.block do
  let address : Net.SocketAddress := .v4 ⟨.ofParts 127 0 0 1, port⟩
  let server ← Server.serve address Api.mk
  IO.eprintln s!"Server running on {address}"
  server.waitShutdown

def main (args : List String) : IO Unit :=
  match args with
  | [] => IO.eprintln "usage: simpleserver [port]"
  | port :: _ => do
    let p := port.toNat?
    match p with
    | none => IO.eprintln s!"Invalid port: {port}, port must be a positive integer."
    | some n => startServer $ UInt16.ofNat n
