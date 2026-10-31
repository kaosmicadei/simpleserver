import Std.Http

open Std Async Http Server

/--
Get the content of a file if it exists, otherwise return `none`.
-/
def getFile (filename : System.FilePath) : IO (Option String) := do
  if not (← filename.pathExists) then
    IO.eprintln s!"File not found: {filename}"
    pure none
  else
    let content ← IO.FS.readFile filename
    pure (some content)

/--
List the contents of a directory, returning an empty array if the path is not a
directory.
-/
def getDirectoryContent (dirpath : System.FilePath) : IO (Array String) := do
  if not (← dirpath.isDir) then
    IO.eprintln s!"{dirpath} is not a directory"
    pure #[]
  else
    let files ← dirpath.readDir
    -- `.isDir` is a IO operation, so we use `mapM` instead of `map` to propagate
    -- the monadic context.
    files.mapM fun entry => do
      if (← entry.path.isDir) then
        pure $ s!"{entry.fileName}/"
      else
        pure entry.fileName

/--
Create an HTML link for a given entry.
-/
def makeHtmlLink (entry : String) : String := s!"<a href=\"{entry}\">{entry}</a>"

/--
Create an HTML list from a list of entries.
-/
def makeHtmlList (entries : List String) : String :=
  let wrapper := fun s : String => s!"<li>{s}</li>"
  let content := "".intercalate (entries.map wrapper)
  s!"<ul>{content}</ul>"

/--
Format a directory listing as an HTML page.
-/
def formatDirectoryListing (entries : Array String) : String :=
  let list := makeHtmlList $ (entries.map makeHtmlLink).toList
  s!"<html><body>{list}</body></html>"

structure Api

/--
Get the index file if it exists, otherwise return a directory listing of the
current directory.
-/
def getIndexOrDirectory : IO String := do
  let index ← getFile ⟨"index.html"⟩
  match index with
  | some content => pure content
  | none => getDirectoryContent ⟨"."⟩ >>= pure ∘ formatDirectoryListing

/--
Handle GET requests for the API.
-/
def Api.get (req : Request Body.Stream) : Async (Response Body.Full) := do
  match req.line.uri with
  | .originForm path _ =>
    match path.segments with
    | #[] => Response.ok |>.html (← getIndexOrDirectory)
    | _ => do
      let asPath : System.FilePath := ⟨s!".{path}"⟩
      if (← asPath.isDir) then
        let content ← getDirectoryContent asPath
        Response.ok |>.html (formatDirectoryListing content)
      else
        let fileContent ← getFile asPath
        match fileContent with
        | none => Response.notFound |>.text "Not Found"
        | some content => Response.ok |>.text content
  | _ => Response.notFound |>.text "Not Found"

instance : Handler Api where
  onRequest _ req := do
    IO.eprintln s!"{req.line.method} {req.line.uri} {req.line.version}"
    match req.line.method with
    | .get => Api.get req
    | _ => Response.badRequest |>.text "Bad Request"

/--
Start the server on the given port.
-/
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
