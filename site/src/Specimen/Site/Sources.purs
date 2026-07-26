-- | Finding the `.purs` files a book is made of — either in a local
-- | workspace, or in a registry package vendored by `spago fetch` into a
-- | throwaway workspace under the temp directory.
-- |
-- | The decisions here are pure and live at the top of the module; the
-- | filesystem shows up only at the bottom. That split is deliberate:
-- | the cache logic is exactly the part that went wrong before, and it
-- | is now the part that can be tested without a filesystem.
module Specimen.Site.Sources
  ( Vendored
  , Cache(..)
  , plausiblePackageName
  , packageNameOfDirectory
  , findVendored
  , inspectCache
  , isSourceFile
  , skipDirectory
  , Package
  , resolvePackage
  , findPurs
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class.Console (log)

-- | A package as `spago fetch` leaves it: a directory named
-- | `<name>-<version>` under `.spago/p`.
type Vendored =
  { dir :: String
  , version :: String
  }

-- | What a look at the fetch cache found.
data Cache
  -- | Nothing vendored under this name — fetch it.
  = NotFetched
  -- | A vendored directory exists but carries no sources. This is what a
  -- | `spago fetch` that died partway leaves behind, and it is the case
  -- | the old check missed: it asked only whether the directory was
  -- | there, so a poisoned cache survived every later run and every book
  -- | built from it came out empty. Treated as a miss, and refetched.
  | Incomplete Vendored
  -- | Vendored, with sources.
  | Usable Vendored

derive instance Eq Cache

instance Show Cache where
  show = case _ of
    NotFetched -> "NotFetched"
    Incomplete v -> "Incomplete " <> v.dir
    Usable v -> "Usable " <> v.dir <> " (" <> v.version <> ")"

-- | The package name a directory stands for.
-- |
-- | Repositories are conventionally `purescript-foo` while the package
-- | inside them is `foo` — the registry drops the prefix, and so does
-- | this. Without it a book built from a checkout is captioned
-- | "PURESCRIPT · PURESCRIPT FOO", which is what pointing the CLI at a
-- | cloned repo does every time.
packageNameOfDirectory :: String -> String
packageNameOfDirectory dir =
  case String.stripPrefix (Pattern "purescript-") dir of
    Just rest | rest /= "" -> rest
    _ -> dir

-- | Registry package names are lowercase, digits and hyphens, opening on
-- | a letter. Anything else was meant to be a directory.
plausiblePackageName :: String -> Boolean
plausiblePackageName s =
  case SCU.uncons s of
    Just { head, tail } ->
      isLower head && Array.all packageChar (SCU.toCharArray tail)
    Nothing -> false
  where
  packageChar c = isLower c || isDigit c || c == '-'

isLower :: Char -> Boolean
isLower c = c >= 'a' && c <= 'z'

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

-- | Pick the entry that is this package at some version. The version
-- | must start with a digit, which is what keeps `aff` from matching
-- | `aff-promise`.
findVendored :: String -> Array String -> Maybe Vendored
findVendored name = Array.findMap match
  where
  match dir = do
    rest <- String.stripPrefix (Pattern (name <> "-")) dir
    first <- SCU.charAt 0 rest
    if isDigit first then Just { dir, version: rest } else Nothing

-- | Decide what the cache holds, given the entries under `.spago/p` and
-- | a way to count the sources inside a candidate. Pure, so the awkward
-- | case — vendored but empty — is testable without staging a broken
-- | download on disk.
inspectCache :: String -> Array String -> (Vendored -> Int) -> Cache
inspectCache name entries sourceCount =
  case findVendored name entries of
    Nothing -> NotFetched
    Just vendored
      | sourceCount vendored > 0 -> Usable vendored
      | otherwise -> Incomplete vendored

-- | A `.purs` file is a source; everything else in the tree is not.
isSourceFile :: String -> Boolean
isSourceFile = isJust <<< String.stripSuffix (Pattern ".purs")

-- | Directories the walk never descends into. `test` is skipped unless
-- | the caller asked for it — a book of a library is its library.
skipDirectory :: Boolean -> String -> Boolean
skipDirectory includeTests name =
  String.take 1 name == "."
    || name == "output"
    || name == "node_modules"
    || (not includeTests && name == "test")

-- ----------------------------------------------------------------------------
-- The filesystem
--
-- Everything above is a decision; everything below carries one out.

-- | A resolved target, ready to be harvested.
type Package =
  { name :: String
  , version :: String -- "local" for a workspace directory
  , files :: Array String
  , local :: Boolean
  }

-- | Resolve a target — a directory, or a registry package name — to the
-- | source files a book is built from.
-- |
-- | A registry package is vendored by `spago fetch` into a throwaway
-- | workspace under the temp directory and kept there between runs. The
-- | cache is only trusted when `inspectCache` says it carries sources;
-- | an incomplete vendored directory is refetched rather than believed.
resolvePackage :: { target :: String, includeTests :: Boolean } -> Effect Package
resolvePackage { target, includeTests } = do
  isDir <- directoryExists target
  if isDir then do
    let dir = absolute target
    files <- findPurs includeTests dir
    pure { name: packageNameOfDirectory (basename dir), version: "local", files, local: true }
  else do
    unless (plausiblePackageName target) $
      die (show target <> " is neither a directory nor a plausible registry package name")
    vendored <- fetchIfNeeded target includeTests
    files <- findPurs includeTests (joinPath [ packageStore target, vendored.dir, "src" ])
    pure { name: target, version: vendored.version, files, local: false }

-- | Look in the cache, and fetch when what's there can't be used.
-- | Deliberately willing to fetch twice rather than trust once: a
-- | refetch costs seconds, where a poisoned cache silently produces an
-- | empty book every time.
fetchIfNeeded :: String -> Boolean -> Effect Vendored
fetchIfNeeded name includeTests = do
  cache <- readCache name includeTests
  case cache of
    Usable vendored -> pure vendored
    NotFetched -> refetch "fetching" Nothing
    Incomplete vendored ->
      refetch ("re-fetching (cached " <> vendored.dir <> " has no sources)") (Just vendored)
  where
  refetch why stale = do
    log ("  " <> why <> " " <> name <> " from the registry…")
    -- An empty vendored directory has to go before the refetch, not
    -- after: spago sees its own lockfile, considers the package already
    -- present, and no-ops straight over it. Clearing the directory is
    -- what makes the retry actually retry.
    for_ stale \vendored ->
      removeDirectory (joinPath [ packageStore name, vendored.dir ])
    spagoFetch name (workspace name)
    cache <- readCache name includeTests
    case cache of
      Usable vendored -> pure vendored
      Incomplete vendored ->
        die ("spago fetch left " <> vendored.dir <> " with no sources under src/")
      NotFetched ->
        die ("spago fetch ran but " <> name <> " is not under " <> packageStore name)

readCache :: String -> Boolean -> Effect Cache
readCache name includeTests = do
  let store = packageStore name
  present <- directoryExists store
  if not present then pure NotFetched
  else do
    entries <- listDirectory store
    case findVendored name entries of
      Nothing -> pure NotFetched
      Just vendored -> do
        sources <- findPurs includeTests (joinPath [ store, vendored.dir, "src" ])
        pure (inspectCache name entries (const (Array.length sources)))

-- | Every `.purs` file under a directory, skipping what `skipDirectory`
-- | says to skip.
findPurs :: Boolean -> String -> Effect (Array String)
findPurs includeTests root = do
  present <- directoryExists root
  if not present then pure [] else go root
  where
  go dir = do
    entries <- listDirectory dir
    Array.concat <$> traverse (visit dir) entries

  visit dir entry = do
    let path = joinPath [ dir, entry ]
    isDir <- directoryExists path
    if isDir then
      if skipDirectory includeTests entry then pure [] else go path
    else pure (if isSourceFile entry then [ path ] else [])

workspace :: String -> String
workspace name = joinPath [ tmpDirectory unit, "specimen-site-fetch", name ]

packageStore :: String -> String
packageStore name = joinPath [ workspace name, ".spago", "p" ]

foreign import directoryExists :: String -> Effect Boolean
foreign import listDirectory :: String -> Effect (Array String)
foreign import joinPath :: Array String -> String
foreign import absolute :: String -> String
foreign import basename :: String -> String
foreign import tmpDirectory :: Unit -> String
foreign import spagoFetch :: String -> String -> Effect Unit
foreign import removeDirectory :: String -> Effect Unit
foreign import die :: forall a. String -> Effect a
