import { existsSync, readdirSync, statSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { join, resolve, basename as base } from "node:path";
import { tmpdir } from "node:os";
import { execSync } from "node:child_process";

export const directoryExists = (path) => () => {
  try {
    return statSync(path).isDirectory();
  } catch {
    return false;
  }
};

export const listDirectory = (path) => () => {
  try {
    return readdirSync(path);
  } catch {
    return [];
  }
};

export const joinPath = (parts) => join(...parts);
export const absolute = (path) => resolve(path);
export const basename = (path) => base(path);
export const tmpDirectory = () => tmpdir();

// A throwaway workspace whose only job is to make the registry hand over
// a package's sources. `spago fetch` vendors them under .spago/p.
export const spagoFetch = (name) => (workspace) => () => {
  mkdirSync(join(workspace, "src"), { recursive: true });
  writeFileSync(
    join(workspace, "spago.yaml"),
    `workspace: {}\npackage:\n  name: specimen-site-fetch\n  dependencies:\n    - ${name}\n`,
  );
  writeFileSync(join(workspace, "src", "Main.purs"), "module Main where\n");
  execSync("spago fetch --quiet", { cwd: workspace, stdio: ["ignore", "inherit", "inherit"] });
};

export const removeDirectory = (path) => () => {
  rmSync(path, { recursive: true, force: true });
};

export const die = (message) => () => {
  console.error(message);
  process.exit(1);
};
