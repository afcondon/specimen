#!/usr/bin/env node
// Entry point for `npx purescript-specimen`.
//
// The real program is the PureScript bundle beside it, which runs on
// import; this exists only to carry the shebang, since the bundle is a
// build artifact and can't hold one.
import "../cli/specimen-site.js";
