// Demo snippets for the language-help popover — one markdown code fence per
// canonical language. Clicking a language in the (i) popover appends its
// snippet to the Run Code editor.
//
// Most come from the canonical "hello + math check" pattern in
// ephemeral_test_suite.md; the rest are minimal examples written to match
// the actual invocation in ephemeral_core/config.py LANG_MAP (e.g. actionlint
// receives a workflow YAML, latex/pandoc write artifacts to /output).
// Hand-written on purpose — there is no single source of truth for snippets.
const fence = (lang, body) => "```" + lang + "\n" + body + "\n```";

export const LANG_SNIPPETS = {
  "05ab1e": fence(
    "05ab1e",
    '"Markdown: 05ab1e | Math Check: " 1 1 + " - OK" J'
  ),
  actionlint: fence(
    "actionlint",
    [
      "name: demo",
      "on: push",
      "jobs:",
      "  build:",
      "    runs-on: ubuntu-latest",
      "    steps:",
      '      - run: echo "actionlint: workflow is valid"',
    ].join("\n")
  ),
  bash: fence(
    "bash",
    'echo "bash $BASH_VERSION | Math Check: $((7 + 10)) - OK"'
  ),
  brainfuck: fence(
    "brainfuck",
    // The canonical Wikipedia "Hello World!" (verified end-to-end; the
    // earlier example contained stray '?' chars — invalid brainfuck).
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.\n" +
      ">>.<-.<.+++.------.--------.>>+.>++."
  ),
  c: fence(
    "c",
    [
      "#include <stdio.h>",
      "int main() { printf(\"Markdown: C (GCC) | Math Check: %d - OK\\n\", 11 + 9); return 0; }",
    ].join("\n")
  ),
  cjam: fence(
    "cjam",
    '"Markdown: CJam | Math Check: " 1 2 + " - OK"'
  ),
  clojure: fence(
    "clojure",
    '(println (str "Markdown: Clojure " (clojure-version) " | Math Check: " (+ 28 2) " - OK"))'
  ),
  cobol: fence(
    "cobol",
    [
      "       IDENTIFICATION DIVISION.",
      "       PROGRAM-ID. DEMO.",
      "       PROCEDURE DIVISION.",
      '           DISPLAY "cobol: hello from gnucobol".',
      "           STOP RUN.",
    ].join("\n")
  ),
  cpp: fence(
    "cpp",
    [
      "#include <iostream>",
      'int main() { std::cout << "Markdown: C++ (G++) | Math Check: " << 12 + 12 << " - OK" << std::endl; return 0; }',
    ].join("\n")
  ),
  crystal: fence(
    "crystal",
    'puts "Markdown: Crystal #{Crystal::VERSION} | Math Check: #{19 + 1} - OK"'
  ),
  elixir: fence(
    "elixir",
    'IO.puts "Markdown: Elixir #{System.version} | Math Check: #{29 + 11} - OK"'
  ),
  fortran: fence(
    "fortran",
    [
      "program test",
      '  print *, "Markdown: Fortran (GFortran) | Math Check: ", 15 + 15, " - OK"',
      "end program test",
    ].join("\n")
  ),
  "gh-runner": fence(
    "gh-runner",
    [
      "# bash inside a GitHub Actions ubuntu-22.04 runner image (catthehacker)",
      'echo "gh-runner: job on ubuntu-22.04 | Math Check: $((7 + 10)) - OK"',
    ].join("\n")
  ),
  go: fence(
    "go",
    [
      'package main',
      'import ("fmt"; "runtime")',
      "func main() {",
      '    fmt.Printf("Markdown: Go %s | Math Check: %d - OK\\n", runtime.Version(), 14 + 6)',
      "}",
    ].join("\n")
  ),
  golfscript: fence(
    "golfscript",
    '"Markdown: GolfScript | Math Check: " 3 4 + " - OK"'
  ),
  haskell: fence(
    "haskell",
    'main = putStrLn $ "Markdown: Haskell | Math Check: " ++ show (26 + 4) ++ " - OK"'
  ),
  java: fence(
    "java",
    [
      "public class Main {",
      "    public static void main(String[] args) {",
      '        System.out.println("Markdown: Java " + System.getProperty("java.version") + " | Math Check: " + (18 + 18) + " - OK");',
      "    }",
      "}",
    ].join("\n")
  ),
  julia: fence(
    "julia",
    'println("Markdown: Julia $VERSION | Math Check: $(23 + 7) - OK")'
  ),
  latex: fence(
    "latex",
    [
      "\\documentclass{article}",
      "\\begin{document}",
      "Hello from \\LaTeX{} --- compiled to a PDF artifact in /output.",
      "\\end{document}",
    ].join("\n")
  ),
  lisp: fence(
    "lisp",
    '(format t "Markdown: Common Lisp (SBCL) | Math Check: ~d - OK~%" (+ 27 3))'
  ),
  lolcode: fence(
    "lolcode",
    ['HAI 1.2', 'VISIBLE "Markdown: Lolcode | Math Check: 14 - OK"', "KTHXBYE"].join("\n")
  ),
  lua: fence(
    "lua",
    'print("Markdown: Lua " .. _VERSION .. " | Math Check: " .. (8 + 8) .. " - OK")'
  ),
  nim: fence(
    "nim",
    'echo "Markdown: Nim ", NimVersion, " | Math Check: ", 20 + 5, " - OK"'
  ),
  node: fence(
    "node",
    "console.log(`Markdown: Node ${process.version} | Math Check: ${5 + 8} - OK`);"
  ),
  ocaml: fence(
    "ocaml",
    'Printf.printf "Markdown: OCaml | Math Check: %d - OK\\n" (30 + 10);;'
  ),
  octave: fence(
    "octave",
    'printf("Markdown: Octave %s | Math Check: %d - OK\\n", version(), 24 + 6);'
  ),
  pandoc: fence(
    "pandoc",
    ["# Pandoc demo", "", "Markdown **converted to a PDF** artifact in `/output`.", ""].join("\n")
  ),
  "pandoc-docx": fence(
    "pandoc-docx",
    ["# Pandoc demo", "", "Markdown **converted to a DOCX** artifact in `/output`.", ""].join("\n")
  ),
  "pandoc-pdf": fence(
    "pandoc-pdf",
    ["# Pandoc demo", "", "Markdown **converted to a PDF** artifact in `/output`.", ""].join("\n")
  ),
  perl: fence(
    "perl",
    'printf "Markdown: Perl v%vd | Math Check: %d - OK\\n", $^V, 9 + 5;'
  ),
  php: fence(
    "php",
    [
      "<?php",
      'echo "Markdown: PHP " . phpversion() . " | Math Check: " . (10 + 20) . " - OK";',
      "?>",
    ].join("\n")
  ),
  piet: fence(
    "piet",
    [
      "# piet programs are images, not text — the esolang/piet image",
      "# expects a drawn program, so this block is a placeholder.",
    ].join("\n")
  ),
  prolog: fence(
    "prolog",
    [
      ":- initialization(main).",
      "main :- Res is 31 + 9, write('Markdown: Prolog (SWI) | Math Check: '), write(Res), write(' - OK'), nl, halt.",
    ].join("\n")
  ),
  pwsh: fence(
    "pwsh",
    'Write-Output "Markdown: PowerShell | Math Check: $(36 + 4) - OK"'
  ),
  python: fence(
    "python",
    [
      "import sys",
      'print(f"Markdown: Python {sys.version.split()[0]} | Math Check: {12 + 30} - OK")',
    ].join("\n")
  ),
  pywine: fence(
    "pywine",
    [
      "# python + wine in one image (tobix/pywine)",
      "python --version",
      'echo "pywine: python & wine co-exist in one container"',
    ].join("\n")
  ),
  r: fence(
    "r",
    'cat(sprintf("Markdown: R %s | Math Check: %d - OK\\n", R.version.string, 22 + 8))'
  ),
  ruby: fence(
    "ruby",
    'puts "Markdown: Ruby #{RUBY_VERSION} | Math Check: #{6 + 7} - OK"'
  ),
  rust: fence(
    "rust",
    [
      "fn main() {",
      '    println!("Markdown: Rust | Math Check: {} - OK", 13 + 4);',
      "}",
    ].join("\n")
  ),
  science: fence(
    "science",
    [
      "import matplotlib",
      'matplotlib.use("Agg")',
      "import matplotlib.pyplot as plt",
      "import numpy as np",
      "",
      "x = np.linspace(0, 2 * np.pi, 100)",
      "fig, ax = plt.subplots(figsize=(6, 3.5))",
      'ax.plot(x, np.sin(x), color="#4a9eff")',
      'ax.set_title("science image demo")',
      'plt.savefig("/output/chart.png", dpi=110, bbox_inches="tight")',
      'print("chart saved to /output/chart.png")',
    ].join("\n")
  ),
  tiddlywiki: fence(
    "tiddlywiki",
    [
      "# the elasticdog/tiddlywiki image builds a single-file wiki from a folder",
      "tiddlywiki --version",
      'echo "tiddlywiki: hello from the build image"',
    ].join("\n")
  ),
  verilog: fence(
    "verilog",
    [
      "module test;",
      "  initial begin",
      '    $display("Markdown: Verilog (Icarus) | Math Check: %d - OK", 25 + 5);',
      "    $finish;",
      "  end",
      "endmodule",
    ].join("\n")
  ),
};
