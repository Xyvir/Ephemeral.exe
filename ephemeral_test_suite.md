# Ephemeral Master Test Suite

## PART 1: FENCED CODE BLOCKS

````
```05ab1e
"Markdown: 05ab1e | Math Check: " 1 1 + " - OK" J
```
````


````
```bash
# Active check: Bash version + Math (7 + 10)
echo "Markdown: Bash $BASH_VERSION | Math Check: $((7 + 10)) - OK"
```
````


```````bf
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.
>>.<-.<.+++.------.--------.>>+.>++.
```
````


````
```c
#include <stdio.h>
// Active check: C + Math (11 + 9)
int main() { printf("Markdown: C (GCC) | Math Check: %d - OK\n", 11 + 9); return 0; }
```
````

````
```cjam
"Markdown: CJam | Math Check: " 1 2 + " - OK"
```
````

````
```clojure
;; Active check: Clojure version + Math (28 + 2)
(println (str "Markdown: Clojure " (clojure-version) " | Math Check: " (+ 28 2) " - OK"))
```
````


````
```cpp
#include <iostream>
// Active check: C++ + Math (12 + 12)
int main() { std::cout << "Markdown: C++ (G++) | Math Check: " << 12 + 12 << " - OK" << std::endl; return 0; }
```
````

````
```crystal
# Active check: Crystal version + Math (19 + 1)
puts "Markdown: Crystal #{Crystal::VERSION} | Math Check: #{19 + 1} - OK"
```
````

````
```elixir
# Active check: Elixir version + Math (29 + 11)
IO.puts "Markdown: Elixir #{System.version} | Math Check: #{29 + 11} - OK"
```
````


````
```fortran
program test
  ! Active check: Fortran + Math (15 + 15)
  print *, "Markdown: Fortran (GFortran) | Math Check: ", 15 + 15, " - OK"
end program test
```
````

````
```freebasic
' Active check: FreeBASIC + Math (33 + 7)
Print "Markdown: FreeBASIC " & __FB_VERSION__ & " | Math Check: " & (33 + 7) & " - OK"
```
````

````
```go
package main
import ("fmt"; "runtime")
func main() {
    // Active check: Go version + Math (14 + 6)
    fmt.Printf("Markdown: Go %s | Math Check: %d - OK\n", runtime.Version(), 14 + 6)
}
```
````

````
```golfscript
"Markdown: GolfScript | Math Check: " 3 4 + " - OK"
```
````

````
```haskell
-- Active check: Haskell + Math (26 + 4)
main = putStrLn $ "Markdown: Haskell | Math Check: " ++ show (26 + 4) ++ " - OK"
```
````

````
```java
public class Main {
    public static void main(String[] args) {
        // Active check: Java + Math (18 + 18)
        System.out.println("Markdown: Java " + System.getProperty("java.version") + " | Math Check: " + (18 + 18) + " - OK");
    }
}
```
````


````
```julia
# Active check: Julia version + Math (23 + 7)
println("Markdown: Julia $VERSION | Math Check: $(23 + 7) - OK")
```
````

````
```lisp
;; Active check: Common Lisp + Math (27 + 3)
(format t "Markdown: Common Lisp (SBCL) | Math Check: ~d - OK~%" (+ 27 3))
```
````

````
```lolcode
HAI 1.2
VISIBLE "Markdown: Lolcode | Math Check: 14 - OK"
KTHXBYE
```
````

````
```lua
-- Active check: Lua version + Math (8 + 8)
print("Markdown: Lua " .. _VERSION .. " | Math Check: " .. (8 + 8) .. " - OK")
```
````



````
```nim
# Active check: Nim version + Math (20 + 5)
echo "Markdown: Nim ", NimVersion, " | Math Check: ", 20 + 5, " - OK"
```
````

````
```node
// Active check: Node version + Math (5 + 8)
console.log(`Markdown: Node ${process.version} | Math Check: ${5 + 8} - OK`);
```
````

````
```ocaml
(* Active check: OCaml + Math (30 + 10) *)
Printf.printf "Markdown: OCaml | Math Check: %d - OK\n" (30 + 10);;
```
````

````
```octave
# Active check: Octave version + Math (24 + 6)
printf("Markdown: Octave %s | Math Check: %d - OK\n", version(), 24 + 6);
```
````

````
```perl
# Active check: Perl version + Math (9 + 5)
printf "Markdown: Perl v%vd | Math Check: %d - OK\n", $^V, 9 + 5;
```
````

````
```php
<?php
// Active check: PHP version + Math (10 + 20)
echo "Markdown: PHP " . phpversion() . " | Math Check: " . (10 + 20) . " - OK";
?>
```
````

````
```pwsh
# Active check: PowerShell + Math (36 + 4)
Write-Output "Markdown: PowerShell | Math Check: $(36 + 4) - OK"
```
````

````
```prolog
% Active check: Prolog + Math (31 + 9)
:- initialization(main).
main :- Res is 31 + 9, write('Markdown: Prolog (SWI) | Math Check: '), write(Res), write(' - OK'), nl, halt.
```
````

````
```python
import sys
# Active check: Python version + Math (12 + 30)
print(f"Markdown: Python {sys.version.split()[0]} | Math Check: {12 + 30} - OK")
```
````

````
```r
# Active check: R version + Math (22 + 8)
cat(sprintf("Markdown: R %s | Math Check: %d - OK\n", R.version.string, 22 + 8))
```
````

````
```ruby
# Active check: Ruby version + Math (6 + 7)
puts "Markdown: Ruby #{RUBY_VERSION} | Math Check: #{6 + 7} - OK"
```
````

````
```rust
fn main() {
    // Active check: Rust + Math (13 + 4)
    println!("Markdown: Rust | Math Check: {} - OK", 13 + 4);
}
```
````

````
```science
import sys
import numpy as np
# Active check: Anaconda Python + Math (21 + 9)
print(f"Markdown: Science (Anaconda) | Math Check: {21 + 9} - OK")
```
````


````
```verilog
module test;
  initial begin
    // Active check: Verilog + Math (25 + 5)
    $display("Markdown: Verilog (Icarus) | Math Check: %d - OK", 25 + 5);
    $finish;
  end
endmodule
```
````

````
```code.png b64
iVBORw0KGgoAAAANSUhEUgAAAFcAAAAXCAYAAAB+kNMAAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB9sDChMZB4c0/WoAAAAZdEVYdENvbW1lbnQAQ3JlYXRlZCB3aXRoIEdJTVBXgQ4XAAAA7UlEQVRYw+2Y0Q6DMAhFL8b/op8OX8YedGatdUuMzZi794VCTKQHSrQCWKCRmaLAsYurorh04gZ42cVVDS6duFnvcagppHTe+2v5FCAiMIG6TEXqQs9Eck6hWvkiAtizhRefcK+CHbEAfvEJ92LAr+LMHagpX/W1a7+dz5H/TnPO45UD7BmgqTpXkwAcoTkD1G3tnLkU4RIu4VIn4Wb7rrxd5xIsxwLhDvtLWq/4Wku4gwAT7h9r/tQl4r6t/cYgunccPrhze0cwNCq7Jbj6qpEcZFQ2zVhowUYDdGcjqs1cvaHQqHI5KvgR2JGAH5UkmGgNBgvjAAAAAElFTkSuQmCC
```
```piet
# the program is the seeded image above (code.png)
```
````


## PART 2: SHEBANG SYNTAX TESTS
(Instructions: Copy ONLY the code inside the block, not the backticks.)

```text
#!05ab1e
"Shebang: 05ab1e | Math: " 1 1 + " - OK" J
```

```text
#!bash
echo "Shebang: Bash $BASH_VERSION | Math: $((7 + 10)) - OK"
```

```text
#!bf
+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++.>>.++.<<<.>>+.>>+.<.<<.>>+.>.<.
<<.>>-.>>.+++.<<.>>-.<<<.
```

```text
#!c
#include <stdio.h>
int main() { printf("Shebang: C | Math: %d - OK\n", 11 + 9); return 0; }
```

```text
#!cjam
"Shebang: CJam | Math: " 1 2 + " - OK"
```

```text
#!clojure
(println (str "Shebang: Clojure " (clojure-version) " | Math: " (+ 28 2) " - OK"))
```

```text
#!cpp
#include <iostream>
int main() { std::cout << "Shebang: C++ | Math: " << 12 + 12 << " - OK" << std::endl; return 0; }
```

```text
#!crystal
puts "Shebang: Crystal #{Crystal::VERSION} | Math: #{19 + 1} - OK"
```

```text
#!elixir
IO.puts "Shebang: Elixir #{System.version} | Math: #{29 + 11} - OK"
```


```text
#!fortran
program test
  print *, "Shebang: Fortran | Math: ", 15 + 15, " - OK"
end program test
```

```text
#!freebasic
Print "Shebang: FreeBASIC " & __FB_VERSION__ & " | Math: " & (33 + 7) & " - OK"
```

```text
#!go
package main
import ("fmt"; "runtime")
func main() { fmt.Printf("Shebang: Go %s | Math: %d - OK\n", runtime.Version(), 14 + 6) }
```

```text
#!golfscript
"Shebang: GolfScript | Math: " 3 4 + " - OK"
```

```text
#!haskell
main = putStrLn $ "Shebang: Haskell | Math: " ++ show (26 + 4) ++ " - OK"
```


```text
#!java
public class Main {
    public static void main(String[] args) {
        System.out.println("Shebang: Java | Math: " + (18 + 18) + " - OK");
    }
}
```
```text
#!julia
println("Shebang: Julia $VERSION | Math: $(23 + 7) - OK")
```

```text
#!lisp
(format t "Shebang: Common Lisp | Math: ~d - OK~%" (+ 27 3))
```

```text
#!lolcode
HAI 1.2
VISIBLE "Shebang: Lolcode | Math: 14 - OK"
KTHXBYE
```

```text
#!lua
print("Shebang: Lua " .. _VERSION .. " | Math: " .. (8 + 8) .. " - OK")
```


```text
#!nim
echo "Shebang: Nim ", NimVersion, " | Math: ", 20 + 5, " - OK"
```

```text
#!node
console.log(`Shebang: Node ${process.version} | Math: ${5 + 8} - OK`);
```

```text
#!ocaml
Printf.printf "Shebang: OCaml | Math: %d - OK\n" (30 + 10);;
```

```text
#!octave
printf("Shebang: Octave %s | Math: %d - OK\n", version(), 24 + 6);
```

```text
#!perl
printf "Shebang: Perl v%vd | Math: %d - OK\n", $^V, 9 + 5;
```

```text
#!php
<?php echo "Shebang: PHP " . phpversion() . " | Math: " . (10 + 20) . " - OK"; ?>
```

```text
#!pwsh
Write-Output "Shebang: PowerShell | Math: $(36 + 4) - OK"
```

```text
#!prolog
:- initialization(main).
main :- Res is 31 + 9, write('Shebang: Prolog | Math: '), write(Res), write(' - OK'), nl, halt.
```

```text
#!python
import sys
print(f"Shebang: Python {sys.version.split()[0]} | Math: {12 + 30} - OK")
```

```text
#!r
cat(sprintf("Shebang: R %s | Math: %d - OK\n", R.version.string, 22 + 8))
```

```text
#!ruby
puts "Shebang: Ruby #{RUBY_VERSION} | Math: #{6 + 7} - OK"
```

```text
#!rust
fn main() { println!("Shebang: Rust | Math: {} - OK", 13 + 4); }
```

```text
#!science
import sys
print(f"Shebang: Science (Anaconda) | Math: {21 + 9} - OK")
```


```text
#!verilog
module test;
  initial begin
    $display("Shebang: Verilog | Math: %d - OK", 25 + 5);
    $finish;
  end
endmodule
```

