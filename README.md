# here
`import "github.com/ohir/here"`

Package here provides three debug printer functions: Dump, Printf, and Pif.

When used direct, those functions write to the os.Stderr; if guarded by the Verbose function they can write to any io.StringWriter provided or be turned off (substituted with a noop versions).
It is possible to simultanously have many sets of Here printers - each set writing to distinct destination.

``` go
// package API:
func Dump(a ...interface{})                  // a Printf("%#v...", ...) improved

func Printf(Fmt string, a ...interface{})    // a Printf, to complete the set

func Pif(c bool, Fmt string, a ...interface{}) bool // conditional Printf

func Verbose(V bool, osw ...io.StringWriter) (
    func(...interface{}),                    // Dump
    func(string, ...interface{}),            // Printf
    func(bool, string, ...interface{}) bool, // Pif
)
```
Function `Verbose(V bool) (Dump, Printf, Pif)`  returns package functions guarded by the boolean flag. If passed flag is false, no-op stubs of all functions are returned instead of printing ones.  Optional `io.StringWriter` can be provided after the flag to set printers output:
``` go
	D, P, Pif := here.Verbose(true, &builder)  // print to strings.Builder.
	// or
	D, P, Pif := here.Verbose(true, os.Stdout) // override default Stderr.
```

### Code:
func [Dump](/here.go?s=7308:7335#L249), func [Printf](/here.go?s=3726:3767#L126), func [Pif](/here.go?s=4762:4812#L155), func [Verbose](/here.go?s=2468:2610#L82)
- Docs: [![Go Reference](https://pkg.go.dev/badge/github.com/ohir/here.svg)](https://pkg.go.dev/github.com/ohir/here)
- Coverage: [100%](/here_test.go)

### Example usage:
``` go
  package main

  import "os"
  import "github.com/ohir/here" // println with nicer output
  import "github.com/ohir/mopt" // commandline flags parsing

  func main() {
    here.Printf(
      "Functions print to os.Stderr always, unless guarded by Verbose:\n")

    _, prn, _ := here.Verbose(true, os.Stdout) // override default Stderr
    prn("prn now prints to Stdout\n")

    D, pe, pif := here.Verbose(mopt.Usage("-D debug output").OptB('D'))

    msg := "will print to Stderr"
    pe("D, pe, pif %s with commandline -D flag\n", msg)

    x, y := 0, 0
    //  for x < 7 && y <= 2*x { // comment out, copy, add pif:
    for pif(x < 7 && y <= 2*x, " !loop break: x:%d, y:%d\n", x, y) {
      y = x/2 + 3
      x++
    }
    for i := 1; i < 4; i++ { // Dump can print conditionally, too
      D(i == 2, "-- peek into", i, []byte(msg), '😁', uint('😁'), pe)
    }
  }
  // Output: // -D
  // Functions print to os.Stderr always, unless guarded by Verbose:
  // prn now prints to Stdout
  // D, pe, pif will print to Stderr with commandline -D flag
  // loop break: x:1, y:3
  // -- Here! peek into >>>
  //
  //  1|int(+2 0x0002 0b00000010)
  //
  //  2|b20("will print to Stderr")
  // hex:  77 69 6C 6C 20 70 72 69 6E 74 20 74 6F 20 53 74 64 65 72 72
  // pos: __0__1__2__3__4__5__6__7__8__9_10_11_12_13_14_15_16_17_18_19
  //
  //  3|int32(+128513 0x1f601 U+1F601 '😁' 0b11111011000000001)
  //
  //  4|uint(+128513 0x1f601)
  //    0001,1111,0110,0000,0001
  //    19 1|15 F|11 6|7  0|3  1
  //
  //  5|(func(string, ...interface {}))(0xbacaffe)
  // ~~
  //
  // -- <<< END of peek into --
```
