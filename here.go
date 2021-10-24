// (c) 2021 Ohir Ripe. MIT license.

/* Package here provides three debug printer functions: Dump, Printf, and If.
When used direct these write to the os.Stderr. If guarded by the Verbose
function, they can write to any io.StringWriter provided, or be turned off
(substituted with a noop versions). It is possible to simultanously have
many sets of Here printers - each set writing to distinct destination.

Example usage:
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
  // Output:
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
*/

package here

import (
	"fmt"
	"io"
	"os"
	"strings"
)

const tWidth = 99 // nowadays terminals are wide
type bld = *strings.Builder

/* function Verbose(V bool) (Dump, Printf, If) returns functions mentioned.
If passed flag V is false, Verbose returns a no-op stubs of all three instead.
Optional io.StringWriter can be provided after the flag to set printers output:

	D, P, Pif := here.Verbose(true, os.Stdout) // override default Stderr.
*/
func Verbose(V bool, osw ...io.StringWriter) (func(...interface{}),
	func(string, ...interface{}),
	func(bool, string, ...interface{}) bool,
) {
	var out io.StringWriter = os.Stderr
	if len(osw) > 0 && osw[0] != nil {
		out = osw[0]
	}
	if V {
		return func(a ...interface{}) { // Dump
				out.WriteString(dump(a...).String())
			},
			func(Fmt string, a ...interface{}) { // Printf
				out.WriteString(fmt.Sprintf(Fmt, a...))
			},
			func(c bool, Fmt string, a ...interface{}) bool { // If
				o, r := pif(c, Fmt, a...)
				out.WriteString(o.String())
				return r
			}
	} else {
		return func(...interface{}) {}, func(string, ...interface{}) {}, stubIf
	}
}

// func P is a fmt.Fprintf writing to the os.Stderr. It is included in the
// package API to complement its version returned by the Verbose() guard.
func Printf(Fmt string, a ...interface{}) { fmt.Fprintf(os.Stderr, Fmt, a...) }

/* Function If by default prints if the c condition is true.
"If" behaviour can be fine tuned both on input and output (ie. when it
prints and what it returns) by short punctuation based commands at the
start of its Fmt string:

First optional character controls printing "how":
  "- text" output nothing, just return value of c (turn "If" off)
  "  text" (space) elide command punctuation from output.

Next optional character controls printing "when":
  "+ text" print if c is true  (default)
  "! text" print if c is false
  ": text" print always with added 'true'/'false' prefix, unless elided

If "when" is present, next optional character may control the returned bool:
  + return true  always ( ++ !+ :+ )
  ! return false always ( !! +! :! )
otherwise If returns boolean value it got.

Example:
  //  for x < 7 && y > x && !tmout {
  for here.If(x < 7 && y > x && !tmout,
    "loop broke with x:%d, y:%d, tmo:%t\n", x, y, tmout){
	//...
  }
*/
func If(c bool, fm string, a ...interface{}) bool {
	o, r := pif(c, fm, a...)
	os.Stderr.WriteString(o.String())
	return r
}

func pif(c bool, fm string, a ...interface{}) (bld, bool) {
	from, pos, posMax := 0, 0, len(fm)-1
	var o strings.Builder
	if posMax < 0 || fm[pos] == '-' { // off
		return &o, c
	}
	p := func(Fmt string) {
		o.WriteString(fmt.Sprintf(Fmt, a...))
	}
	// ours := func(x byte) bool { return x == '!' || x == '+' || x == ':' }
	ctl := fm[0]
	if ctl == ' ' {
		fm = fm[1:]
		posMax--
		if posMax < 0 { // single space
			if c {
				p("true")
			} else {
				p("false")
			}
			return &o, c
		}
		ctl = fm[0]
		// do not elide for shorts: " :", " !", " +"
		for i, x := 0, ctl; i < 6 && i < posMax; x = fm[i] {
			if !(x == '!' || x == ':' || x == '+') {
				from = i
				break
			}
			i++
		}
	} //  "_.!tx" BUG 47 - should be default, returns mandated after dot
	if ctl == ':' {
		switch {
		case from != 0:
			p(fm[from:])
		case c:
			p("true" + fm)
		default:
			p("false" + fm)
		}
	} else if c && ctl != '!' { // default print
		p(fm[from:])
	} else if !c && ctl == '!' {
		p(fm[from:])
	}
	if !(ctl == '!' || ctl == ':' || ctl == '+') {
		return &o, c
	}
	pos++
	if pos <= posMax {
		switch fm[pos] {
		case '!':
			return &o, false
		case '+':
			return &o, true
		}
	}
	return &o, c
}

// stubIf needs to do a bit, semantics may not depend on the verbose flag
func stubIf(c bool, fm string, _ ...interface{}) bool {
	posMax := len(fm) - 1
	if posMax < 1 || fm[0] == '-' {
		return c
	} else if fm[0] == ' ' {
		fm = fm[1:]
		posMax--
	}
	if posMax < 1 || !(fm[0] == '!' || fm[0] == ':' || fm[0] == '+') {
		return c
	} else if fm[1] == '+' {
		return true
	} else if fm[1] == '!' {
		return false
	}
	return c
}

/*
Function Dump (aka describe) acts as improved Printf("%#v",...).

It prints, possibly under given condition, its input parameters in most
convenient for a human form. (For things that got implemented, for others
fmt.Printf family "%#v" format is used).

Builtin signed integers are emited in details as decimal, hex, and binary.
Builtin types of []byte and unsigned integers are printed in details, and
underlined by a byte/bit position ruler.  Values of other types are printed
using Printf family "%#v" format - but with each value or struct member
given in a separate line - with strings, tables and slices possibly
truncated - to not clobber the output with unusable garbage. For after having
a glimpsee these can be further inspected - passed as individual fields.
Dump describes input values one by one, and numbers them from [1] to [n].

Conditional print:
  If argument list begins with a bool value, and this value is false, D simply
  returns. (If you really want to see just a single bool, prepend it with a
  literal 'true').

Block title:
  Similarily, first (or first after the condition) string argument may provide
  a title to the block of output - if it begins with two dashes and a space:
  "-- here-comment". Otherwise anonymous "-- Here >>>" marker is used.

*/
func Dump(a ...interface{}) { os.Stderr.WriteString(dump(a...).String()) }

func dump(ia ...interface{}) bld {
	const (
		hereHead  = "-- Here! >>>\n\n"
		hereHeadS = "-- Here! %s >>>\n\n"
		hereTail  = "-- <<< END of Here! --\n"
		hereTailS = "-- <<< END of %s --\n"
	)
	var o strings.Builder
	p := func(fm string, a ...interface{}) { o.WriteString(fmt.Sprintf(fm, a...)) }
	m := hereTail
	if len(ia) == 0 {
		return &o
	}
	if b, ok := ia[0].(bool); ok {
		if !b {
			return &o
		} else {
			ia = ia[1:]
		}
	}
	if len(ia) == 0 {
		return &o
	}
	if s, ok := ia[0].(string); ok && len(s) > 2 &&
		s[0] == '-' && s[1] == '-' && s[2] == ' ' {
		m = fmt.Sprintf(hereTailS, s[3:])
		p(hereHeadS, s[3:])
		ia = ia[1:]
	} else {
		p("\n" + hereHead)
	}
	for i, iv := range ia {
		i++ // print 1 based
		switch v := iv.(type) {
		case bool:
			p("%2d|%t", i, v)
		case uint, uint32, uint64, uintptr, uint16, uint8:
			p("%2d|%T(%+[2]d %#04[2]x)%s", // |bN H|",
				i, v, b64ruler(true, asu64(v), "\n   "))
		case int, int64, int16, int8:
			p("%2d|%T(%+[2]d %#04[2]x %#08[2]b)", i, v)
		case int32: // rune
			p("%2d|%T(%+[2]d %#04[2]x %#[2]U %#08[2]b)", i, v)
		case []byte:
			hd := fmt.Sprintf("%2d|b%d(%q)", i, len(v), v)
			xs := xplit32(v, "\n   :  ")
			p("%s\nhex:  %s\npos: %s", hd, xs, byteRuler(len(xs)))
		case string:
			p("%2d|s%d(%q)", i, len(v), v)
		default:
			chop(&o, fmt.Sprintf("%2d|%#v", i, v))
		}
		o.WriteString("\n\n")
	}
	o.WriteString(m)
	return &o
}

// byteRuler returns string __0__1__2__3 ...up to _30_31 (at tWidth==99)
func byteRuler(n int) string {
	if n < 6 {
		return "__0"
	} else if n > 31*3 {
		n = 31 * 3 // restrict to 0..31 (0..31, n+3)
	}
	n = (n / 3) * 3 // be prudent
	buf := make([]byte, n+3)
	for i, d := 0, byte('_'); i <= n; i += 3 {
		if i > 0 && (i/3)%10 == 0 {
			d = byte(i/30) + '0'
		}
		buf[i], buf[i+1], buf[i+2] = '_', d, byte((i/3)%10)+'0'
	}
	return string(buf)
}

// Func b64ruler returns bit representation of an uint64 - laid out
// as nibbles - as much as needed to show all 1s. If first argument
// is true, func returns also ruler with bit position and hex digit
// to place under each nibble. Each (bits, ruler) part is prepended
// by the pfx string, if non empty. Usually pfx := "\n", or "\n\t".
//    1010,1011,1100,1101,1110,1111  ,nibble
//    23 A|19 B|15 C|11 D|7  E|3  F  |bN Hex|
func b64ruler(ruler bool, v uint64, pfx string) string {
	const blen = (64 / 4) * 5
	acu, hex, w := v, v, blen
	bits := [blen]byte{}
	rulr := [blen]byte{}
ruout:
	for i := 0; i < 64; i++ {
		wr := w % 5
		w--
		if wr != 1 {
			bits[w] = byte(acu&1) + '0'
			acu >>= 1
		}
		switch {
		case wr == 0:
			if hv := byte(hex & 15); hv < 10 {
				rulr[w] = hv + '0'
			} else {
				rulr[w] = hv + '7'
			}
			hex >>= 4
		case wr == 4:
			rulr[w] = ' '
		case wr == 3 && i < 10:
			rulr[w] = ' '
		case wr == 3 && i > 9:
			rulr[w] = byte(i%10) + '1' // ones digit
		case wr == 2 && i < 10:
			rulr[w] = byte(i)&15 + '0' // single digit
		case wr == 2 && i > 9:
			rulr[w] = byte(i/10) + '0' // tenths digit
		case wr == 1:
			if i > 7 && acu == 0 {
				w++
				break ruout
			}
			bits[w] = ','
			rulr[w] = '|'
			i--
		}
	}
	if ruler {
		return pfx + string(bits[w:]) + pfx + string(rulr[w:])
	} else {
		return string(bits[w:])
	}
}

// xplit32 takes slice of bytes and returns "% X" formatted into multiline
// string, 32 bytes printed in each line; starting with pfx, if given.
func xplit32(v []byte, pfx string) string {
	xs := []byte(fmt.Sprintf("% X", v))
	if len(xs) > tWidth { // max ruler
		for k, i := 1, 0; i < len(xs)-1; i++ {
			if xs[i] == ' ' {
				if k%32 == 0 {
					xs[i] = '\n'
				}
				k++
			}
		}
		if len(pfx) != 0 { // slow but short
			return strings.ReplaceAll(string(xs), "\n", pfx)
		}
	}
	return string(xs)
}

// chop %#v output to lines. [80/20].
func chop(oi *strings.Builder, ov string) {
	var comsp, brace, lastw int
	const div = "\n~~\n"
	var t strings.Builder
	t.Grow(1 << 12)
	for i := 1; i < len(ov); i++ {
		switch ov[i] {
		case ':':
			if comsp != 0 {
				t.WriteString(ov[lastw:comsp])
				t.WriteString(div)
				t.WriteString(ov[comsp:i])
				lastw = i
			} else if brace != 0 {
				t.WriteString(ov[lastw:brace])
				t.WriteString(div + " ")
				t.WriteString(ov[brace:i])
				lastw = i
			}
			brace, comsp = 0, 0
		case '{':
			brace, comsp = i+1, 0 // "{\n"
		case ' ':
			if ov[i-1] == ',' {
				comsp, brace = i, 0 // ",\n "
			} else {
				comsp, brace = 0, 0
			}
		}
	}
	t.WriteString(ov[lastw:])
	t.WriteString(div)
	// now truncate long items
	const lineTrunc = " ...[Truncated]\n"
	lnt := strings.Split(t.String(), "\n")
	for i, so := range lnt {
		if len(so) > tWidth-len(lineTrunc) {
			oi.WriteString(so[:tWidth-len(lineTrunc)])
			oi.WriteString(lineTrunc)
		} else if i < len(lnt)-2 {
			oi.WriteString(so)
			oi.WriteByte('\n')
		} else {
			oi.WriteString(so)
		}
	}
}

func asu64(iv interface{}) uint64 {
	switch v := iv.(type) {
	case uint:
		return uint64(v)
	case uint32:
		return uint64(v)
	case uint64:
		return uint64(v)
	case uintptr:
		return uint64(v)
	case uint16:
		return uint64(v)
	case uint8:
		return uint64(v)
	}
	return 0xdeadbeefcafebaca
}

/* Func XBit returns human readable form of bits and bit fields of an uint value
- as prescribed by its pic (format) string. XBit is included here for
author's convenience and for a few others to reuse. Here version
allocates its output string and is guarded against misuse.

Bits of input are consumed from the lowest to the highest (ie. are
shifted right), and bitpeek output is produced using single-character
commands mixed with descriptions given as quoted text or bit labels.
So pic (format) string is a Little Endian picture, with Label of b63
being at the beginning, and b0 at the end: `'Bit11:?Bit10:?..Bit0:?`.

   get N bits | output:
   > - 1 bit  | LABEL - show if bit is SET (1).    Otherwise skips.
   < - 1 bit  | LABEL - show if bit is UNSET (0).  Otherwise skips.
   = - 1 bit  | LABEL lowercased if bit is UNSET. Uppercase in fmt.
   ? - 1 bit  | digit 0/1 in place of '?' after LABEL. Eg."Flag:0".
   B - 1 bit  | digit 0  1     : B for bit,  use ? for labeled bit.
   E - 2 bits | digit 0..3     : decimal, short for D02@
   F - 3 bits | digit 0..7     : decimal, short for D03@
   H - 4 bits | hex digit 0..F : hexadecimal
   G - 5 bits | C32s character :               (CRC  spelling code)
   A - 7 bits | 7b char/ascii; : A for Ascii   (emits ~ for A < 32)
   C - 8 bits | 8b char/utf8;  : C for Char    (emits ~ for C < 32)
   I  32 bits | IPv4 address   : as I32@       (emits dotted quads)
   D  nn bits | decimal number : as Dnn@
   !  nn bits |   skip nn bits : as !nn@
     nn@ bits | bits to use for !, I, and D - nn in range 01@..64@.

   Text, labels, and escapes:

   unquoted text : Command-chars are interpreted, unless escaped
                   with a backslash \ character.

   'quoted text' : is not interpreted except for \t and \n escapes.

   'Label=       : Label is a special form of quoted text, where four
   'Label?		   characters = < ? > are interpreted as 'bit to read'
   'Label>		   placeholders and last such placeholder in a chain
   'Label<		   replaces the closing quote character. Each Label
				   placeholder consumes a single bit from input.
                   Eg. 'TX= ACK< ERR:? are three labels in a chain.
                   Label may contain escaped \= \< \' \> \? chars.

    multi\n line : Bitpeek can produce multiline output either with
                   literal newline (and tab) characters and with
				   customary \n \t escapes in its format string.

        \ Escape : Next character is not interpreted except \n for
                   NL, \t for TAB and \' for ' - in the quoted text.

Example pic strings:

*/
func XBit(pic string, from uint64) (rb string) {
	// Note: this is an old C ported - newer CPUs branch predictors do not like
	// this kind of code. But it works, it has mere 170 Go lines, and I (ohir)
	// have many picstrings for various headers made. So, after a light touch,
	// bitpeek has a new place here, as XBit.
	pi := len(pic) // pic index
	oi := pi + 22  // account for up to two 'I32@I32@'
	ot := make([]byte, oi)
	var asis, c byte // flow control, temp c
	defer func() {   // users will find a way to make invalid fmt string
		e := recover()
		if e != nil {
			rb = "Error: Invalid BPfmt format string!"
		}
	}()
ploop:
	for pi > 0 {
		pi--
		w := pic[pi]
		switch { // labels and escapes
		case pi > 0 && pic[pi-1] == '\\':
			switch w {
			case 'n':
				w = '\n'
			case 't':
				w = '\t'
			}
			oi--
			ot[oi] = w
			pi-- // skip leading \
			continue
		case asis == 0: // goto control
		case w == '\'':
			asis = 0
			continue
		case asis == 1: // '' emit quoted
			oi--
			ot[oi] = w
			continue
		case w|3 == 63: // next label ahead
			asis = 0
		case asis == 2: // lowercase label
			if w > 63 && w < 91 {
				w |= 0x20
			}
			fallthrough
		case asis == 3: // emit label.
			oi--
			ot[oi] = w
			continue
		case asis == 4: // skip label
			continue
		}
		switch w { // command
		case '\'': // 1: quoted
			asis = 1
			continue
		case '?': // labeled bit
			c = 48 + byte(from)&1
			asis = 3
			from >>= 1
		case 'B': // Bit
			c = 48 + byte(from)&1
			from >>= 1
		case 'H': // Hex, usually seen in flock
			for {
				c = byte(from) & 15
				if c < 10 {
					c += 0x30
				} else {
					c += 0x37
				}
				from >>= 4
				if pi > 0 && pic[pi-1] == 'H' {
					pi--
					oi--
					ot[oi] = c
					continue
				}
				break
			}
		case 'C': // Character 8bit
			c = byte(from)
			if c < 32 {
				c = '~' // make printable
			}
			from >>= 8
		case 'A': // Ascii 7bit
			c = byte(from) & 0x7f
			if c < 32 {
				c = '~'
			}
			from >>= 7
		case '=': // lowercase if UNSET (0)
			asis = 2 + byte(from)&1
			from >>= 1
		case '>': // emit label if SET (1)
			asis = 4 - byte(from)&1
			from >>= 1
		case '<': // emit label if UNSET (0)
			asis = 3 + byte(from)&1
			from >>= 1
		case 'F': // 3b decimal
			c = 48 + byte(from)&7
			from >>= 3
		case 'G': // teletype crc
			c = byte(from) & 0x1f
			if c < 26 {
				c += 97 // 65 for C32S
			} else {
				c += 24
			}
			from >>= 5
		case 'E': // 2b decimal
			c = 48 + byte(from)&3
			from >>= 2
		case '@': // bitcount 01..64
			k := (10 * uint8(pic[pi-2]-48)) + uint8(pic[pi-1]-48)
			switch {
			case k == 0, k > 64:
				fallthrough
				//lint:ignore ST1015 Go spec allows this!
			default:
				e := `PICERR!`
				for i := 6; oi > 0 && i >= 0; i-- {
					oi--
					ot[oi] = e[i]
				}
				break ploop
			case pi > 2 && pic[pi-3] == '!':
				pi -= 3
				from >>= k
			case pi > 2 && pic[pi-3] == 'D':
				pi -= 3
				v := from &^ (^uint64(0) << k)
				from >>= k
				for v > 9 {
					k := v / 10
					oi--
					ot[oi] = byte(48 + v - k*10)
					v = k
				}
				oi--
				ot[oi] = byte(48 + v)
			case pi > 2 && pic[pi-3] == 'I':
				pi -= 3
				ia := from &^ (^uint64(0) << k)
				from >>= k
				for i := 0; i < 4; i++ {
					v := byte(ia)
					ia >>= 8
					for v > 9 {
						k := v / 10
						oi--
						ot[oi] = byte(48 + v - k*10)
						v = k
					}
					oi--
					ot[oi] = byte(48 + v)
					if i < 3 {
						oi--
						ot[oi] = '.'
					}
				}
			}
		default:
			c = w
		}
		if c != 0 {
			oi--
			ot[oi] = c
			c = 0
		}
	}
	return string(ot[oi:])
}
