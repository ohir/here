package here

import (
	"crypto/sha256"
	"fmt"
	"os"
	"strings"
	"testing"
)

// goldenhash
const sumOK = "4325312faa9a6334d480c34a9cd06e08e5360a7b914df0da78b6235b0a2c212d"

var logsink strings.Builder

type tBI struct {
	by  byte
	i8  int8
	st  string
	stm string
	stb string
	bys []byte
	bym []byte
	byb []byte
	i32 int32
	u16 uint16
	u64 uint64
	upt uintptr
}

func TestDump(t *testing.T) {
	v := tBI{
		by: 15, bys: []byte("short"), i32: 0x2022, upt: 0xdeadbeef,
		st: "not that long", stm: midstr, stb: longstr,
		bym: []byte(midstr), byb: []byte(longstr),
	}
	var bu strings.Builder
	D, p, _ := Verbose(true, &bu)
	p("\nTesting Dump:\n")
	D("-- Here-comment", v.by, v.i8, v.st, v.bys, v.bym, v.byb, v.i32, v.u16, v.u64, v.upt, v)
	D("1Test", v.by, v.bym, v.byb, v.u64)
	D("2Test", "for", []byte("short"))
	D("3Test", []byte("for long"), []byte(longstr))
	D(longstr, "0123456789012345678")
	D(371)
	m, n := 11, 33
	D(m > n, m, n, m < n)
	exp := 3365 // just size for now
	if bu.Len() != exp {
		t.Logf("Expected %d characters in buffer, but got %d instead!", exp, bu.Len())
		t.Fail()
	}
	logsink.WriteString(bu.String())
}

func TestDumpCorner(t *testing.T) {
	var bu strings.Builder
	D, p, pif := Verbose(false, &bu)
	p("\nTesting/using corner cases of Dump\n")
	D()
	pif(true, "!!ha")
	D, p, pif = Verbose(true, &bu)
	p("\nTesting/using corner cases of Dump\n")
	D()
	D(true)
	D(true, true)
	pif(true, "!!ha")
	Dump()
	Printf("")
	If(true, "!!ha")
	exp := 83 // 1|true
	if bu.Len() != exp {
		t.Logf("Expected %d characters in buffer, but got %d instead!", exp, bu.Len())
		t.Fail()
	}
	logsink.WriteString(bu.String())
}

func TestBitRuler(t *testing.T) {
	var bu strings.Builder
	_, p, _ := Verbose(true, &bu)
	p("-- Testing bitruler --")
	ita := [...]uint64{
		0,
		0xa,
		0xab,
		0xabc,
		0xabcd,
		0xabcde,
		0xabcdef,
		0x6e20666f72200e0d,
		0xffffffffffffffff,
		0x1000000000000000,
	}
	for _, hdr := range ita {
		bu.WriteString(b64ruler(true, hdr, "\n"))
	}
	p("\n---\n")
	bu.WriteString(b64ruler(false, 0xabacdefa, "\n"))
	exp := 786
	if bu.Len() != exp {
		t.Logf("Expected %d characters in buffer, but got %d instead!", exp, bu.Len())
		t.Fail()
	}
	logsink.WriteString(bu.String())
}

func TestCoverage(t *testing.T) {
	var bu strings.Builder
	bu.WriteString(b64ruler(false, 1<<55, ""))
	bu.WriteString(byteRuler(5))
	_ = asu64(uint(1))
	_ = asu64(uint32(1))
	_ = asu64(true)
	logsink.WriteString(bu.String())
}

func TestXBit(t *testing.T) {
	var bu strings.Builder
	_, p, _ := Verbose(true, &bu)
	p("\nTest XBit (bitpeek):\n")
	var hdr uint64 = 0x7841AAbeefFDd37E
	p("%s\n\n", XBit(`'Show ALL'  ________________________________
'❶ Labels:' 'SYN=.ACK<.ERR>.EXT=  with  0 1 1 1  bits
'❷ Labels:' 'SYN=.ACK<.ERR>.EXT=  with  1 0 0 0  bits
          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
' ‾‾‾‾‾‾‾‾ label chain: SYN=.ACK<.ERR>.EXT='
'  Char C:' C
' Label ?:' 'BitIs: ?
' Ascii A:' A
' Decimal:' D08@	('D08@')
'   Hex H:' 0xHH 	('0xHH')
' Octal  :' 0EFF 	('0EFF')
'  C32s G:' GG   	('GG')
' Three F:' F
'   Duo E:' E
'   Bit B:' B
'  Quoted:' '偩 =<\'>?ABCDEFGH\t_Tab\t_Tab\n NewLine: \\backslash ԹՖ'
' Escapes:' 偩 \=\<\'\>\?\A\B\C\D\E\F\G\H\t_Tab\t_Tab\n NewLine: \\backslash ԹՖ
`, hdr))
	p("-- corner cases, errs, partial errs\n")
	rt := func(desc, pic string, u uint64) {
		p("\n%s (%#x):\n     pic: %s\n  result: %s\n", desc, u, pic, XBit(pic, u))
	}
	rt("-- Invalid format", "!@66", hdr)
	rt("-- Print nothing format", "!33@", hdr)
	rt("-- Skip all format", "!63@", hdr)
	rt("-- Partial invalid format", "  !00@  !02@  ACHHHH 'should give PICERR message'", uint64(0x12cb))
	rt("-- Zero string format", "", hdr)
	rt("-- Other letter-commands", `\A:A \B:B \C:C \D:D02@ \E:E \F:F \G:G \H:H \I:I32@`, hdr)
	rt("-- Flag digit; at b29..b32", "'SYN:? ACK:? RDY:? ERR:? !29@", uint64(6<<29))
	rt("-- Flag lc/UC; at b14..b17", "'SYN= ACK= RDY= ERR= !14@", uint64(6<<14))
	rt("-- Flag show if 1; at b49..b52", "'SYN> ACK> RDY> ERR> !49@", uint64(6<<49))
	rt("-- Packet example", `(SYN= ACK= ERR= EXT= OVL= RTX= "GG") 'From: 'I32@:D16@`, hdr)

	exp := 1788 // we can't dissect buffer - rely on size and hash
	if bu.Len() != exp {
		t.Logf("Expected %d characters in buffer, but got %d instead!", exp, bu.Len())
		t.Fail()
	}
	logsink.WriteString(bu.String())
}

func TestPifStub(t *testing.T) {
	var bu strings.Builder
	_, p, pif := Verbose(true, &bu)
	_, _, paf := Verbose(false)
	part := func(s string) {
		p("\nPif test: %s\n", s)
		logsink.WriteString(bu.String())
		bu.Reset()
	}
	part("testing If vs stubIf")
	for i, v := range piftt {
		s := v[1:]
		if pif(true, s) != paf(true, s) {
			t.Logf("If and stubIf return differs at %d-true:>%s< \tpif:%t paf:%t",
				i, s, pif(true, s), paf(true, s))
			t.Fail()
		}
		if pif(false, s) != paf(false, s) {
			t.Logf("If and stubIf return differs at %d-false:>%s< \tpif:%t paf:%t",
				i, s, pif(false, s), paf(false, s))
			t.Fail()
		}
	}
	part("pif-paf comparison end")
}

func TestPifShallow(t *testing.T) {
	var bu strings.Builder
	_, p, pif := Verbose(true, &bu)
	part := func(s string) {
		p("\n^^^Above test Pif: %s\n", s)
		logsink.WriteString(bu.String())
		bu.Reset()
	}
	part("-- Testing If --")
	exp := "Got a > b\n"
	a, b := 3, 4
	if pif(a > b, exp) {
		t.Logf("Pif returned true whether it should return false!")
		t.Fail()
	}
	if bu.String() != "" {
		t.Logf("Pif wrote %q to buf whether it should not!", bu.String())
		t.Fail()
		p("\n^^^ERR: written but it should not!\n")
	}
	exp = "Got a < b\n"
	part("?Tru: ")
	if !pif(a < b, exp) {
		t.Logf("Pif returned false whether it should return true!")
		t.Fail()
	}
	exp = "### a <= b\n"
	part("default behaviour")
	if !pif(a <= b, exp) {
		t.Logf("Pif returned false whether it should return true!")
		t.Fail()
	}
	exp = "!Got a > b\n"
	part("return true")
	pif(a > b, exp)
	if bu.String() != exp {
		t.Logf("Pif miswrote! Expected >%q<, got >%q<", exp, bu.String())
		t.Fail()
	}
	exp = ": a > b\n"
	part("return false")
	pif(a > b, exp)
	pif(a < b, exp)
	if bu.String() != "false"+exp+"true"+exp {
		t.Logf("Pif miswrote! Expected >%s%s<, got >%s<", exp, exp, bu.String())
		t.Fail()
	}
	part("write always")
	pif(a > b, "")
	pif(a < b, "")
	pif(a > b, "-off")
	pif(a < b, "-do not print")
	if bu.Len() != 0 {
		t.Logf("Pif Should not write with empty/- format!")
		t.Fail()
		p("\nERR: written with empty/- format!\n")
		part("Pif off test failed")
	}
	part("turn off")
	r := false
	r = pif(a > b, ":! prev:%t\n", r)
	r = pif(a < b, "[1] Is True so it prints prev:%t\n", r)
	r = pif(a < b, " [1] Is True so it prints (elided) prev:%t\n", r)
	r = pif(a > b, "[2] Is False so it does not print prev:%t\n", r)
	r = pif(a > b, " [2] Is False so it does not print(elided) prev:%t\n", r)
	r = pif(a > b, "![3] Is False but ! so it prints prev:%t\n", r)
	r = pif(a > b, " ![3] Is False but ! so it prints (elided) prev:%t\n", r)
	r = pif(a < b, "![4] Is True but ! so it does not print prev:%t\n", r)
	r = pif(a < b, ":[5] Here : so it prints always prev:%t\n", r)
	r = pif(a > b, ":[6] Here : so it prints always prev:%t\n", r)
	r = pif(a < b, " :[7] Here : so it prints always with pfx (elided) prev:%t\n", r)
	r = pif(a > b, " :[8] Here : so it prints always with pfx (elided) prev:%t\n", r)
	r = pif(a > b, ":! prev:%t\n", r)
	r = pif(a > b, ":+ prev:%t\n", r)
	r = pif(a > b, ":! prev:%t\n", r)
	r = pif(a > b, " :!(elided) prev:%t\n", r)
	r = pif(a > b, " :+(elided) prev:%t\n", r)
	r = pif(a > b, " :!(elided) prev:%t\n", r)
	r = pif(a > b, " !!(elided) prev:%t\n", r)
	r = pif(a > b, " !+(elided) prev:%t\n", r)
	r = pif(a > b, " ++(elided) prev:%t\n", r)
	r = pif(a > b, " +!(elided) prev:%t\n", r)
	p("Last pif result was:%t\n", r)
	part("prefix switches")
	pif(a < b, ":")  // should print 'true:' to sink
	pif(a < b, "+")  // should add + => 'true:+'  in sink
	pif(a > b, "!")  // should add ! => 'true:+!' in sink
	pif(a < b, " :") // should print next 'true:' => 'true:+!true:'
	pif(a < b, " +") // should add + => 'true:+true:+'  in sink
	pif(a > b, " !") // should add ! => 'true:+!true:+!' in sink
	pif(a < b, " ")  // should not print to sink
	part("print switches")
}

func TestStubIf(t *testing.T) {
	var bu strings.Builder
	_, p, _ := Verbose(true, &bu)
	_, _, paf := Verbose(false)
	p("\nTesting stubIf by table:\n")
	for i, v := range piftt {
		c, s := v[0], v[1:]
		rt, rf := paf(true, s), paf(false, s)
		switch {
		case c == '_' && rt && !rf: // should got copied
		case c == '0' && !rt && !rf: // should be false
		case c == '1' && rt && rf: // should be true
		default:
			t.Logf("If return differs for %d: paf(true,\"%s\"):%t | paf(false,\"%s\"):%t",
				i, v, rt, v, rf)
			t.Fail()
		}
	}
	p("(stubIf should not output) paf by table end\n")
	logsink.WriteString(bu.String())
}

func TestPif(t *testing.T) {
	var bu strings.Builder
	_, p, pif := Verbose(true, &bu)
	p("\nTesting If by table:\n")
	for i, v := range piftt {
		c, s := v[0], v[1:]
		rt, rf := pif(true, s), pif(false, s)
		switch {
		case c == '_' && rt && !rf: // should got copied
		case c == '0' && !rt && !rf: // should be false
		case c == '1' && rt && rf: // should be true
		default:
			t.Logf("If return differs for %d: pif(true,\"%s\"):%t | pif(false,\"%s\"):%t",
				i, v, rt, v, rf)
			t.Fail()
		}
	}
	p("\npif by table end\n")
	logsink.WriteString(bu.String())
}

func TestRegression(t *testing.T) {
	var skipck bool
	if outfn := os.Getenv("MKGOLD"); outfn != "" {
		if outfn == "NOHASH" {
			skipck = true
		} else if outfn == "T" {
			os.Stdout.WriteString(logsink.String())
		} else if err := os.WriteFile(outfn, []byte(logsink.String()), 0660); err != nil {
			t.Fatalf("Can not dump to file %s [%v]", outfn, err)
		} else {
			fmt.Fprintf(os.Stderr, "\"Golden\" output has been written to %s\n", outfn)
		}
	}
	if !skipck {
		out := fmt.Sprintf("%x", sha256.Sum256([]byte(logsink.String())))
		if out != sumOK {
			t.Logf("Regression! Expected hash: %s", sumOK)
			t.Logf("        Hash now computed: %s", out)
			t.Logf("Diff MKGOLD output before setting a new hash to:")
			t.Logf("const sumOK = \"%s\"", out)
			t.Fail()
		}
	}
}

var piftt = []string{
	//  0      1      2      3      4      5      6     7     8     9
	"1++", "1!+", "1:+", "0+!", "0!!", "0:!", "_+", "_!", "_:", "_-+!", //  0-9
	// 10      11     12      13      14      15      16     17     18     19
	"1 ++", "1 !+", "1 :+", "0 +!", "0 !!", "0 :!", "_ +", "_ !", "_ :", "_-!+", // 10-19
	//   20        21        22        23        24        25        26       27       28       29
	"1 ++Aa", "1 !+Ab", "1 :+Ac", "0 +!Ad", "0 !!Ae", "0 :!Af", "_ +Ag", "_ !Ah", "_ :Ai", "_-!!Aj", // 20-29
	//    30         31        32         33         34         35         36        37        38        39
	"1 ++ Ba", "1 !+ Bb", "1 :+ Bc", "0 +! Bd", "0 !! Be", "0 :! Bf", "_ + Bg", "_ ! Bh", "_ : Bi", "_-++ Bj", // 30-39
	//   40      41       42        43        44        45      46       47       48        49
	"_ +Ca", "_ !Cb", "_ :Cc", "_ + Cd", "_ ! Ce", "_ : Cf", "_;Cg", "_.!Ch", "_':Ci", "_-!Cj", // 40-49
	//   50      51       52        53        54        55
	"_.+Da", "_.!Db", "_.:Dc", "_.+ Dd", "_.! De", "_.: Df",
	//   56        57        58         59         60         61   62   63
	"_ .+Ea", "_ .!Eb", "_ .:Ec", "_ .+ Ed", "_ .! Ee", "_ .: Ef", "_ ", "_",
}

const (
	srtstr  = "str short"
	midstr  = "O mighty King I beseth Yours"
	longstr = "'Well hast thou done,' said the friar; 'but hast thou often been angered?'" // Decameron
)
