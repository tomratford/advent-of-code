/*
Advent of code; Year YYYY, Day XX

# Some notes on the challenge or solution here

usage:

	go run main.go path/to/input.txt
*/
package main

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: go run main.go path/to/input.txt")
		return
	}

	input, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println(err)
		return
	}

	p, err := Parse(string(input))
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(Part1(p))

	fmt.Println(Part2(p))

	fmt.Println(lcm(lcm(lcm(3767, 3779), 3889), 4057))
}

// Structs and types

type Intensity int

const (
	LOW Intensity = iota
	HIGH
)

func (i Intensity) String() string {
	if i == LOW {
		return "low"
	} else {
		return "high"
	}
}

type Pulse struct {
	Sender     string
	Intensity  Intensity
	Recipients []string
}

func (p Pulse) String() string {
	return fmt.Sprintf("%v -%v-> %v", p.Sender, p.Intensity, p.Recipients)
}

type State struct {
	Instructions []Pulse
	ptr          int

	Modules map[string]Module
}

func (s *State) String() string {
	var b strings.Builder
	for _, i := range s.Instructions {
		b.WriteString(fmt.Sprintln(i))
	}
	return b.String()
}

func NewState(modules map[string]Module) State {
	return State{
		[]Pulse{
			{
				"button",
				LOW,
				[]string{"broadcaster"},
			},
		},
		0,
		modules,
	}
}

func (s *State) NextState() bool {
	p := s.Instructions[s.ptr]
	s.ptr++

	for _, r := range p.Recipients {
		if m, ok := s.Modules[r]; ok {
			new_pulse := m.Recieve(p)
			if new_pulse.Sender != "" {
				s.Instructions = append(s.Instructions, new_pulse)
			}
		}
	}

	return s.ptr < len(s.Instructions)
}

type Module interface {
	Name() string
	To() []string
	Recieve(pulse Pulse) Pulse
}

type Broadcaster struct {
	Recipients []string
}

func (b Broadcaster) String() string { return fmt.Sprintf("broadcaster -> %v", b.Recipients) }

func (b Broadcaster) To() []string { return b.Recipients }

func (b Broadcaster) Recieve(pulse Pulse) Pulse {
	return Pulse{"broadcaster", pulse.Intensity, b.Recipients}
}

func (b Broadcaster) Name() string { return "broadcaster" }

type FlipFlop struct {
	name       string
	On         bool
	Recipients []string
}

func (f *FlipFlop) String() string { return fmt.Sprintf("%%%s -> %v", f.Name, f.Recipients) }

func (f *FlipFlop) To() []string { return f.Recipients }

func (f *FlipFlop) Name() string { return f.name }

func (f *FlipFlop) Recieve(pulse Pulse) Pulse {
	if pulse.Intensity == LOW {
		f.On = !f.On
		if f.On { // Was Off
			return Pulse{
				f.Name(),
				HIGH,
				f.Recipients,
			}
		} else { // was On
			return Pulse{
				f.Name(),
				LOW,
				f.Recipients,
			}
		}
	}
	return Pulse{}
}

type Conjunction struct {
	name       string
	Memory     map[string]Intensity
	Recipients []string
}

func (c *Conjunction) String() string { return fmt.Sprintf("&%s -> %v", c.Name, c.Recipients) }

func (c *Conjunction) To() []string { return c.Recipients }

func (c *Conjunction) Name() string { return c.name }

func (c *Conjunction) Recieve(pulse Pulse) Pulse {
	if _, ok := c.Memory[pulse.Sender]; ok {
		c.Memory[pulse.Sender] = pulse.Intensity
	}

	for _, i := range c.Memory {
		if i == LOW {
			return Pulse{
				c.Name(),
				HIGH,
				c.Recipients,
			}
		}
	}

	return Pulse{
		c.Name(),
		LOW,
		c.Recipients,
	}
}

// Solution for Part 1 of the challenge
func Part1(input map[string]Module) int {
	lows := 0
	highs := 0
	state := NewState(input)
	for i := 0; i < 1000; i++ {
		for state.NextState() {
		}
		state.Instructions = append(state.Instructions, Pulse{
			"button",
			LOW,
			[]string{"broadcaster"},
		})
	}
	for _, i := range state.Instructions {
		if i.Intensity == LOW {
			lows += len(i.Recipients)
		} else {
			highs += len(i.Recipients)
		}
	}
	// fmt.Println(state.Instructions)
	// fmt.Println(lows-1, highs)
	return (lows - 1) * highs
}

// Solution for Part 2 of the challenge
func Part2(input map[string]Module) int {
	state := NewState(input)
	i := 1
	oldptr := state.ptr
	// rx is fed by rg which is fed by...
	var kd, zf, vg, gs int // all inverters, as is rx. So when a low pulse is sent to these figure out and get lcm
	for i < 20000 {
		//fmt.Printf("press %d\n", i)
		for state.NextState() {
		}
		for j := oldptr; j < len(state.Instructions); j++ {
			inst := state.Instructions[j]
			if inst.Intensity == LOW {
				switch {
				case slices.Contains(inst.Recipients, "kd"):
					fmt.Printf("kd #%d: %d, ", kd, i)
					kd++
				case slices.Contains(inst.Recipients, "zf"):
					fmt.Printf("zf #%d: %d, ", zf, i)
					zf++
				case slices.Contains(inst.Recipients, "vg"):
					fmt.Printf("vg #%d: %d\n", vg, i)
					vg++
				case slices.Contains(inst.Recipients, "gs"):
					fmt.Printf("gs #%d: %d, ", gs, i)
					gs++
				}
			}
		}
		i++
		oldptr = state.ptr
		state.Instructions = append(state.Instructions, Pulse{
			"button",
			LOW,
			[]string{"broadcaster"},
		})
	}
	/*
				this sucked, the cycle is mental

				id #3    - #2   - #1     loop info
				kd 10301 - 6534 - 2767 = 3767 loop starting at -1000
				zf 10337 - 6558 - 2779 = 3779 loop starting at -1000
				gs 10667 - 6778 - 2889 = 3889 loop starting at -1000
				vg 11171 - 7114 - 3057 = 4057 loop starting at -1000

		Loop starts at the same place so the # of button presses is correct?
	*/
	return i
}

// from the internet
func lcm(a, b int64) int64 {
	return int64(math.Abs(float64(a*b)) / float64(gcd(a, b)))
}

func gcd(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a

}

// Function to parse the input string (with newlines) into output of choice
func Parse(input string) (map[string]Module, error) {

	Conjunctions := make(map[string]*Conjunction)

	rtn := make(map[string]Module)

	lines := strings.Split(input, "\n")
	for _, l := range lines {
		if len(l) == 0 {
			break
		}

		words := strings.Split(strings.ReplaceAll(l[1:], ",", ""), " ")
		name := words[0]
		recips := words[2:]

		switch l[0] {
		case 'b':
			// Broadcaster
			rtn["broadcaster"] = Broadcaster{recips}
		case '%':
			// Flip flop
			rtn[name] = &FlipFlop{
				name,
				false,
				recips,
			}
		case '&':
			// Conjunction
			c := &Conjunction{
				name,
				make(map[string]Intensity),
				recips,
			}
			rtn[name] = c
			Conjunctions[name] = c
		default:
			return rtn, fmt.Errorf("unreachable case %q", l)
		}
	}

	for _, r := range rtn {
		for _, m := range r.To() {
			if _, ok := Conjunctions[m]; ok {
				Conjunctions[m].Memory[r.Name()] = LOW
			}
		}
	}

	return rtn, nil
}
