package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"sync"
	"time"
)

type triple struct {
	nexthop int
	cost    int
	changed bool
}

type pair struct {
	j    int
	cost int
}

type packet struct {
	l int
	p []*pair
}

type table struct {
	t []*triple
}

func createTriple(n int, c int, flag bool) *triple {

	new := triple{nexthop: n, cost: c, changed: flag}
	return &new
}

func createPair(j int, cost int) *pair {

	new := pair{j: j, cost: cost}
	return &new
}

func createPacket(l int, p []*pair) *packet {

	new := packet{l: l, p: p}
	return &new
}

func newMutex() *sync.Mutex {

	var m sync.Mutex
	return &m
}

func packetToString(p *packet) (string, string) {

	l := strconv.Itoa(p.l)

	pairs := ""

	for _, pr := range p.p {

		new_p := "(" + strconv.Itoa(pr.j) + ", " + strconv.Itoa(pr.cost) + ")"
		pairs += new_p
		pairs += " "
	}

	return l, pairs

}

func initialize(i int, neighbours []int, n int) *table {

	new := make([]*triple, n)

	for j := 0; j < n; j++ {

		if i == j {
			continue
		}

		if search(neighbours, j) {

			v := createTriple(j, 1, true)
			new[j] = v

		} else {

			cost := myAbs(i - j)

			if i < j {

				v := createTriple(i+1, cost, true)
				new[j] = v

			} else if j < i {

				v := createTriple(i-1, cost, true)
				new[j] = v
			}

		}
	}

	tab := table{t: new}
	return &tab

}

func search(slice []int, vertice int) bool {

	for _, item := range slice {

		if item == vertice {
			return true
		}
	}
	return false
}

func myAbs(v int) int {

	if v >= 0 {
		return v
	} else {
		return -v
	}
}

func merge(cs []chan *packet) <-chan *packet {
	out := make(chan *packet)
	var wg sync.WaitGroup
	wg.Add(len(cs))
	for _, c := range cs {
		go func(c <-chan *packet) {
			for v := range c {
				out <- v
			}
			wg.Done()
		}(c)
	}
	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}

func vertice(id int, routingTable *table, incoming []chan *packet, outcoming []chan *packet, m *sync.Mutex, bound int,
	messages chan<- string) {

	go func() { /* sender */

		for true {

			r := rand.Float64()
			time.Sleep(time.Duration(r * float64(bound) * float64(time.Second)))

			p := make([]*pair, 0)

			msg1 := ""

			m.Lock()
			for idx, tr := range routingTable.t {

				if idx == id {
					continue
				}

				if tr.changed {

					new := createPair(idx, tr.cost)
					p = append(p, new)
					tr.changed = false

					msg1 = msg1 + fmt.Sprintf(" %d", idx)

				} else {
					continue
				}

			}
			m.Unlock()

			if msg1 != "" {

				msg1 = fmt.Sprintf("Sender %d: na tych zmiennych zmieniono flagi na false -", id) + msg1
				messages <- msg1
			} else {
				msg1 = fmt.Sprintf("Sender %d : wszystkie zmienne mają flagi ustawione na false", id)
				messages <- msg1
			}

			if len(p) > 0 {

				packet := createPacket(id, p)

				_, t := packetToString(packet)

				msg := fmt.Sprintf("Sender %d : Wysyłam pakiet "+t, id)
				messages <- msg

				for _, ch := range outcoming {
					ch <- packet
				}
			}
		}

	}()

	go func() { /* receiver */

		for true {

			out := merge(incoming)

			for pack := range out {

				l := pack.l

				_, t := packetToString(pack)

				msg := fmt.Sprintf("Receiver %d - Otrzymałem pakiet: "+t+" od %d", id, l)
			
				before := fmt.Sprintf("Receiver %d - stan przed: " , id)
				after := fmt.Sprintf("Receiver %d - stan po: ", id)

				m.Lock()

				for idx,tr := range routingTable.t {

					if idx == id {
						continue
					}

					before = before + fmt.Sprintf("%d - (%d, %d, %t) ", idx, 
																			tr.nexthop,
																			tr.cost,
																		    tr.changed)

				}

				m.Unlock()

			
				for _, pr := range pack.p {

					new_cost := 1 + pr.cost
					j := pr.j

					if id == j {
						continue
					}
					
					m.Lock()
					if new_cost < routingTable.t[j].cost {

						routingTable.t[j].cost = new_cost
						routingTable.t[j].nexthop = l
						routingTable.t[j].changed = true
						
					}
					m.Unlock()
				}

				m.Lock()

				for idx,tr := range routingTable.t {

					if idx == id {
						continue
					}

					after = after + fmt.Sprintf("%d - (%d, %d, %t) ", idx, 
																		tr.nexthop,
																		tr.cost,
																		tr.changed)

				}

				m.Unlock()

				messages <- (msg + "\n" + before + "\n" + after)
			

			}

		}

	}()

	for true {

	}
}

func printServer(messages <-chan string) {

	for true {

		msg := <-messages
		fmt.Println(msg)
		fmt.Println(" - - - - - - - - - - - -")
	}
}

func main() {

	var n, d, v1, v2 int

	end := make(chan bool)
	msg := make(chan string)
	bound := 3

	fmt.Println("Wprowadz liczbę wierzchołków : ")
	fmt.Scanln(&n)
	fmt.Println("Wprowadz liczbę skrótów : ")
	fmt.Scanln(&d)

	fmt.Println("Generuje graf nieskierowany o zadanych parametrach...")

	myGraph := make([][]int, n)
	inEdges := make([][]chan *packet, n)
	outEdges := make([][]chan *packet, n)

	for i := 0; i < n-1; i++ {

		myGraph[i] = append(myGraph[i], i+1)

		e := make(chan *packet)
		outEdges[i] = append(outEdges[i], e)
		inEdges[i+1] = append(inEdges[i+1], e)

		myGraph[i+1] = append(myGraph[i+1], i)

		e = make(chan *packet)
		outEdges[i+1] = append(outEdges[i+1], e)
		inEdges[i] = append(inEdges[i], e)

	}

	for i := 0; i < d; i++ {

		for {

			v1 = rand.Intn(n)
			v2 = rand.Intn(n)

			found := search(myGraph[v1], v2)

			if !found && v1 != v2 {
				break
			}



		}
	
		myGraph[v1] = append(myGraph[v1], v2)

		e := make(chan *packet)
		outEdges[v1] = append(outEdges[v1], e)
		inEdges[v2] = append(inEdges[v2], e)


		myGraph[v2] = append(myGraph[v2], v1)

		e = make(chan *packet)
		outEdges[v2] = append(outEdges[v2], e)
		inEdges[v1] = append(inEdges[v1], e)


	}

	fmt.Println("Graf wygenerowany. Drukuje liste sąsiedztwa...")

	for i, list := range myGraph {
		fmt.Printf("Wierzchołek %d ", i)
		fmt.Printf("Sąsiedzi : ")
		fmt.Println(list)
	}

	fmt.Println("Rozpoczynam symulacje...")

	for i := 0; i < n; i++ {

		tab := initialize(i, myGraph[i], n)
		m := newMutex()
		go vertice(i, tab, inEdges[i], outEdges[i], m, bound, msg)

	}

	go printServer(msg)

	<-end

	fmt.Println("Symulacja zakończona...")

}
